namespace XMLExtraction

open FSharp.Data
open Utils
open InterlockingModel

/// Required types and extensions of existing types
[<AutoOpen>]
module TypeDefinitions =
    [<Literal>]
    let sample_xml_file = "sample.xml"
    // Loading the type provider with a sample file
    type RailwayXML = XmlProvider<sample_xml_file>

    (* Modules and functions for more convenient access of
       values extracted by the type provider *)

    module XMLRoute =
        let Id (route : RailwayXML.Route) : string = route.Id

    module TrackSection =
        let Id (track_section : RailwayXML.TrackSection) : string = track_section.Id
        let Type (track_section : RailwayXML.TrackSection) : string = track_section.Type

    module Neighbor =
        let Id (neighbor : RailwayXML.Neighbor) : string = neighbor.Ref
        let Side (neighbor : RailwayXML.Neighbor) : string = neighbor.Side

    module MarkerBoard =
        let Id (markerboard : RailwayXML.Markerboard) : string = markerboard.Id
        let Track (markerboard : RailwayXML.Markerboard) : string = markerboard.Track

    module Condition =
        let Type (condition : RailwayXML.Condition) : string = condition.Type
        let Ref (condition : RailwayXML.Condition) : string = condition.Ref

    type PointPosition with
        static member fromString : string -> PointPosition = function
            | "plus"  -> Plus
            | "minus" -> Minus

    type RouteDirection with
        static member fromString : string -> RouteDirection = function
            | "up"   -> Up
            | "down" -> Down

    type LayoutSegment with
        static member fromString (id : string) : string * string -> LayoutSegment =
            function
            | "linear", _      -> LinearLayoutSegment id
            | "point", "stem"  -> PointStemLayoutSegment id
            | "point", "plus"  -> PointForkLayoutSegment (id, Plus)
            | "point", "minus" -> PointForkLayoutSegment (id, Minus)

[<AutoOpen>]
module BasicObjectExtraction =
    type ModelObjectsExtractionParameters =
        { xml_file_path : string
          train_ids : TrainIds
          train_length : int
          routes : Routes }
    /// Extracts all trains, points and linears from xml file
    let extractBasicModelObjectsFromXML : ModelObjectsExtractionParameters -> ModelObjects =
        fun parameters ->
        let xml = RailwayXML.Load parameters.xml_file_path
        let linears : (LinearId * Linear) seq =
            xml.Interlocking.Network.TrackSections
            |> Seq.choose
                (fun track_section ->
                 match track_section.Type with
                 | "linear" ->
                     Some(LinearId track_section.Id,
                          { id = LinearId track_section.Id
                            train = None })
                 | _ -> None)
        let points : (PointId * Point) seq =
            xml.Interlocking.Network.TrackSections
            |> Seq.choose
                (fun track_section ->
                 match track_section.Type with
                 | "point" ->
                     Some(PointId track_section.Id,
                           { id = PointId track_section.Id
                             position = Plus})
                 | _ -> None )
        let trains =
            List.zip parameters.train_ids parameters.routes
            |> List.map (fun (id,route) ->
                         id,{ id = id; route = route; length = parameters.train_length })
        let basic_objects : ModelObjects =
            { trains  = trains |> Map.ofList
              points  = points |> Map.ofSeq
              linears = linears |> Map.ofSeq }
        basic_objects

[<AutoOpen>]
module LayoutExtraction =
    /// Retrieves all connection pairs from a given xml file.
    /// Assumes that the layout defined in the xml file is well-formed
    let createLayoutFromXML (path : string) : Result<RailwayNetworkLayout, string> =
        let xml = RailwayXML.Load path
        let elements = xml.Interlocking.Network.TrackSections
        /// getting the adjacent neighbor type
        /// if its a linear then its straightforward
        /// if its a point then we have to figure out if its the stem or one of the forks
        let getNeighborType : string -> string -> Result<LayoutSegment, string> =
            fun from_id to_id -> resultFlow {
            let! element =
                elements
                |> Seq.tryFind (TrackSection.Id >> (=)to_id)
                |> function
                    | None -> Error (sprintf "no tracksection with id %s" to_id)
                    | Some element -> Ok element
            let! from_neighbor =
                element.Neighbors
                |> Seq.tryFind (Neighbor.Id >> (=)from_id)
                |> function
                    | Some neighbor -> Ok neighbor
                    | None -> sprintf "could not find %s in neighbors of %s" to_id from_id
                              |> Error
            return LayoutSegment.fromString to_id (element.Type, from_neighbor.Side) }

        /// All connection pairs going from linear to other
        let linears : Result<LayoutSegment * LayoutSegment, string> seq  =
            let isLinearWithUpNeighbor : RailwayXML.TrackSection -> bool =
                fun element ->
                let element_is_linear = element.Type = "linear"
                let element_has_up_neighbor =
                    element.Neighbors |> Seq.exists (Neighbor.Side >> (=)"up")
                (element_is_linear && element_has_up_neighbor)

            let getFromIdAndToId : RailwayXML.TrackSection -> string * string =
                fun element ->
                let from_id = element.Id
                let neighbor = element.Neighbors |> Seq.find (Neighbor.Side >> (=)"up")
                let to_id = neighbor.Ref
                from_id, to_id

            let getLayoutElementPair
                : string * string -> Result<LayoutSegment * LayoutSegment, string> =
                fun (from_id, to_id) ->
                let from_element = LinearLayoutSegment from_id
                let to_element = getNeighborType from_id to_id
                Result<_,_>.map (fun to_el -> from_element, to_el) to_element

            elements
            |> Seq.filter isLinearWithUpNeighbor
            |> Seq.map (getFromIdAndToId >> getLayoutElementPair)

        let getLayoutElementPair
            : string -> RailwayXML.Neighbor -> Result<LayoutSegment * LayoutSegment, string> =
            fun from_id neighbor ->
            let neighbor_id = neighbor.Ref
            let from_element =
                LayoutSegment.fromString from_id ("point", neighbor.Side)
            getNeighborType from_id neighbor_id
            |> Result<_,_>.map (fun to_element -> from_element, to_element)

        /// All connection pairs going from point to other
        let points : Result<LayoutSegment * LayoutSegment, string> seq = seq {
            for element in elements |> Seq.filter (TrackSection.Type >> (=)"point") do
                let from_id = element.Id
                for neighbor in element.Neighbors do
                    yield getLayoutElementPair from_id neighbor }
        [linears; points]
        |> Seq.concat
        |> List.ofSeq
        |> Result<_,_>.sequence
        |> Result<_,_>.map Map.ofList

[<AutoOpen>]
module RouteExtraction =
    let extractRouteFragmentFromXML (path : string) (linear_length : int) (route_id : string)
                                    : Result<Route, string> = resultFlow {
        let xml = RailwayXML.Load path

        let route = xml.Interlocking.Routetable.Routes
                    |> Seq.tryFind (XMLRoute.Id >> (=)route_id)

        let! (route : RailwayXML.Route) =
            xml.Interlocking.Routetable.Routes
            |> Seq.tryFind (XMLRoute.Id >> (=)route_id)
            |> function
                | Some route -> Ok route
                | None -> Error (sprintf "no route with id %s" route_id)

        let markerboard_src_id : string = route.Source

        let! (linear_src_id : string) =
            xml.Interlocking.Network.Markerboards
            |> Seq.tryFind (MarkerBoard.Id >> (=)markerboard_src_id)
            |> function
                | Some markerboard -> Ok markerboard.Track
                | None -> Error (sprintf "no markerboard for id %s" markerboard_src_id)

        let points : (string * PointPosition) seq =
            route.Conditions
            |> Seq.filter (Condition.Type >> (=)"point")
            |> Seq.map (fun condition ->
                        let pos = Option.get condition.Val
                        let id = condition.Ref
                        let point_pos = PointPosition.fromString pos
                        (id, point_pos) )

        // if the id exists as a point id then the id is a point
        let tryGetPoint : string -> (string * PointPosition) option =
            fun id -> points |> Seq.tryFind (fst >> (=)id)

        let vacancies : RouteSegments =
            let routeElement : RailwayXML.Condition -> RouteSegment =
                fun condition ->
                let id = condition.Ref
                match tryGetPoint id with
                | Some (id,pos) -> PointRouteSegment(PointId id, pos)
                | None          -> LinearRouteSegment(LinearId id, linear_length)
            route.Conditions
            |> Seq.filter (Condition.Type >> (=)"trackvacancy")
            |> Seq.map routeElement
            |> List.ofSeq

        let! (direction : RouteDirection) =
            xml.Interlocking.Network.Markerboards
            |> Seq.tryFind (MarkerBoard.Id >> (=)markerboard_src_id)
            |> function
                | None -> Error (sprintf "no markerboard with id %s" markerboard_src_id)
                | Some markerboard ->
                    markerboard.Mounted
                    |> RouteDirection.fromString
                    |> Ok

        let first_element = LinearRouteSegment(LinearId linear_src_id, linear_length)
        let route_elements = first_element :: vacancies

        return Route (route_elements, direction) }

    let extractRouteFragmentsFromXML (path : string) (train_len : int) (route_ids : string list)
                                     : Result<Route, string> =
        let extractRouteFragment : string -> Result<Route, string> = fun route_id ->
            extractRouteFragmentFromXML path train_len route_id
        route_ids
        |> Seq.map extractRouteFragment
        |> Result<_,_>.reduce stitchRoutePair

[<AutoOpen>]
module ModelGenerationFromXML =
    type ModelGenerationParameters =
        { modelGeneratorFunction : ModelGeneratorFunction
          xml_file_path : string
          routes : string list list }
    let generateModelFromXML : ModelGenerationParameters -> Result<string,string> =
        fun parameters -> resultFlow {
        let default_train_length = 2
        let extractRouteFragments : string list -> Result<Route,string> =
            extractRouteFragmentsFromXML parameters.xml_file_path default_train_length
        let! (routes : Route list) =
            parameters.routes
            |> Result<_,_>.traverse extractRouteFragments

        let! (layout : RailwayNetworkLayout) = createLayoutFromXML parameters.xml_file_path
        let validateAndGenerate = validateAndGenerateModel parameters.modelGeneratorFunction

        let train_ids = [0 .. Seq.length routes - 1]
                        |> List.map (sprintf "%i" >> TrainId)

        let! model =
            {  xml_file_path = parameters.xml_file_path
               train_ids = train_ids
               train_length = default_train_length
               routes = routes }
            |> extractBasicModelObjectsFromXML
            |> validateAndGenerate layout

        return model }
