namespace ScriptingTools

open InterlockingModel
open XMLExtraction.LayoutExtraction
open UMC
open Utils
open System.IO

[<AutoOpen>]
module SimpleTypes =
    type SimpleTrackSegment = LLinear of name : string
                            | LPointFork of name : string * PointPosition
                            | LPointStem of name : string
    let (<+>) (el1 : SimpleTrackSegment) (el2 : SimpleTrackSegment) = el1, el2
    type SimpleLayout = (SimpleTrackSegment * SimpleTrackSegment) list

    type SimpleRouteElement =
        | RLinear of name : string * length : int
        | RPoint of name : string * position : PointPosition
    type SimpleRoute = SimpleRouteElement list

    type SimpleTrain =
        { id : string
        ; length : int
        ; route : SimpleRoute
        ; route_direction : RouteDirection }
    type SimpleTrains = SimpleTrain list

    type LayoutType = CustomLayout of SimpleLayout
                    | XMLLayout of path : string

    type SimpleModelArgs =
        { trains : SimpleTrain list
        ; layout : LayoutType
        ; show_stats : bool
        ; output_file : string option }

    type Stats =
        { num_of_trains : int
        ; train_lengths : int list
        ; route_lengths : int list
        ; total_route_sub_segments : int
        ; total_linears : int
        ; total_points : int
        ; shared_points : int
        ; shared_linears : int}

    type SimpleRouteElement with
        static member Length (element : SimpleRouteElement) =
            match element with
            | RLinear(_,len) -> len
            | _              -> 1

/// Functions for converting script model representation to
/// to a validated internal representation
[<AutoOpen>]
module ScriptTools =
    let private toLayoutSegment : SimpleTrackSegment -> LayoutSegment =
        function
        | LPointFork(n, Plus) -> PointForkLayoutSegment (n, Plus)
        | LPointFork(n, Minus) -> PointForkLayoutSegment (n, Minus)
        | LPointStem n -> PointStemLayoutSegment n
        | LLinear n -> LinearLayoutSegment n

    type SimpleRouteElement with
        static member toRouteSegment : SimpleRouteElement -> RouteSegment =
            function
            | RLinear(n,len) -> LinearRouteSegment (LinearId n, len)
            | RPoint(n, Plus) -> PointRouteSegment (PointId n, Plus)
            | RPoint(n, Minus) -> PointRouteSegment (PointId n, Minus)

    let private getRailwayLayoutFromCustom (custom_layout : SimpleLayout) : RailwayNetworkLayout =
        custom_layout
        |> List.map (fun (el1,el2) -> toLayoutSegment el1, toLayoutSegment el2)
        |> Map.ofList

    let private getRoute (train : SimpleTrain) : Route =
        let route_elements =
            train.route
            |> List.map SimpleRouteElement.toRouteSegment
        Route (route_elements, train.route_direction)

    let private getTrains (trains : SimpleTrain list) : Trains =
        let toTrain : SimpleTrain -> Train = fun t ->
            { id = TrainId t.id; length = t.length; route = getRoute t }
        trains |> List.map toTrain

    /// Extract linears from layout
    let private getLinears (layout : RailwayNetworkLayout) : Linears =
        Map.toList layout
        |> List.unzip
        ||> List.append
        |> List.choose
            (function
             | LinearLayoutSegment n -> Some({id = LinearId n; train = None})
             | _ -> None)
        |> Set.ofList
        |> Set.toList

    /// Extract points from layout
    let private getPoints (layout : RailwayNetworkLayout) : Points =
        Map.toList layout
        |> List.unzip
        ||> List.append
        |> List.choose
            (function
             | PointForkLayoutSegment(n, _)
             | PointStemLayoutSegment n -> Some({id = PointId n; position = Plus})
             | _ -> None)
        |> Set.ofList
        |> Set.toList

    let private getObjects (trains : SimpleTrains) (layout : RailwayNetworkLayout)
                           : ModelObjects =
        let train_ids = trains |> List.map (fun t -> TrainId t.id)
        let trains_map : Map<TrainId,Train> =
            List.zip train_ids (getTrains trains)
            |> Map.ofList

        let linears = getLinears layout
        let linear_ids = linears |> List.map Linear.id
        let linears_map : Map<LinearId,Linear> =
            List.zip linear_ids linears
            |> Map.ofList

        let points = getPoints layout
        let point_ids = points |> List.map Point.id
        let points_map : Map<PointId,Point> =
            List.zip point_ids points
            |> Map.ofList
        { trains = trains_map
          linears = linears_map
          points = points_map }

    /// Pretty print the raw layout from an XML file
    let printRawLayout (path : string) =
        let layoutSegmentToSimple segment =
            match segment with
            | LinearLayoutSegment id -> LLinear id
            | PointForkLayoutSegment(id, pos) -> LPointFork(id, pos)
            | PointStemLayoutSegment id -> LPointStem id
            |> sprintf "%A"

        let layout = resultFlow {
            let! layout = createLayoutFromXML path
            let output_layout =
                layout
                |> Map.toList
                |> List.map (fun (x,y) ->
                             let x_simple = layoutSegmentToSimple x
                             let y_simple = layoutSegmentToSimple y
                             sprintf "%s <+> %s" (x_simple.PadRight 24) y_simple)
            return output_layout |> String.concat "\n" }
        match layout with
        | Ok layout_string -> printfn "%s" layout_string
        | Error msg -> printfn "ERROR: %s" msg

    let private generateStats (trains : SimpleTrains) (layout : RailwayNetworkLayout) : Stats =
        let num_of_trains = trains |> List.length
        let train_lengths = trains |> List.map (fun {length=len} -> len)
        let route_lengths = trains |> List.map (fun {route=r} -> List.length r)
        let total_route_sub_segments =
            trains
            |> Seq.map
                (fun {route=r} ->
                 Seq.map (SimpleRouteElement.Length) r |> Seq.sum)
            |> Seq.sum
        let total_linears = getLinears layout |> List.length
        let total_points = getPoints layout |> List.length

        let getPointNames = function RPoint(n,_) -> Some n | _ -> None
                            |> Seq.choose
        let shared_points =
            uniqueProducts trains
            |> Seq.map (fun ({route=r1}, {route=r2}) ->
                        let r1_points = r1 |> getPointNames |> Set.ofSeq
                        let r2_points = r2 |> getPointNames |> Set.ofSeq
                        Set.intersect r1_points r2_points)
            |> Seq.map Set.count
            |> Seq.sum
        let getLinearNames = function RLinear(n,_) -> Some n | _ -> None
                             |> Seq.choose
        let shared_linears =
            uniqueProducts trains
            |> Seq.map (fun ({route=r1}, {route=r2}) ->
                        let r1_linears = r1 |> getLinearNames |> Set.ofSeq
                        let r2_linears = r2 |> getLinearNames |> Set.ofSeq
                        Set.intersect r1_linears r2_linears)
            |> Seq.map Set.count
            |> Seq.sum
        { num_of_trains = num_of_trains
        ; train_lengths = train_lengths
        ; route_lengths = route_lengths
        ; total_route_sub_segments = total_route_sub_segments
        ; total_linears = total_linears
        ; total_points = total_points
        ; shared_points = shared_points
        ; shared_linears = shared_linears }


    let generateUMCModel (model_args : SimpleModelArgs) : unit =
        let validateAndGenerate =
            UMCModelConstruction.composeModel
            |> validateAndGenerateModel
        let output = resultFlow {
            let! layout =
                match model_args.layout with
                | CustomLayout custom_layout ->
                    Ok(getRailwayLayoutFromCustom custom_layout)
                | XMLLayout path -> createLayoutFromXML path
            let trains = model_args.trains
            let objects = getObjects trains layout
            let! model = validateAndGenerate layout objects
            let stats = generateStats trains layout
            let output =
                if model_args.show_stats then
                    sprintf "STATS:\n%A\n\nMODEL:\n%s" stats model
                else sprintf "MODEL:\n%s" model
            return output }
        match output, model_args.output_file with
        | Ok output, None -> printfn "%s" output
        | Ok output, Some file_path ->
            File.WriteAllText(file_path, output)
            printfn "model written to file %s" file_path
        | Error msg, _ -> printfn "ERROR:\n%s" msg
