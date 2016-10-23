namespace InterlockingModel

open Utils

[<AutoOpen>]
module TypeDefinitions =
    type TrainId = TrainId of string
    type TrainIds = TrainId list

    type LinearId = LinearId of string

    type PointId = PointId of string

    type PointPosition = Plus | Minus

    type RouteSegment = LinearRouteSegment of LinearId * length : int
                      | PointRouteSegment of PointId * required_position : PointPosition
    type RouteSegments = RouteSegment list

    type RouteDirection = Up | Down

    type Route = Route of RouteSegments * RouteDirection
    type Routes = Route list

    type Train = { id : TrainId
                   route : Route
                   length : int }
    type Trains = Train list

    type Linear = { id : LinearId
                    train : Train option }
    type Linears = Linear list

    type Point = { id : PointId
                   position : PointPosition }
    type Points = Point list

    /// Elements for composing a railway network layout
    type LayoutSegment = LinearLayoutSegment of id : string
                       | PointStemLayoutSegment of id : string
                       | PointForkLayoutSegment of id : string * position : PointPosition

    type RailwayNetworkLayout = Map<LayoutSegment, LayoutSegment>

    /// A collection of objects to be instantiated in the end model
    type ModelObjects = { trains  : Map<TrainId, Train>
                          linears : Map<LinearId, Linear>
                          points  : Map<PointId, Point> }
    type ValidatedModelObjects = Validated of ModelObjects
    type ModelGeneratorFunction = ValidatedModelObjects -> string


    (* Extending the types with field access functions *)
    type TrainId with
        static member value : TrainId -> string = fun (TrainId v) -> v

    type LinearId with
        static member value : LinearId -> string = fun (LinearId v) -> v

    type PointId with
        static member value : PointId -> string = fun (PointId v) -> v

    type RouteSegment with
        static member length : RouteSegment -> int = function
            | LinearRouteSegment(_,len) -> len
            // All points has been simplified to have a length of one
            | PointRouteSegment _ -> 1

    type Route with
        static member segments : Route -> RouteSegments =
            fun (Route (segments,_)) -> segments
        static member direction : Route -> RouteDirection =
            fun (Route (_,dir)) -> dir

    type ModelObjects with
        member this.trainList : Trains =
            this.trains |> Map.toList |> List.map snd
        member this.pointList : Points =
            this.points |> Map.toList |> List.map snd
        member this.linearList : Linears =
            this.linears |> Map.toList |> List.map snd

    (* Creating static access functions for record fields,
        with the functions having the same name as its field *)

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Train =
        let id : Train -> TrainId = fun t -> t.id
        let route : Train -> Route = fun t -> t.route
        let length : Train -> int = fun t -> t.length

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Linear =
        let id : Linear -> LinearId = fun tc -> tc.id
        let train : Linear -> Train option = fun tc -> tc.train

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Point =
        let id : Point -> PointId = fun p -> p.id
        let position : Point -> PointPosition = fun p -> p.position

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module LayoutElement =
        let id : LayoutSegment -> string = function
            | LinearLayoutSegment id -> id
            | PointStemLayoutSegment id -> id
            | PointForkLayoutSegment (id,_) -> id

    /// Helper type used in the validation process.
    /// The type is used where routes or route segment lengths need validation
    /// and can represent a simple success or an error case with a description
    type SuccessResult = Result<unit,string>
    /// Infix operator for combining simple unit results
    let (&&&) (a : SuccessResult) (b : SuccessResult) : SuccessResult =
        match a with
        | Ok () -> b
        | Error _ -> a

/// Functions for constructing routes
[<AutoOpen>]
module RouteConstruction =
    /// Stitch two route fragments together to one route
    let stitchRoutePair (route1 : Route) (route2 : Route) : Result<Route, string> =
        let stitch : RouteSegments -> RouteSegments -> RouteDirection -> Route =
            fun route1_elements route2_elements direction ->
            let stitched = List.concat [route1_elements; List.tail route2_elements]
            Route (stitched, direction)

        let must_have_same_direction : Route -> Route -> string =
            sprintf """
            route1 and route2 must have same direction
            route1: %A
            route2: %A
            """
        let (|Diff_directions|_|) (r1 : Route, r2 : Route) =
            let (Route(_, dir1)) = r1
            let (Route(_, dir2)) = r2
            if dir1 <> dir2
            then Some(must_have_same_direction r1 r2)
            else None

        let r1_must_end_where_r2_starts : Route -> Route -> string =
            sprintf "route1 must end where route2 starts\nroute1: %A\nroute2: %A"
        let (|NoCommonStichPoint|_|) (r1 : Route, r2 : Route) =
            let (Route(r1_elements, _)) = r1
            let (Route(r2_elements, _)) = r2
            match r1_elements, r2_elements with
            | r1_elements, (r2_first::_)
              when r1_elements <> []
              && List.last r1_elements = r2_first -> None
            | _ -> Some (r1_must_end_where_r2_starts r1 r2)

        match route1, route2 with
        | Diff_directions(error_msg) -> Error error_msg
        | NoCommonStichPoint(error_msg) -> Error error_msg
        | Route (route1_elements,dir), Route (route2_elements,_) ->
              Ok (stitch route1_elements route2_elements dir)

    let stitchRoutes (routes : Routes) : Result<Route, string> =
        routes
        |> Seq.map Ok
        |> Result<_,_>.reduce stitchRoutePair

/// Functions for checking if routes are valid together in a layout
module RouteValidation =
    /// Checks that a route is has a linear as start and end
    let private routeHasLinearStartAndEnd (route : Route) : SuccessResult =
        let route_segments = Route.segments route
        match Seq.head route_segments, Seq.last route_segments with
        | LinearRouteSegment _, LinearRouteSegment _ -> Ok ()
        | _ -> route_segments
               |> sprintf "route is must have a linear as start and end\n%A"
               |> Error

    /// Checks that a given route is legal in a given layout
    let private routeIsValidInLayout (layout : RailwayNetworkLayout) (route : Route)
                                     : SuccessResult =
        // helper function for looking up an element in the layout
        let tryFind = fun from_element ->
            Map.tryFind from_element layout

        // helper function that evaluates if two segments are connected
        let areConnected : LayoutSegment -> LayoutSegment -> bool =
            fun from_element to_element ->
            match tryFind from_element with
            | Some(to_element') when to_element' = to_element -> true
            | _ -> false

        let linearToLinear : string -> string -> bool = fun from_id to_id ->
            let from_linear, to_linear = LinearLayoutSegment from_id, LinearLayoutSegment to_id
            areConnected from_linear to_linear

        let linearToPoint : string -> string -> PointPosition -> bool =
            fun from_id to_id pos ->
            let from_linear = LinearLayoutSegment from_id
            let to_stem = PointStemLayoutSegment to_id
            let to_fork = PointForkLayoutSegment (to_id, pos)
            areConnected from_linear to_stem || areConnected from_linear to_fork

        let pointToLinear : string -> PointPosition -> string -> bool =
            fun from_id pos to_id ->
            let from_stem = PointStemLayoutSegment from_id
            let from_fork = PointForkLayoutSegment(from_id, pos)
            let to_linear = LinearLayoutSegment to_id
            // Since the Route-elements doesn't specify stem or fork
            // -we have to try both to see if they exists in the layout
            areConnected from_stem to_linear || areConnected from_fork to_linear

        let pointToPoint : string -> PointPosition -> string -> PointPosition -> bool =
            fun from_id from_pos to_id to_pos ->
            let from_stem = PointStemLayoutSegment from_id
            let from_fork = PointForkLayoutSegment(from_id, from_pos)
            let to_stem = PointStemLayoutSegment to_id
            let to_fork = PointForkLayoutSegment(to_id, to_pos)
            areConnected from_stem to_stem
            || areConnected from_stem to_fork
            || areConnected from_fork to_stem
            || areConnected from_fork to_fork

        /// checks that a connection between two given elements exist in the current layout
        /// basically we look up in the layout map to see if there exist a mapping from
        /// a route element to the other given route element
        let connectionExistInLayout : (RouteSegment * RouteSegment) -> bool =
            function
            | LinearRouteSegment(LinearId from_id, _),
              LinearRouteSegment(LinearId to_id, _) ->
                  linearToLinear from_id to_id
            | LinearRouteSegment(LinearId from_id, _),
              PointRouteSegment(PointId to_id, pos) ->
                linearToPoint from_id to_id pos
            | PointRouteSegment(PointId from_id, pos),
              LinearRouteSegment(LinearId to_id, _) ->
                pointToLinear from_id pos to_id
            | PointRouteSegment(PointId from_id, from_pos),
              PointRouteSegment(PointId to_id, to_pos) ->
                  pointToPoint from_id from_pos to_id to_pos

        let verifySegmentPair : RouteSegment [] -> SuccessResult = fun segment_pair ->
            let segment1, segment2 = segment_pair.[0], segment_pair.[1]
            match connectionExistInLayout(segment1, segment2) with
            | true  -> Ok ()
            | false ->
                sprintf "no connection between tracks [ %A -> %A ]" segment1 segment2
                |> Error

        match Route.direction route with
        | Up -> Route.segments route
        | Down -> List.rev (Route.segments route)
        |> Seq.windowed 2
        |> Seq.map verifySegmentPair
        |> Seq.reduce (&&&)

    /// verifying that two given routes can be used together in a model without obvious deadlock
    let private noObviousConflict (Route (route1_segments, _)) (Route (route2_segments, _))
                                  : SuccessResult =
        let diffStart : RouteSegments -> RouteSegments -> SuccessResult =
            fun r1_segments r2_segments ->
            let diff_start = Seq.head r1_segments <> Seq.head r2_segments
            if diff_start then Ok ()
            else Error "The given routes start at the same place"

        let diffEnd : RouteSegments -> RouteSegments -> SuccessResult =
            fun r1_segments r2_segments ->
            let diff_end = Seq.last r1_segments <> Seq.last r2_segments
            if diff_end then Ok ()
            else Error "The given routes have the same destination"

        let validStartAndEnd : RouteSegments -> RouteSegments -> SuccessResult =
            fun r1_segments r2_segments ->
            let route1_start = Seq.head r1_segments
            let route2_start = Seq.head r2_segments
            let route1_end   = Seq.last r1_segments
            let route2_end   = Seq.last r2_segments
            let diff_start_end =
                not(route1_start = route2_end && route2_start = route1_end)
            if diff_start_end then Ok ()
            else Error "The given routes start and end in exact opposite locations"

        diffStart route1_segments route2_segments
        &&& diffEnd route1_segments route2_segments
        &&& validStartAndEnd route1_segments route2_segments

    let verifyRoutes (layout: RailwayNetworkLayout) (routes : Routes) : SuccessResult =
        let individual_routes_valid : SuccessResult seq =
            routes
            |> Seq.map (fun route ->
                        routeHasLinearStartAndEnd route
                        &&& routeIsValidInLayout layout route)

        let routes_are_valid_together : SuccessResult seq =
            uniqueProducts routes
            |> Seq.map (fun (r1,r2) -> noObviousConflict r1 r2)

        [ individual_routes_valid
        ; routes_are_valid_together ]
        |> Seq.concat
        |> Seq.reduce (&&&)

/// Validation functions for verifying that the route segments
/// obey the constraints defined by the lengths of the trains
module LengthConstraints =
    type Intersection =
        { train1 : Train
          train2 : Train
          train1_track_length : int
          train2_track_length : int
          track_id : LinearId }

    /// Checks that a route segment is shorter or equal to the train that holds the route
    let private routeSegmentIsShorterOrEqual (train : Train) : RouteSegment -> SuccessResult =
        function
        | LinearRouteSegment(segment_id,length)
            when length > train.length ->
                let err_msg =
                    sprintf "%A in the route of train %A must be equal to or smaller than %i"
                err_msg segment_id train.id train.length
                |> Error
        | _ -> Ok ()

    /// Verify that lengths of intersecting track segments
    /// are obeying the following rule:
    ///
    /// if t2.length <= t1.track_length
    /// then t2.track_length == t2.length
    /// else t2.track_length == t1.track_length
    ///
    /// where t1.length >= t2.length
    let private shortestConstrainedByLongest (intersection : Intersection) : SuccessResult =
        // dividing the trains of the intersection into longest and shortest trains
        // together with the length of the intersecting track representation in their route.
        // train1 is the longest train and train2 is the shortest train
        let [ t1, t1_track_length
            ; t2, t2_track_length ] =
              [ intersection.train1, intersection.train1_track_length
              ; intersection.train2, intersection.train2_track_length ]
              |> List.sortByDescending (fst >> Train.length)

        let track_id = intersection.track_id

        // When the shortest train is shorter than the length of
        // the track representation in the longest train,
        // then the shortest trains track representation must be
        // exactly the length of the shortest train.
        //
        // Eg. the shortest train length is 2 and the longest train length is 4
        // furthermore is the longest train track representation 3,
        // and therefore the shortest trains track representation must be 2
        if t2.length <= t1_track_length then
            if t2_track_length = t2.length then Ok ()
            else
                let err_msg = sprintf """
                    the intersecting track %A, between train %A and train %A
                    must have a length of %i in the route of train %A
                    """
                Error(err_msg track_id t1.id t2.id t2.length t2.id)
        else// shortest_train.length > longest_train_track_length
            // for example the length of shortest train is 2
            // and the length of the longest train track representation is 1,
            // then the length of the shortest trains track representation
            // must also be 1
            if t2_track_length = t1_track_length then Ok ()
            else
                let err_msg = sprintf """
                    intersecting track %A between train %A and train %A
                    must have a length of %i in the route of train %A
                    """
                Error(err_msg track_id t1.id t2.id t1.length t2.id)

    // Collect all intersections between two train routes
    let private getIntersections (t1 : Train, t2 : Train) : Intersection seq =
        let getLinearSegment : RouteSegment -> (LinearId * int) option =
            function
            | LinearRouteSegment(id,length) -> Some(id,length)
            | _ -> None

        let getLinearSegments : Route -> (LinearId * int) seq =
            fun (Route(segments,_)) ->
            segments |> Seq.choose getLinearSegment

        let t1_linear_lengths : Map<LinearId, int> =
            getLinearSegments t1.route |> Map.ofSeq

        getLinearSegments t2.route
        |> Seq.choose (
            fun (track_id, t2_track_len) ->
            Map.tryFind track_id t1_linear_lengths
            |> Option.map (fun t1_track_len ->
                          { train1 = t1
                            train2 = t2
                            train1_track_length = t1_track_len
                            train2_track_length = t2_track_len
                            track_id = track_id }))

    let checkLengthContraints (trains : Trains) : SuccessResult =
        let all_individual_routes_are_valid : SuccessResult seq =
            trains
            |> Seq.collect (fun train ->
                            let (Route(route_segments,_)) = train.route
                            route_segments
                            |> Seq.map (routeSegmentIsShorterOrEqual train))
        let all_route_intersections_are_valid : SuccessResult seq =
            uniqueProducts trains
            |> Seq.collect getIntersections
            |> Seq.map shortestConstrainedByLongest

        [ all_individual_routes_are_valid
        ; all_route_intersections_are_valid ]
        |> Seq.concat
        |> Seq.reduce (&&&)

[<AutoOpen>]
module ModelGeneration =
    /// Interface describing the properties to be generated for the model
    type ModelCheckingPropertyDefinitions =
        abstract NoCollision : string
        abstract AllTrainsArrived : string
        abstract NoDerailment : string
        abstract TrainsDetectedOnPoints : string
        abstract AllMessagesHandled : string

    /// Validates that all trains have a route and that all the routes are valid in the layout
    let private validateTrainRoutes (layout : RailwayNetworkLayout) (objects : ModelObjects)
                                    : Result<ValidatedModelObjects, string> =
        /// gets a route if the train have one
        let getRoute : Train -> Result<Route, string>  = fun train ->
            match train.route with
            // there exist a route with at least one element
            | Route(head::tail, direction) as route -> Ok (route)
            | _ -> Error (sprintf "no route for train %A" train.id)

        let routes_valid : SuccessResult =
            objects.trainList
            |> Result<_,_>.traverse getRoute
            >>= (RouteValidation.verifyRoutes layout)

        match routes_valid with
        | Ok ()     -> Ok(Validated objects)
        | Error msg -> Error msg

    let checkLengthContraints (valid_objects : ValidatedModelObjects)
                              : Result<ValidatedModelObjects, string> =
        let (Validated objects) = valid_objects
        let length_constraints_ok =
            objects.trainList
            |> LengthConstraints.checkLengthContraints
        match length_constraints_ok with
        | Ok ()     -> Ok(Validated objects)
        | Error msg -> Error msg

    // Updates all the linears, which are first on a route, to reflect presence of a train
    let updateTrainLocations (Validated objects) : Result<ValidatedModelObjects, string> =
        let updateTrainLocation (train : Train) (objects : ModelObjects) = resultFlow {
            let! linear_id =
                match train.route with
                | Route(LinearRouteSegment(linear_id,_)::_,_) -> Ok linear_id
                | _ -> Error (sprintf """
                              possibly malformed route for train %A
                              perhaps validation is missing?
                              """ train)
            let! linear =
                match Map.tryFind linear_id objects.linears with
                | Some linear -> Ok linear
                | None -> Error (sprintf "linear %A doesnt exist" linear_id)
            let updated_linear = { linear with train = Some train }
            let updated_linears = objects.linears |> Map.add linear_id updated_linear
            return { objects with linears = updated_linears } }
        objects.trainList
        |> Result<_,_>.fold updateTrainLocation (Ok objects)
        |> Result<_,_>.map Validated

    let generateRawModel : ModelGeneratorFunction = fun (Validated objects) ->
        sprintf "%A" objects

    /// Validates the routes,
    /// updates the train locations in the network based on their first route segment,
    /// and generates the final model instantiation using the generateModel function
    let validateAndGenerateModel (modelGenFun : ModelGeneratorFunction)
        : RailwayNetworkLayout -> ModelObjects -> Result<string, string>  =
        fun layout objects ->
        validateTrainRoutes layout objects
        >>= checkLengthContraints
        >>= updateTrainLocations
        >>= (modelGenFun >> Ok)

