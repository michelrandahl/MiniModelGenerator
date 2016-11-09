namespace UMC

open InterlockingModel
open UMCTrain
open UMCPoint
open UMCLinear
open System
open Utils

[<AutoOpen>]
module UMCDefinitions =
    /// the UMC classes defining the behavior of the model
    let train_class = UMCTrain.class_definition
    let point_class = UMCPoint.class_definition
    let linear_class = UMCLinear.class_definition

    /// Model specific string representations
    /// used as object identifiers in the UMC model
    type TrainId with
        static member modelRepresentation train_id =
            TrainId.value train_id |> sprintf "train_%s"
    type LinearId with
        static member modelRepresentation linear_id =
            LinearId.value linear_id |> sprintf "linear_%s"
    type PointId with
        static member modelRepresentation point_id =
            PointId.value point_id |> sprintf "point_%s"
    type PointPosition with
        static member modelRepresentation : PointPosition -> string = function
            | Plus -> "True"
            | Minus -> "False"
    type RouteSegment with
        static member modelRepresentation : RouteSegment -> string = function
            | LinearRouteSegment(lin_id,_)  -> LinearId.modelRepresentation lin_id
            | PointRouteSegment(point_id,_) -> PointId.modelRepresentation point_id

/// Functions for generation of UMC abstractions
[<AutoOpen>]
module AbstractionDefinitions =
    type AbstractionType = Action | State
    type Abstraction<'a> =
        { name : 'a  -> string
          abstraction_type : AbstractionType
          predicate : 'a -> string }

    let getAbstractionName : 'a -> Abstraction<'a> -> string = fun args abstraction ->
        abstraction.name args

    let abstractionDefinition (abstraction : Abstraction<'a>) (args : 'a) : string =
        let predicate = abstraction.predicate args
        let name = abstraction.name args
        match abstraction.abstraction_type with
        | Action -> sprintf "Action: %s -> %s" predicate name
        | State  -> sprintf "State: %s -> %s" predicate name

    let withPointModelRep : Point -> (string -> string) -> string =
        fun {id=id} stringGenerator  ->
        PointId.modelRepresentation id
        |> stringGenerator

    let withTrainModelRep : Train -> (string -> string) -> string =
        fun {id=id} stringGenerator ->
        TrainId.modelRepresentation id
        |> stringGenerator

    let pointIn : PointPosition -> Abstraction<Point> = fun position ->
        let name = fun point ->
            match position with
            | Plus  -> withPointModelRep point (sprintf "%s_in_plus")
            | Minus -> withPointModelRep point (sprintf "%s_in_minus")
        let predicate = fun point ->
            match position with
            | Plus  -> withPointModelRep point (sprintf "%s.current_position = True")
            | Minus -> withPointModelRep point (sprintf "%s.current_position = False")
        { name = name
        ; abstraction_type = State
        ; predicate = predicate }

    let pointInPlus : Abstraction<Point> = pointIn Plus
    let pointInMinus : Abstraction<Point> = pointIn Minus

    let noTrainDetectedOnPoint : Abstraction<Point> =
        let name = fun point ->
            withPointModelRep point (sprintf "no_train_on_%s")
        let predicate = fun point ->
            withPointModelRep point (sprintf "%s.train = null")
        { name = name
        ; abstraction_type = State
        ; predicate = predicate }

    let all_pairs_of_trains_at_diff_positions : Abstraction<Trains> =
        let name = fun _ -> "trains_at_diff_positions"
        let predicate = fun trains ->
            let trainPairAtDiffPositions : (Train * Train) -> string =
                fun (train1,train2) ->
                let train1_id = TrainId.modelRepresentation train1.id
                let train2_id = TrainId.modelRepresentation train2.id

                let trainPartsAtDiffPositions : (int * int) -> string = fun (id1,id2) ->
                    sprintf "%s.occupies[%d] /= %s.occupies[%d]"
                            train1_id
                            id1
                            train2_id
                            id2

                crossProductOfLists [0..train1.length-1] [0..train2.length-1]
                |> Seq.map trainPartsAtDiffPositions
                |> String.concat " and \n"

            uniqueProducts trains
            |> Seq.map trainPairAtDiffPositions
            |> String.concat " and \n"
        { name = name
        ; abstraction_type = State
        ; predicate = predicate }

    let positioning : Abstraction<Point> =
        let name = fun point ->
            (sprintf "positioning_%s")
            |> withPointModelRep point
        let predicate = fun point ->
            (sprintf "inState(%s.POSITIONING)")
            |> withPointModelRep point
        { name = name
        ; abstraction_type = State
        ; predicate = predicate }

    let trainArrived : Abstraction<Train> =
        let name = fun train ->
            withTrainModelRep train (sprintf "%s_arrived")
        let predicate = fun train ->
            withTrainModelRep train (sprintf "inState(%s.ARRIVED)")
        { name = name
        ; abstraction_type = State
        ; predicate = predicate }

    let trainNotOnPoint : Abstraction<Train * Point> =
        let name : Train * Point -> string = fun (train, point) ->
            let train_model_rep = TrainId.modelRepresentation train.id
            let point_model_rep = PointId.modelRepresentation point.id
            sprintf "%s_not_on_%s" train_model_rep point_model_rep
        let predicate : Train * Point -> string = fun (train, point) ->
            let point_model_rep = PointId.modelRepresentation point.id
            let train_model_rep = TrainId.modelRepresentation train.id
            let trainPartNotOnPoint : int -> string = fun index ->
                sprintf "%s.occupies[%d] /= %s"
                        train_model_rep
                        index
                        point_model_rep
            [0 .. train.length - 1]
            |> Seq.map trainPartNotOnPoint
            |> String.concat " and\n"
        { name = name
        ; abstraction_type = State
        ; predicate = predicate }

    let discarded_msg : Abstraction<unit> =
        { name = fun _ -> "discarded_message"
        ; abstraction_type = Action
        ; predicate = fun _ -> "lostevent" }

    let pointMalfunction : Abstraction<Point> =
        let pointModelRep = fun p -> PointId.modelRepresentation p.id
        { name = fun p -> sprintf "%s_malfunction" (pointModelRep p)
        ; abstraction_type = State
        ; predicate = fun p -> sprintf "inState(%s.MALFUNCTION)" (pointModelRep p) }

    let all_abstraction_declarations (validated_objects : ValidatedModelObjects) : string =
        let (Validated objects) = validated_objects
        let trains : Trains = objects.trainList
        let points : Points = objects.pointList

        let no_trains_detected_on_points =
            points |> List.map (abstractionDefinition noTrainDetectedOnPoint)
        let points_positioning =
            points |> List.map (abstractionDefinition positioning)
        let points_in_plus =
            points |> List.map (abstractionDefinition pointInPlus)
        let points_in_minus =
            points |> List.map (abstractionDefinition pointInMinus)
        let trains_arrived =
            trains |> List.map (abstractionDefinition trainArrived)
        let trains_at_diff_positions =
            (all_pairs_of_trains_at_diff_positions, trains)
            ||> abstractionDefinition
        let trains_not_on_points =
            crossProductOfLists trains points
            |> List.ofSeq
            |> List.map (abstractionDefinition trainNotOnPoint)
        let discarded_message =
            abstractionDefinition discarded_msg ()
        let points_malfunction =
            points |> List.map (abstractionDefinition pointMalfunction)

        [ trains_arrived
        ; no_trains_detected_on_points
        ; points_positioning
        ; points_in_plus
        ; points_in_minus
        ; points_malfunction
        ; [ trains_at_diff_positions ]
        ; trains_not_on_points
        ; [ discarded_message ] ]
        |> List.concat
        |> String.concat "\n"

//// Extension functions for generating object instantiations in UMC
[<AutoOpen>]
module ModelObjectInstantiations =
    type Linear with
        static member modelObjectInstantiation : Linear -> string = fun linear ->
            let train_id =
                match linear.train with
                | Some train -> TrainId.modelRepresentation train.id
                | None       -> "null"
            let linear_id = LinearId.modelRepresentation linear.id
            sprintf "%s: Linear(train => %s);" linear_id train_id

    type Point with
        static member modelObjectInstantiation : Point -> string = fun point ->
            let model_point_id = PointId.modelRepresentation point.id
            sprintf "%s: Point;" model_point_id

    type Train with
        static member modelObjectInstantiation : Train * ModelObjects -> string =
            fun (train,objects) ->
            let train_id : string = TrainId.modelRepresentation train.id

            let pointPosition : RouteSegment -> string = function
                | PointRouteSegment (_,pos) -> PointPosition.modelRepresentation pos
                | _ -> "null"

            let route_segments : RouteSegments = Route.segments train.route

            let route_element_ids : string =
                route_segments
                |> Seq.map RouteSegment.modelRepresentation
                |> String.concat ","

            let track_lengths : string =
                route_segments
                |> Seq.map (RouteSegment.length >> string)
                |> String.concat ","

            let requested_point_positions : string =
                route_segments
                |> Seq.map pointPosition
                |> String.concat ","

            let occupies : string option =
                Seq.tryHead route_segments
                |> function
                   | Some(LinearRouteSegment(linear_id,_)) -> Some linear_id
                   | _ -> None
                |> Option.map (LinearId.modelRepresentation
                              >> Seq.replicate train.length
                              >> String.concat ",")

            match occupies with
            | Some occupies ->
                [ sprintf "%s: Train(" train_id
                ; sprintf "route_segments => [%s]," route_element_ids
                ; sprintf "track_lengths => [%s]," track_lengths
                ; sprintf "train_length => %i," train.length
                ; sprintf "occupies => [%s]," occupies
                ; sprintf "requested_point_positions => [%s]);" requested_point_positions ]
                |> String.concat "\n"
            | None -> sprintf """
                              %s: Train(
                              route_segments => [],
                              track_lengths => [],
                              train_length => %i,
                              occupies => [],
                              requested_point_positions => []);
                              """
                              train_id train.length

    let modelObjectInstantiation (Validated objects) : string =
        let trains : string seq =
            Map.toSeq objects.trains
            |> Seq.map (fun (_,train) -> Train.modelObjectInstantiation (train,objects))
        let linears : string seq =
            Map.toSeq objects.linears |> Seq.map (snd >> Linear.modelObjectInstantiation)
        let points : string seq =
            Map.toSeq objects.points |> Seq.map (snd >> Point.modelObjectInstantiation)
        [trains; linears; points]
        |> Seq.concat
        |> String.concat "\n\n"

[<AutoOpen>]
module Properties =
    /// Class containing generated properties
    /// (implements the ModelCheckingPropertyDefinitions interface)
    type ModelCheckingProperties(validated_objects : ValidatedModelObjects) =
        let (Validated objects) = validated_objects
        let trains = objects.trainList
        let points = objects.pointList

        interface ModelCheckingPropertyDefinitions with
            member this.NoMalfunctionsWhenTrainHasNotArrived : string =
                let p_malfunctions : string =
                    points
                    |> Seq.map (fun p -> getAbstractionName p pointMalfunction)
                    |> String.concat " or "
                let t_arrivals : string =
                    trains
                    |> Seq.map getAbstractionName
                    |> Seq.map ((|>)trainArrived)
                    |> String.concat " and "
                sprintf "not E[not (%s) U (final and not (%s))];"
                        p_malfunctions t_arrivals

            member this.NoCollision : string =
                all_pairs_of_trains_at_diff_positions
                |> getAbstractionName trains
                |> sprintf "AG (%s);"

            member this.AllTrainsArrived : string =
                trains
                |> Seq.map getAbstractionName
                |> Seq.map ((|>)trainArrived)
                |> String.concat " and "
                |> sprintf "EF AG (%s);"

            member this.NoDerailment : string =
                let positioningImpliesNoTrain = fun abstractionName ->
                    let point_is_positioning = abstractionName positioning
                    let no_train_detected =
                        abstractionName noTrainDetectedOnPoint
                    [ point_is_positioning; " implies "; no_train_detected ]
                    |> String.concat ""
                points
                |> Seq.map (getAbstractionName >> positioningImpliesNoTrain)
                |> String.concat " and\n"
                |> sprintf "AG (%s);"

            member this.TrainsDetectedOnPoints : string =
                let trainsAtPointImpliesTrainsDetected = fun (train1, train2, point) ->
                    let train1_not_on_point =
                        trainNotOnPoint |> getAbstractionName (train1,point)
                    let train2_not_on_point =
                        trainNotOnPoint |> getAbstractionName (train2,point)
                    let no_train_on_point =
                        noTrainDetectedOnPoint |> getAbstractionName point
                    [ "(not ("; train1_not_on_point; " and "; train2_not_on_point; ") "
                    ; "implies not "; no_train_on_point; ")" ]
                    |> String.concat ""
                points
                |> Seq.collect (fun point ->
                                uniqueProducts trains
                                |> Seq.map (fun (t1,t2) -> t1,t2,point))
                |> Seq.map trainsAtPointImpliesTrainsDetected
                |> String.concat " and\n"
                |> sprintf "AG (%s);"

            member this.AllMessagesHandled : string =
                getAbstractionName () discarded_msg
                |> sprintf "AG not (EX {%s} true);"

    /// Collects all the model properties into one string
    let collectModelProperties (properties : ModelCheckingPropertyDefinitions) =
        let trains_at_diff_positions =
            properties.NoCollision
            |> sprintf """
                -- safety property:
                -- no incident
                -- no trains occupy the same location node at the same time
                %s
                """
        let no_derailment =
            properties.NoDerailment
            |> sprintf """
                -- safety property:
                -- no trains are located at any 'point' while it is changing its position
                %s
                """
        let all_trains_arrived =
            properties.AllTrainsArrived
            |> sprintf """
                -- progress property that specifies that
                -- all trains has arrived at their destinations
                %s
                """
        let no_malfunction_when_train_has_not_arrived =
            properties.NoMalfunctionsWhenTrainHasNotArrived
            |> sprintf """
                -- property that specifies that
                -- there do not exist a final state where a train has not arrived and a malfunction has occurred
                %s
                """
        let trains_detected_on_points =
            properties.TrainsDetectedOnPoints
            |> sprintf """
                -- property to verify that all trains are correctly detected at points
                %s
                """
        let all_messages_handled =
            properties.AllMessagesHandled
            |> sprintf """
                -- no signal is ever lost in the system
                %s
                """

        [ trains_at_diff_positions
        ; no_derailment
        ; trains_detected_on_points
        ; all_trains_arrived
        ; no_malfunction_when_train_has_not_arrived
        ; all_messages_handled ]
        |> String.concat "\n"

/// Functions for composing a UMC model
module UMCModelConstruction =
    let private objects_section objects =
        modelObjectInstantiation objects
        |> sprintf "Objects\n %s"

    let private abstractions_section objects =
        objects
        |> AbstractionDefinitions.all_abstraction_declarations
        |> sprintf "Abstractions {\n%s\n}"

    let private properties_section objects =
        ModelCheckingProperties(objects)
        |> collectModelProperties

    let composeModel : ModelGeneratorFunction = fun objects ->
        [ train_class
        ; linear_class
        ; point_class
        ; objects_section objects
        ; abstractions_section objects
        ; properties_section objects ]
        |> String.concat "\n"

