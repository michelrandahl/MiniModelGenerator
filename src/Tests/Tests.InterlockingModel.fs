namespace Tests.InterlockingModel
open Utils
open InterlockingModel

module RouteConstruction =
    open FsCheck
    open FsCheck.Xunit

    /// Generate integers bigger than zero
    let int_bigger_than_zero =
        Arb.generate<NonNegativeInt>
        |> Gen.where (fun i -> i.Get > 0)

    /// Generator of random routes
    let routeGen : Gen<Route> =
        let route s =
            let direction = Arb.generate<RouteDirection>
            let linear_segment id (l : NonNegativeInt) =
                LinearRouteSegment(LinearId id, l.Get)
            let point_segment id p =
                PointRouteSegment(PointId id, p)
            let segment id =
                [ Gen.map (linear_segment id) int_bigger_than_zero
                ; Gen.map (point_segment id) Arb.generate<PointPosition> ]
                |> Gen.oneof
            let segments =
                [for id in [1..s] |> Seq.map string -> segment id]
                |> Gen.sequence
            Gen.map2 (fun s d -> Route(s,d)) segments direction
        Gen.sized route
    let validRouteArb : Arbitrary<Route> =
        Arb.fromGen routeGen
        |> Arb.filter
            (fun (Route(segments,_)) ->
             let first_is_linear =
                 match Seq.head segments with
                 | LinearRouteSegment _ -> true
                 | _ -> false
             let last_is_linear =
                 match Seq.last segments with
                 | LinearRouteSegment _ -> true
                 | _ -> false
             first_is_linear
             && last_is_linear)

    [<Property>]
    let ``route split in half and stitched with stitchRoutePair yields same route`` () =
        Prop.forAll validRouteArb
            (fun route ->
             let (Route(all_segments, dir)) = route
             let half = (List.length all_segments) / 2
             let segments1 = List.take half all_segments
             let segments2 = List.skip (half-1) all_segments
             let route1 = Route(segments1, dir)
             let route2 = Route(segments2, dir)
             let result = stitchRoutePair route1 route2
             match result with
             | Ok result_route -> result_route = route
             | Error msg -> raise (System.ArgumentException(msg)))

    // gen route from layout and verifyRoutes
    // gen length constrained routes and checkLengthConstraints
