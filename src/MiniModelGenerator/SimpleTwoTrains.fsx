#load "Prelude.fsx"

open InterlockingModel
open ScriptingTools

let basic_layout (names : string list) =
    let [l1; p2; l3; l4; p5; l6] = names
    [ LLinear l1            <+> LPointStem p2
    ; LPointFork(p2, Plus)  <+> LLinear l3
    ; LPointFork(p2, Minus) <+> LLinear l4
    ; LLinear l3            <+> LPointFork(p5,Plus)
    ; LLinear l4            <+> LPointFork(p5,Minus)
    ; LPointStem p5         <+> LLinear l6 ]

let generateLayout (N : int) : SimpleLayout =
    let layouts = seq {
        for n in [0..N-1] do
            let start = n * 5 + 1
            let end' = start + 5
            let layout = [start .. end']
                        |> List.map string
                        |> basic_layout
            yield layout }
    List.ofSeq layouts
    |> List.concat

let generateRoute layout_map (segment : SimpleTrackSegment) (pos : PointPosition) =
    let rec loop segment = seq {
        yield segment
        let next_segment = layout_map |> Map.tryFind segment
        match next_segment with
        | Some segment' ->
            match segment' with
            | LPointStem n -> yield! loop (LPointFork(n, pos))
            | LPointFork(n,_) -> yield! loop (LPointStem n)
            | _ -> yield! loop segment'
        | None -> () }
    loop segment

let generateTrains (t1_length : int) (t2_length : int) (layout : SimpleLayout) : SimpleTrains =
    let layout_map = Map.ofList layout
    let route1 =
        generateRoute layout_map (LLinear "1") Plus
        |> List.ofSeq
        |> List.map (function
                     | LLinear n -> RLinear(n, t1_length)
                     | LPointStem n | LPointFork(n,_) -> RPoint(n, Plus))
    let route2 =
        generateRoute layout_map (LLinear "4") Minus
        |> List.ofSeq
        |> List.map (function
                     | LLinear n -> RLinear(n, t2_length)
                     | LPointStem n | LPointFork(n,_) -> RPoint(n, Minus))
        |> List.rev
    [ {id = "1"; length = t1_length; route = route1; route_direction = Up}
    ; {id = "2"; length = t2_length; route = route2; route_direction = Down} ]

for i in [1..10] do
    let layout = generateLayout i
    let trains = generateTrains 2 2 layout
    let output_file = sprintf "../../UMCModels/SimpleTwoTrains/model%i.umc" i
    generateUMCModelWithConstraintedLengths
        { trains = trains
        ; layout = CustomLayout layout
        ; show_stats = true
        ; output_file = Some output_file }

