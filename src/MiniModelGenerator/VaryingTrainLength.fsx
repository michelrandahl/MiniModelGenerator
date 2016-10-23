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

let generateTrains (t1_length : int) (t2_length : int) (intermed_length: int) (layout : SimpleLayout) : SimpleTrains =
    let layout_map = Map.ofList layout
    let layout_route1 =
        generateRoute layout_map (LLinear "1") Plus
        |> List.ofSeq
    let route1 =
        layout_route1
        |> List.mapi (fun i x ->
                      match i,x with
                      | 0,LLinear n -> RLinear(n, t1_length)
                      | i,LLinear n when i = List.length layout_route1 - 1 -> RLinear(n, t1_length)
                      | _,LLinear n -> RLinear(n, intermed_length)
                      | _,LPointFork(n,_) | _,LPointStem n -> RPoint(n, Plus))
    let layout_route2 =
        generateRoute layout_map (LLinear "4") Minus
        |> List.ofSeq
    let route2 =
        layout_route2
        |> List.mapi (fun i x ->
                      match i,x with
                      | 0,LLinear n -> RLinear(n, t2_length)
                      | i,LLinear n when i = List.length layout_route2 - 1 -> RLinear(n, t2_length)
                      | _,LLinear n -> RLinear(n, intermed_length)
                      | _,LPointFork(n,_) | _,LPointStem n -> RPoint(n, Minus))
        |> List.rev
    [ {id = "1"; length = t1_length; route = route1; route_direction = Up}
    ; {id = "2"; length = t2_length; route = route2; route_direction = Down} ]

for i in [2..10] do
    let layout = generateLayout 3
    let trains = generateTrains 10 10 i layout
    let outputfile = sprintf "../../UMCModels/VaryingTrainLength/model%i.umc" i
    generateUMCModel { trains = trains
                     ; layout = CustomLayout layout
                     ; show_stats = true
                     ; output_file = Some outputfile }
