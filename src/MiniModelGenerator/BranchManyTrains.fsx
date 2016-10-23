#load "Prelude.fsx"

open InterlockingModel
open ScriptingTools

let branchLayout xs =
    let rec loop xs  = seq {
        match xs with
        | [x;y] ->
            let prev = x + y
            let prev_id = string prev
            yield LPointFork(prev_id, Plus) , LLinear (string x)
            yield LPointFork(prev_id, Minus), LLinear (string y)
        | xs when List.length xs > 2 ->
            let current_id = xs |> Seq.sum |> string
            let half = (List.length xs) / 2
            let xs_left = xs |> List.take half
            let xs_right = xs |> List.skip half
            let stem_left_id = xs_left |> Seq.sum |> string
            let stem_right_id = xs_right |> Seq.sum |> string

            yield LPointFork(current_id, Plus), LPointStem stem_left_id
            yield! loop xs_left
            yield LPointFork(current_id, Minus), LPointStem stem_right_id
            yield! loop xs_right
        | _ -> () }
    let tail = xs |> loop |> List.ofSeq
    let stem_id = xs |> Seq.sum |> string
    let head = LLinear "0", LPointStem stem_id
    head :: tail

let getRouteTrace layout_map start end' =
    let rec loop prev route =
        match layout_map |> Map.tryFind prev with
        | Some(LLinear n) when (LLinear n) = (LLinear end') ->
            RLinear(n, 2) :: route
        | Some(LLinear n) ->
            (RLinear(n,2) :: route)
            |> loop (LLinear n)
        | Some(LPointStem n) ->
            let result_left =
                //(LPointFork(n, Plus) :: route) |> loop
                (RPoint(n, Plus) :: route)
                |> loop (LPointFork(n, Plus))
            let result_right =
                //(LPointFork(n, Minus) :: route) |> loop
                (RPoint(n, Minus) :: route)
                |> loop (LPointFork(n, Minus))
            match result_left, result_right with
            | x::xs, [] -> x::xs
            | [], y::ys -> y::ys
            | _ -> []
        | Some(LPointFork(n,pos)) ->
            //(LPointStem n :: route) |> loop
            (RPoint(n,pos) :: route)
            |> loop (LPointStem n)
        | None -> []
    loop (LLinear start) [RLinear(start, 2)]

let dualBranch (levels : int) (num_trains : int) =
    let size = int(2.0**(float levels))
    let [xs; ys] = [1..2*size] |> List.chunkBySize size
    let right = branchLayout ys
    let left = branchLayout xs |> List.map (fun (x,y) -> y,x)
    let layout = [left; right] |> List.concat
    let layout_map = Map.ofList layout
    let getRouteInLayout = getRouteTrace layout_map
    let trains =
        List.zip xs ys
        |> List.map
            (fun (l1,l2) ->
             let lin1 = string l1
             let lin2 = string l2
             let route = getRouteInLayout lin1 lin2
             {id=lin1; length=2; route=route; route_direction=Down})
        |> List.take num_trains
        |> List.ofSeq
    layout, trains

for i in [2..4] do
    let layout, trains = dualBranch 2 i
    let output_file = sprintf "../../UMCModels/ManyTrains2/model%i.umc" i
    generateUMCModel { trains = trains
                     ; layout = CustomLayout layout
                     ; show_stats = true
                     ; output_file = Some output_file }

