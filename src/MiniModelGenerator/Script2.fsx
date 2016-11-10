(*
   Example script for generating a UMC model with a layout from an xml file
*)
#load "Prelude.fsx"

(* importing required modules *)
open InterlockingModel
open ScriptingTools

let path = "sample.xml"
(* investigate the layout by loading and printing the raw layout *)
printRawLayout path
(* ^^ outputs following
LLinear "b10"            +> LLinear "t10"
LLinear "t10"            +> LPointStem "t11"
LLinear "t12"            +> LPointFork ("t13",Plus)
LLinear "t14"            +> LLinear "b14"
LLinear "t20"            +> LPointFork ("t13",Minus)
LPointStem "t11"         +> LLinear "t10"
LPointStem "t13"         +> LLinear "t14"
LPointFork ("t11",Plus)  +> LLinear "t12"
LPointFork ("t11",Minus) +> LLinear "t20"
LPointFork ("t13",Plus)  +> LLinear "t12"
LPointFork ("t13",Minus) +> LLinear "t20"
*)

(* Define the trains and routes to be used in the model *)
let trains : SimpleTrains = [
    { id = "1" ; length = 3
    ; route = [ RLinear(name = "b10", length = 3)
              ; RLinear(name = "t10", length = 3)
              ; RPoint(name = "t11", position = Plus)
              ; RLinear(name = "t12", length = 3)
              ; RPoint(name = "t13", position = Plus)
              ; RLinear(name = "t14", length = 3) ]
    ; route_direction = Up }
    { id = "2"
    ; length = 2
    ; route = [ RLinear(name = "b14", length = 2)
              ; RLinear(name = "t14", length = 2)
              ; RPoint(name = "t13", position = Minus)
              ; RLinear(name = "t20", length = 2) ]
    ; route_direction = Down } ]

generateUMCModelWithConstraintedLengths {
    trains = trains
    layout = XMLLayout path
    show_stats = true
    output_file = Some "mymodel2.txt" }

