(*
   Example script for generating a UMC model with a layout from an xml file
*)
#load "Prelude.fsx"

(* importing required modules *)
open InterlockingModel
open ScriptingTools

let path = "lyngby.xml"
//printRawLayout path

(* Define the trains and routes to be used in the model *)
let trains : SimpleTrains = [
    { id = "1" ; length = 2
    ; route = [ RLinear(name = "b30", length = 2)
              ; RLinear(name = "t30", length = 2)
              ; :q
                :]
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
//
//generateUMCModel {
//    trains = trains
//    layout = XMLLayout path
//    show_stats = true
//    output_file = Some "mymodel2.txt" }

