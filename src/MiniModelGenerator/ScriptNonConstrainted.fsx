(*
   Example script for generating a UMC model with a custom layout (and no checking for length constraints)
*)

#load "Prelude.fsx"

(* importing required modules *)
open InterlockingModel
open ScriptingTools

(* Define the trains and routes to be used in the model *)
let trains : SimpleTrains =
    [ { id = "1"
        length = 2
        route = [ RLinear(name = "1", length = 2)
                ; RPoint(name = "1", position = Plus)
                ; RLinear(name = "2", length = 4)
                ; RPoint(name = "2", position = Plus)
                ; RLinear(name = "4", length = 4)
                ; RLinear(name = "5", length = 2) ]
        route_direction = Up }
      { id = "2"
        length = 3
        route = [ RLinear(name = "5", length = 3)
                ; RLinear(name = "4", length = 2)
                ; RPoint(name = "2", position = Minus)
                ; RLinear(name = "3", length = 3) ]
        route_direction = Down } ]

(* Define the network layout in a 'left-to-right' fashion,
   using '+>' to indicate connection between elements *)
let network_layout : SimpleLayout =
    [ LLinear "1"            <+> LPointStem "1"
    ; LPointFork("1", Plus)  <+> LLinear "2"
    ; LPointFork("1", Minus) <+> LLinear "3"
    ; LLinear "2"            <+> LPointFork("2",Plus)
    ; LLinear "3"            <+> LPointFork("2",Minus)
    ; LPointStem "2"         <+> LLinear "4"
    ; LLinear "4"            <+> LLinear "5" ]

(* Generate model based on the definitions above *)
//generateUMCModelWithConstraintedLengths
generateUMCModelWithNoLengthConstraints
    { trains = trains
    ; layout = CustomLayout(network_layout)
    ; show_stats = true
    ; output_file = Some "mymodel.txt" }
