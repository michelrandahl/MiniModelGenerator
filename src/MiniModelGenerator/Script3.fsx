#load "Prelude.fsx"

(* importing required modules *)
open InterlockingModel
open ScriptingTools

let network : SimpleLayout =
    [ LLinear "1"            <+> LPointFork("1", Plus)
      LLinear "3"            <+> LPointFork("1", Minus)
      LPointStem "1"         <+> LPointStem "2"
      LPointFork("2", Plus)  <+> LLinear "2"
      LPointFork("2", Minus) <+> LLinear "4" ]

let trains : SimpleTrains =
    [ { id = "1"
      ; length = 2
      ; route = [ RLinear("1", 2)
                  RPoint("1", Plus)
                  RPoint("2", Plus)
                  RLinear("2", 2) ]
      ; route_direction = Up }
      { id = "2"
      ; length = 3
      ; route = [ RLinear("3", 3)
                  RPoint("1", Minus)
                  RPoint("2", Minus)
                  RLinear("4", 3) ]
      ; route_direction = Up } ]

generateUMCModelWithConstraintedLengths
    { trains = trains
    ; layout = CustomLayout(network)
    ; show_stats = true
    ; output_file = Some "mymodel.txt" }
