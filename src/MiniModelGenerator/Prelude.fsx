(*
   Loading the required files and assemblies for the scripts
*)

// required assembly for parsing xml files

#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

(* loading required files *)
#load "Utils.fs"
#load "InterlockingModel.fs"
#load "UMCTrainClass.fs"
#load "UMCLinearClass.fs"
#load "UMCPointClass.fs"
#load "UMC.fs"
#load "XMLExtraction.fs"
#load "ScriptTools.fs"

open ScriptingTools
