module MiniModelGenerator

open System.Text.RegularExpressions
open InterlockingModel
open UMC
open XMLExtraction
open Utils
open System
open System.IO

type ModelOutput = UMC
                 | Raw // representing the raw F# objects

type ModelOutput with
    static member fromString (s : string) : Result<ModelOutput, string> =
        match s.ToLower() with
        | "umc" -> Ok UMC
        | "raw" -> Ok Raw
        | _ -> Error "wrong model type"

let parseItin arg =
    match Regex("[^\[\]]+").Match arg with
    | m when m.Success && m.Value <> "" ->
        let parsed =
            m.Value.Split ','
            |> Array.filter ((<>)"")
            |> List.ofArray
        if Seq.length parsed > 0
        then Ok parsed
        else Error "error in itinerary argument no routes provided"
    | _ -> Error "error in itinerary argument"

let parseInt arg =
    match Int32.TryParse arg with
    | true, x -> Ok x
    | _ -> Error (sprintf "not an integer: %A" arg)

let parseBool arg =
    match Boolean.TryParse arg with
    | true, x -> Ok x
    | _ -> Error (sprintf "not a bool: %A" arg)

let getPath curr_dir path_arg =
    if File.Exists path_arg
    then Ok path_arg
    else
        let full_path = sprintf "%s/%s" curr_dir path_arg
        if File.Exists full_path
        then Ok full_path
        else Error (sprintf "no file with path %s" path_arg)

[<EntryPoint>]
let main argv =
    let curr_dir = Directory.GetCurrentDirectory()
    if Array.length argv < 4 then
        [ "Following arguments must be provided (in same order):"
          "1. file-path of xml file"
          "2. model type (umc|raw)"
          "3. verify length constraints (true|false)"
          "4. itinerary for train 1 in the form [r_1,r_2,...]"
          "5. itinerary for train 2 in the form [r_3,r_4,...]"
          "6. ... "]
        |> String.concat "\n"
        |> printfn "%s"
    else
      let model = resultFlow {
          let! path = getPath curr_dir argv.[0]
          let! model_output_type = ModelOutput.fromString argv.[1]
          let! verify_length_constraints = parseBool argv.[2]
          let! routes =
              [3 .. Array.length argv - 1]
              |> Result<_,_>.traverse
                  (fun arg_id -> resultFlow {
                   let! route = parseItin argv.[arg_id]
                   return route })
          let parameters =
              { modelGeneratorFunction = generateRawModel
                xml_file_path = path
                routes = routes }
          let! model =
              match model_output_type with
              | Raw -> generateModelFromXML verify_length_constraints parameters
              | UMC ->
                  { parameters with modelGeneratorFunction = UMCModelConstruction.composeModel }
                  |> generateModelFromXML verify_length_constraints
          return model }
      match model with
      | Ok model -> printfn "%s" model
      | Error err -> printfn "ERROR: \n%s" err
    0 // return an integer exit code
