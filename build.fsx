// include Fake libs
#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open Fake.Testing

// Directories
let buildDir  = "./build/"
let testDir   = "./test/"

// Targets
Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir])

Target "Build" (fun _ ->
    !! "src/MiniModelGenerator/*.fsproj"
    |> MSBuildRelease buildDir "Build"
    |> Log "AppBuild-Output: ")

Target "BuildTest" (fun _ ->
    !! "src/Tests/*.fsproj"
    |> MSBuildDebug testDir "Build"
    |> Log "TestBuild-Output: ")

Target "Test" (fun _ ->
    !! (testDir + "/Tests.dll")
    |> XUnit2.xUnit2 (fun p ->
        { p with ToolPath = "packages/xunit.runner.console/tools/xunit.console.exe"
                 HtmlOutputPath = Some "TestResults.html"
                 NoAppDomain = true}))

// Build order
"Clean"
  ==> "Build"
  ==> "BuildTest"
  ==> "Test"

// start build
RunTargetOrDefault "Test"
