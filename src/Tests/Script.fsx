// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "../../packages/FsCheck/lib/net45/FsCheck.dll"
#r "../../packages/xunit.abstractions/lib/portable-net45+win+wpa81+wp80+monotouch+monoandroid+Xamarin.iOS/xunit.abstractions.dll"
#r "../../packages/FsCheck.Xunit/lib/net45/FsCheck.Xunit.dll"
#load "../MiniModelGenerator/Utils.fs"
#load "../MiniModelGenerator/InterlockingModel.fs"
#load "TestUtils.fs"

open Utils
open InterlockingModel
open TestUtils
open System
open FsCheck
open FsCheck.Xunit

type Tree = Leaf of int | Branch of Tree * Tree

let tree =
    let rec tree' s =
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 ->
            let subtree = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Arb.generate<int>
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

Gen.sample 10 10 tree
|> printfn "%A"

let myarb = Arb.generate<PointPosition>
myarb.Sample(10,10)
|> printfn "%A"

let x = Arb.Default.NonNegativeInt()
let y = NonNegativeInt

let routeGen =
    let route s =
        let direction = Arb.generate<RouteDirection>
        let bigger_than_zero =
            Arb.generate<NonNegativeInt>
            |> Gen.where (fun i -> i.Get >= 1)
        let linear_segment id (l : NonNegativeInt) =
            LinearRouteSegment(LinearId id, l.Get)
        let point_segment id p =
            PointRouteSegment(PointId id, p)
        let segment id =
            [ Gen.map (linear_segment id) bigger_than_zero
            ; Gen.map (point_segment id) Arb.generate<PointPosition> ]
            |> Gen.oneof
        let segments =
            [for id in [1..s] |> Seq.map string -> segment id]
            |> Gen.sequence
        Gen.map2 (fun s d -> Route(s,d)) segments direction
    Gen.sized route

Gen.sample 10 10 routeGen
|> printfn "%A"

//let sets_bigger_than_two =
//    Arb.Default.NonEmptySet<int>()
//    |> Arb.filter (fun x -> Set.count x.Get >= 2)
//
//let mygen = sets_bigger_than_two.Generator
//
//mygen.Sample(10,10)
//|> printfn "%A"
//
//let n_choose_k n k =
//    let rec factorial n =
//        match n with
//        | n when n = 0 -> 1
//        | n when n = 1 -> 1
//        | n -> factorial(n - 1) * n
//    (factorial n) / ((factorial k)*(factorial (n - k)))
//
//
//n_choose_k 10 2
//|> printfn "%A"
//
//uniqueProducts [1 .. 4]
//|> Seq.length
//|> printfn "%A"
