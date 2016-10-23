namespace Tests.Utils
open Utils

module crossProductOfLists =
    open Xunit
    open FsUnit.Xunit
    open FsCheck.Xunit

    [<Fact>]
    let ``simple test1`` () =
        let result = crossProductOfLists [1;2] ["a";"b"] |> Set.ofSeq
        let expected = Set.ofList [1,"a"; 1,"b"; 2,"a"; 2,"b"]
        result |> should equal expected

    [<Property>]
    let ``length of cross product list is n**2, where n is the length of one of the input lists``
        (xs : int list) =
        let result =
            crossProductOfLists xs xs
            |> List.ofSeq
            |> List.length
        let expected = float(List.length xs)**2.0 |> int
        result = expected


module uniqueProducts =
    open FsCheck
    open FsCheck.Xunit

    let sets_bigger_than_two : Arbitrary<NonEmptySet<int>> =
        Arb.Default.NonEmptySet<int>()
        |> Arb.filter (fun x -> Set.count x.Get >= 2)
        |> Arb.filter (fun x -> Set.count x.Get <= 12)

    let n_choose_k n k =
        let rec factorial n =
            match n with
            | n when n = 0 -> 1
            | n when n = 1 -> 1
            | n -> factorial(n - 1) * n
        (factorial n) / ((factorial k)*(factorial (n - k)))

    [<Property>]
    let ``product elements are unique in each product``() =
        Prop.forAll sets_bigger_than_two (fun xs ->
            List.ofSeq xs.Get
            |> uniqueProducts
            |> Seq.map (fun (e1,e2) -> e1 <> e2)
            |> Seq.reduce (&&))

    //[<Property(Verbose = true)>]
    [<Property>]
    let ``n choose k products produced``() =
        Prop.forAll sets_bigger_than_two (fun xs ->
            let input = xs.Get
            let n = Set.count input
            let k = 2
            let expected = n_choose_k n k
            let result =
                List.ofSeq input
                |> uniqueProducts
                |> Seq.length
            expected = result)


module Result =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``simple success test of flow`` () =
        let result : Result<int,string> = resultFlow {
            let! x = Ok 42
            let! y = Ok 3
            return x + y }
        let expected : Result<int,string> = Ok 45
        result |> should equal expected

    [<Fact>]
    let ``simple fail test of flow`` () =
        let result = resultFlow {
            let! x = Ok 42
            let! y = Error "wrong"
            return x + y }
        let expected : Result<int,string> = Error "wrong"
        result |> should equal expected

    [<Fact>]
    let ``simple success test of sequence`` () =
        let input = [Ok 1; Ok 2; Ok 3]
        let expected = Ok [1;2;3]
        let result = Result<_,_>.sequence input
        result |> should equal expected

    [<Fact>]
    let ``simple fail test of sequence`` () =
        let input = [Ok 1; Error "wrong"; Ok 3]
        let expected : Result<int list,string> = Error "wrong"
        let result = Result<_,_>.sequence input
        result |> should equal expected

    [<Fact>]
    let ``simple success test of traverse`` () =
        let input = [1 .. 5]
        let expected : Result<int list, string> = Ok [1 .. 5]
        let result =
            input
            |> Result<_,_>.traverse
                (function
                 | n when n = 0 -> Error "wrong"
                 | n -> Ok n)
        result |> should equal expected

    [<Fact>]
    let ``simple fail test of traverse`` () =
        let input = [0 .. 5]
        let expected : Result<int list, string> = Error "wrong"
        let result =
            input
            |> Result<_,_>.traverse
                (function
                  | n when n = 0 -> Error "wrong"
                  | n -> Ok n)
        result |> should equal expected

module maybe =
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let ``simple success test of maybe flow`` () =
        let result : int option = maybe {
            let! x = Some 42
            let! y = Some 3
            return x + y }
        let expected : int option = Some 45
        result |> should equal expected

    [<Fact>]
    let ``simple fail test of maybe flow`` () =
        let result : int option = maybe {
            let! x = Some 42
            let! y = None
            return x + y }
        let expected : int option = None
        result |> should equal expected
