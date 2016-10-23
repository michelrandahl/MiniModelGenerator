module Utils

open System

/// Generate all unique products with values from a given list
/// where no product contains a pair of identical values
let rec uniqueProducts (xs : 'a list) : ('a * 'a) seq = seq {
    match xs with
    | x::xs ->
        for y in xs do
            yield x,y
        yield! uniqueProducts xs
    | _ -> () }

/// Generate all cross products of two sequences
let crossProductOfLists xs ys = seq {
    for x in xs do
        for y in ys do
            yield x,y }

/// Generate 'n choose k' combinations of values from list xs
/// where n is length of xs and each combination is of length k
let combinations (k : int) (xs : 'a list) : ('a list) seq =
    let rec loop (k : int) (xs : 'a list) : ('a list) seq = seq {
        match xs with
        | [] -> ()
        | xs when k = 1 -> for x in xs do yield [x]
        | x::xs ->
            let k' = k - 1
            for ys in loop k' xs do
                yield x :: ys
            yield! loop k xs }
    loop k xs
    |> Seq.filter (List.length >> (=)k)

// In F# 4.1 the Result type will be in the core library with exactly the same definition
type Result<'success,'error> = Ok of 'success
                             | Error of 'error
    with
    static member map (f : 'a -> 'b) (x : Result<'a,'error>) : Result<'b, 'error> =
        match x with
        | Ok x -> Ok (f x)
        | Error err -> Error err
    /// Binding value in result to parameter of a continuation function
    static member bind (x : Result<'a, 'error>) (continueationFun : 'a -> Result<'b, 'error>)
        : Result<'b, 'error> =
        match x with
        | Error err -> Error err
        | Ok x -> continueationFun x


// Computation-Expression definition for the Result type.
// The defined Result type has same functionality as
// the more commonly known Either monad defined in Haskell and other languages.
// The reason why it's called Result in this code, is because
// (soon to-be-released) F# 4.1 will have a Result type defined in its core library.
type ResultBuilder() =
    member this.Bind (x, f) = Result<_,_>.bind x f
    member this.Return (x : 'a) : Result<'a,'error> = Ok x
let resultFlow = new ResultBuilder()

let private traverseResults (f : 'a -> Result<'b, 'error>) (xs : 'a list)
    : Result<'b list, 'error> =
    let folderFun (head : 'a) (tail : Result<'b list, 'error>) : Result<'b list, 'error> =
        resultFlow { let! (h : 'b) = f head
                     let! (t : 'b list) = tail
                     return h :: t }
    let initial_val = Ok []
    // folding from right to left in order to maintain original order of the input list (xs)
    List.foldBack folderFun xs initial_val

let private sequenceResults (xs : Result<'a, 'error> list) : Result<'a list, 'error> =
    traverseResults id xs

let private reduceResults (f : 'a -> 'a -> Result<'a,'error>) (xs : Result<'a, 'error> seq)
    : Result<'a,'error> =
    let reducerFun = fun x y -> resultFlow {
        let! x' = x
        let! y' = y
        let! combined = f x' y'
        return combined }
    xs |> Seq.reduce reducerFun

let private foldResults
    (f : 'a -> 'b -> Result<'b,'error>) (initial : Result<'b,'error>) (xs : 'a list)
    : Result<'b,'error> =
    let folderFun (state : Result<'b,'error>) (x : 'a) : Result<'b, 'error> =
        Result<_,_>.bind state (f x)
    Seq.fold folderFun initial xs

// Extending the Result type with a set of functions for handling Result types
type Result with
    /// Applies a function ('a -> Result) on all elements in a list
    /// and lifts the list of results to a Result containing a list
    static member traverse f xs = traverseResults f xs

    /// Lifts a list of Results to a Result with a list
    static member sequence xs = traverseResults id xs

    /// Reduces a list of Result<a,..> to a Result<a,..>
    /// using a function a -> a -> Result<a,..>
    static member reduce f xs = reduceResults f xs

    /// Fold over a list of 'a with an initial value of 'b
    /// and a function 'a -> 'b -> Result<'b,..>
    static member fold f initial xs = foldResults f initial xs

    /// A bind operator for conveniently chaining together
    /// functions that produce Result types from non-Result types
    static member (>>=) (a: Result<'a, 'error>, f : 'a -> Result<'b, 'error>) : Result<'b, 'error> =
        Result<_,_>.bind a f


// computation expression definition for the Option type (called Maybe in other languages)
type MaybeBuilder() =
    member this.Bind (m : 'a option, f : 'a -> 'b option) : 'b option =
        Option.bind f m
    member this.Return (x : 'a) : 'a option = Some x
let maybe = new MaybeBuilder()
