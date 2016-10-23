module TestUtils

open FsCheck
open FsCheck.Xunit


let traverseGen (f : 'a -> Gen<'b>) (xs : 'a list) : Gen<'b list> =
    let folderFun (head : 'a) (tail : Gen<'b list>) : Gen<'b list> =
        gen { let! h = f head
              let! t = tail
              return h :: t }
    let initial = Gen.constant []
    List.foldBack folderFun xs initial
