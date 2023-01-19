module Ji.Tests.ReaderTests

open Xunit
open FsCheck.Xunit
open Ji.Ast
open Ji.Reader

[<Property>]
let ``reads integers`` (num: int) =
    Assert.Equal(read $"{num}", ExprInt num)
