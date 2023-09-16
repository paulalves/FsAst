namespace FsAst

module Tests =
    open Xunit
    open FsAst.Library.Exp
    open Xunit.Abstractions

    type ParserTests(output: ITestOutputHelper) =        
        [<Fact>]
        member this.``Parse a simple expression``() =
            "41+1" |> parse |> sprintf "%A" |> output.WriteLine
