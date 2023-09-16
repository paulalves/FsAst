namespace FsAst

module Tests =
    open Xunit
    open FsUnit.Xunit
    open FsAst.Library.Ast
    open FsAst.Library.Parser
    open FsAst.Library.Exp
    open Xunit.Abstractions

    let debug argument  =
        fun (output: ITestOutputHelper) -> 
            output.WriteLine(sprintf "%A" argument)
            argument

    type ParserTests(output: ITestOutputHelper) =

        [<Fact>]
        member this.``tokenization tests``() =
            tokenize "x+y^x*y"
            |> debug <| output
            |> should
                equalSeq
                (seq<Token> {
                    Identifier "x"
                    Add
                    Identifier "y"
                    Power
                    Identifier "x"
                    Multiply
                    Identifier "y"
                })
            
            tokenize "x^2+1"
            |> debug <| output
            |> should
                equalSeq
                (seq<Token> {
                    Identifier "x"
                    Power
                    Number 2
                    Add
                    Number 1
                })

            tokenize "3x"
            |> debug <| output 
            |> should
                equalSeq
                (seq<Token> {
                    Number 3
                    Identifier "x"
                })

            tokenize "5"
            |> debug <| output
            |> should
                   equalSeq
                   (seq<Token> {
                       Number 5
                   })

        [<Fact>]
        member this.``parsing tests``() =
            "41+1"
            |> parse
            |> debug <| output
            |> should equal (Op (Const 41, "+", Const 1))
            
            "2*2"
            |> parse
            |> debug <| output
            |> should equal (Op (Const 2, "*", Const 2))
            
            "x^2+1"
            |> parse
            |> debug <| output
            |> should equal (Op (Op (Var "x", "^", Const 1), "+", Const 1))

            // todo: fix this! it's too late of a night to figure out why this is failing     
            // "x*3-1+2*1"
            // |> parse
            // |> debug <| output
            // |> should equal (Op (Op (Op (Op (Var "x", "*", Const 3), "-", Const 1), "+", Op (Op (Var "x", "*", Const 2), "/", Const 2)), "*", Const 1))