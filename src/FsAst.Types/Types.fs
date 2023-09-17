namespace FsAst.Types

open FsAst.Library

(* Non Idiomatic F# for OOP fanatics and C# ppl who can't Function *)

type Lexer =
    member _.Lex(input) : TokenStream = input |> tokenize

type Parser(lexer: Lexer) =
    member this.Log exp : string = exp |> this.Parse |> sprintf "%A"

    member _.Parse exp : Term = exp |> lexer.Lex |> parseTokens

type Interpreter (context, parser : Parser) =    
    member this.Log exp =
        let ast = parser.Parse exp |> sprintf "%A"
        let res = this.Interpret exp |> sprintf "%A"
        sprintf "(Ast = [ %s ], Result = [ %s ])" ast res

    member _.Interpret exp : decimal =
        evalAst context (
            exp
            |> parser.Parse            
        ) |> decimal