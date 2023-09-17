namespace FsAst.Types

open System
open FsAst.Library
open Microsoft.FSharp.Core

(* Non Idiomatic F# for OOP fanatics and C# ppl who can't Function *)

[<CLSCompliant true>]
type Lexer () =
    member _.Lex(input) : TokenStream = input |> tokenize
    
    member this.Log exp : string = exp |> tokenize |> sprintf "(Tokens = [ %A ])"

[<CLSCompliant true>]
type Parser(lexer: Lexer) =
    member this.Log exp =
        let ast = this.Parse exp |> sprintf "%A"
        sprintf "(Ast = [ %s ])" ast 

    member _.Parse exp : Term = exp |> lexer.Lex |> parseTokens

[<CLSCompliant true>]
type Interpreter (context, parser : Parser) =    
    member _.Interpret exp : int =        
        evalAst context (
            exp
            |> parser.Parse            
        )