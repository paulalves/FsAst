namespace FsAst.Library

open System.Text.RegularExpressions

[<AutoOpen>]
module public Ast =
    type Token =
        | Identifier of string
        | Number of int
        | Add
        | Subtract

    type Term =
        | Var of string
        | Const of int
        | Op of Term * string * Term

    type TokenStream = Token list

[<AutoOpen>]
module private Parser =
    let regex = Regex @"((?<token>(\d+|\w+|\^|\+|-|\*|/)))"

    let tokenize (input: string) =
        [ for x in regex.Matches(input) do
              let token =
                  match x.Groups.["token"].Value with
                  | "+" -> Add
                  | "-" -> Subtract
                  | ch when System.Char.IsDigit ch.[0] -> Number(int ch)
                  | ch -> Identifier ch

              yield token ]

    let read (stream: TokenStream) =
        match stream with
        | head :: tail ->
            Some(head, tail)
        | _ -> None

    let rec parseTerm (stream: TokenStream) =
        match read stream with
        | Some(Number number, stream) ->
            Const(number), stream
        | Some(Identifier id, stream) ->
            let term = Var(id)
            term, stream
        | None -> failwith "The stream is empty"
        | Some _ -> failwith "Unexpected token"

    let rec parseExpr (stream: TokenStream) =
        let term, stream = parseTerm stream
        match read stream with
        | Some(Add, stream) ->
            let next, stream = parseExpr stream
            Op(term, "+", next), stream
        | Some(Subtract, stream) ->
            let next, stream = parseExpr stream
            Op(term, "-", next), stream
        | _ -> term, stream

module Exp =
    let parse (input: string) =
        input
            |> tokenize 
            |> parseExpr
            |> function | (ast, []) -> ast
                        | _ -> failwith "Unexpected token"
