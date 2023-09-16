namespace FsAst.Library

open System.Text.RegularExpressions

[<AutoOpen>]
module public Ast =
    type Token =
        | Identifier of string
        | Number of int
        | Add
        | Subtract
        | Power
        | Multiply
        | Division

    type Term =
        | Var of string
        | Const of int
        | Op of Term * string * Term

    type TokenStream = Token list

[<AutoOpen>]
module internal Parser =
    let regex = Regex @"((?<token>(\d+|\w+|\^|\+|-|\*|/)))"

    let isDigit (ch: string) = ch.[0] |> System.Char.IsDigit

    let tokenize (input: string) =
        [ for x in regex.Matches(input) do
              let token =
                  match x.Groups.["token"].Value with
                  | "+" -> Add
                  | "-" -> Subtract
                  | "*" -> Multiply
                  | "^" -> Power
                  | "/" -> Division
                  | ch when isDigit ch -> Number(int ch)
                  | ch -> Identifier(ch)
              yield token ]

    let read (stream: TokenStream) =
        match stream with
        | head :: tail -> Some(head, tail)
        | _ -> None

    let rec parseTerm (stream: TokenStream) =
        match read stream with
        | Some(Number number, stream) -> Const(number), stream
        | Some(Identifier id, stream) ->
            let term = Var(id)
            match read stream with
            | Some(Power, _) ->
                let powerTerm, stream = parsePower stream
                Op(term, "^", powerTerm), stream
            | _ -> term, stream
        | _ -> failwith "Unexpected token"
        
    and parsePower (stream: TokenStream) =
        match read stream with
        | Some(Power, stream) ->
            let rhs, stream = parseTerm stream
            match rhs with
            | Const _ -> parsePower stream
            | _ -> failwith "The right hand side of a power must be a constant"
        | _ -> Const 1 , stream

    let rec parseExpr (stream: TokenStream) =
        let term, stream = parseTerm stream
        match read stream with
        | Some(Add, stream) ->
            let next, stream = parseExpr stream
            Op(term, "+", next), stream
        | Some(Subtract, stream) ->
            let next, stream = parseExpr stream
            Op(term, "-", next), stream
        | Some(Multiply, stream) ->
            let next, stream = parseExpr stream
            Op(term, "*", next), stream
        | Some(Division, stream) ->
            let next, stream = parseExpr stream
            Op(term, "/", next), stream
        | Some(Power, stream) ->
            let next, stream = parseExpr stream
            Op(term, "^", next), stream
        | _ -> term, stream

module Exp =
    let parse (input: string) =
        input
        |> tokenize
        |> parseExpr
        |> function
            | ast, [] -> ast
            | _ -> failwith "Unexpected token"