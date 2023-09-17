namespace FsAst.Library

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
    open System.Text.RegularExpressions
    
    let private regex = Regex @"((?<token>(\d+|\w+|\^|\+|-|\*|/)))"

    let private isDigit (ch: string) = ch.[0] |> System.Char.IsDigit

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

    let private read (stream: TokenStream) =
        match stream with
        | head :: tail -> Some(head, tail)
        | _ -> None

    let rec private parseTerm (stream: TokenStream) =
        match read stream with
        | Some(Number number, stream) -> Const(number), stream
        | Some(Identifier id, stream) -> Var(id), stream
        | _ -> failwith "Unexpected token"

    let rec parsePower (stream: TokenStream) =
        let lhs, stream' = parseTerm stream
        match read stream' with
        | Some(Power, stream'') ->
            let rhs, stream''' = parsePower stream''
            Op(lhs, "^", rhs), stream'''
        | _ -> lhs, stream'

    let rec parseFactor (stream: TokenStream) =
        let lhs, stream' = parsePower stream
        match read stream' with
        | Some(Multiply, stream'') ->
            let rhs, stream''' = parseFactor stream''
            Op(lhs, "*", rhs), stream'''
        | Some(Division, stream'') ->
            let rhs, stream''' = parseFactor stream''
            Op(lhs, "/", rhs), stream'''
        | _ -> lhs, stream'

    let rec parseExpr (stream: TokenStream) =
        let lhs, stream' = parseFactor stream
        match read stream' with
        | Some(Add, stream'') ->
            let rhs, stream''' = parseExpr stream''
            Op(lhs, "+", rhs), stream'''
        | Some(Subtract, stream'') ->
            let rhs, stream''' = parseExpr stream''
            Op(lhs, "-", rhs), stream'''
        | _ -> lhs, stream'

module Exp =
    let parse (input: string) =
        input
        |> tokenize
        |> parseExpr
        |> function
            | ast, [] -> ast
            | _ -> failwith "Unexpected token"