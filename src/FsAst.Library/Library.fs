﻿namespace FsAst.Library

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
   
    let tokenize (input: string) : Token list =
        // todo: remember how to write active patterns and remove this little ugly thing.
        let tokenMap = 
            dict [
                ("+", Add); 
                ("-", Subtract); 
                ("*", Multiply); 
                ("^", Power); 
                ("/", Division)
            ]
        
        let peekNextToken currentIndex =
            let m = regex.Match(input, currentIndex)
            if not m.Success then None
            else
                match m.Groups.["token"].Value with
                | ch when tokenMap.ContainsKey ch -> Some (tokenMap.[ch])
                | ch when isDigit ch -> Some (Number(int ch))
                | ch -> Some (Identifier(ch))
    
        let tokens =
            [ for x in regex.Matches(input) |> Seq.cast<Match> do
                match x.Groups.["token"].Value with
                | ch when tokenMap.ContainsKey ch -> yield tokenMap.[ch]
                | ch when isDigit ch -> 
                    yield Number(int ch)
                    if peekNextToken (x.Index + x.Length)
                       |> Option.exists (function Identifier _ -> true | _ -> false) then
                        yield Multiply
                | ch -> yield Identifier(ch)
            ]
        tokens

    let private read (stream: TokenStream) =
        match stream with
        | head :: tail -> Some(head, tail)
        | _ -> None

    let rec private parseFactor (stream: TokenStream) =
        match read stream with
        | Some(Number number, stream) -> Const(number), stream
        | Some(Identifier id, stream) -> Var(id), stream
        | _ -> failwith "Unexpected token"

    let rec private parsePower (stream: TokenStream) =
        let lhs, stream' = parseFactor stream
        match read stream' with
        | Some(Power, stream'') ->
            let rhs, stream''' = parsePower stream''
            Op(lhs, "^", rhs), stream'''
        | _ -> lhs, stream'

    let rec private parseProduct (stream: TokenStream) =
        let lhs, stream' = parsePower stream
        match read stream' with
        | Some(Multiply, stream'') ->
            let rhs, stream''' = parseProduct stream''
            Op(lhs, "*", rhs), stream'''
        | Some(Division, stream'') ->
            let rhs, stream''' = parseProduct stream''
            Op(lhs, "/", rhs), stream'''
        | _ -> lhs, stream'

    let rec parseExpr (stream: TokenStream) =
        let lhs, stream' = parseProduct stream
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