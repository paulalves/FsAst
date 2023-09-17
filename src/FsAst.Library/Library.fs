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
   
    let tokenize (input: string) : Token list =
        let (|OperatorTokenActivePattern|_|) ch =
            match ch with
            | "+" -> Some Add
            | "-" -> Some Subtract
            | "*" -> Some Multiply
            | "^" -> Some Power
            | "/" -> Some Division
            | _ -> None
            
        let (|NumericTokenActivePattern|_|) ch =
            if isDigit ch then Some (Number (int ch)) else None
        
        let peekNextToken currentIndex =
            let m = regex.Match(input, currentIndex)
            if not m.Success then None
            else
                match m.Groups.["token"].Value with
                | OperatorTokenActivePattern t -> Some t
                | NumericTokenActivePattern n -> Some n
                | ch -> Some (Identifier ch)

        let tokens =
            [ for x in regex.Matches(input) |> Seq.cast<Match> do
                match x.Groups.["token"].Value with
                | OperatorTokenActivePattern t -> yield t
                | NumericTokenActivePattern n -> 
                    yield n
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

[<AutoOpen>]
module Exp =
    let parseTokens tokens =
        match parseExpr tokens with
        | ast, [] -> ast
        | _ -> failwith "Unexpected token"
    
    let parse input =
        input
        |> tokenize
        |> parseTokens
            
[<AutoOpen>]            
module Eval =
    type Context = Map<string, int>
    
    exception VariableNotFoundException of string
    
    let rec private evaluate context term : int =
        match term with
        | Var v -> 
            match Map.tryFind v context with
            | Some value -> value
            | None -> raise (VariableNotFoundException v)
        | Const c -> c
        | Op(left, "+", right) -> (evaluate context left) + (evaluate context right)
        | Op(left, "-", right) -> (evaluate context left) - (evaluate context right)
        | Op(left, "*", right) -> (evaluate context left) * (evaluate context right)
        | Op(left, "/", right) -> 
            let divisor = evaluate context right
            if divisor = 0 then failwith "Division by zero"
            else (evaluate context left) / divisor
        | Op(left, "^", right) -> 
            int (System.Math.Pow(float (evaluate context left), float (evaluate context right)))
        | Op _ -> failwith "Unknown operator"
    
    let evalAst ctx term =
        ctx |> evaluate <| term
    
    let evalExp ctx exp : int =
        evalAst ctx <| parse exp