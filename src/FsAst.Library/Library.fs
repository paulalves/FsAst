(* grammar *)

(**********************************************
expr       = term, { ("+" , term) | ("-" , term) };
term       = factor, { ("*" , factor) | ("/" , factor) };
factor     = power, { "^", power };
power      = number | identifier | func | "(", expr, ")" | "pi";
func       = "sin" | "cos" | "tan" | "log" | "fact" | "sqrt" | "sqrtpi" | "ceiling" | "floor", "(", expr, ")";
number     = ["+" | "-"], digit, {digit}, ["." , {digit}];
identifier = letter, { letter | digit };
digit      = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
letter     = "a" | "b" | "c" | ... | "z" ;
**********************************************)

namespace FsAst.Library

[<AutoOpen>]
module public Ast =
    type Token =
        | Identifier of string
        | Number of decimal
        | Add
        | Subtract
        | Power
        | Multiply
        | Division
        | Func of string
        | OpenParen
        | CloseParen
        | Pi
        | Euler
    
    type Term =
        | Var of string
        | Const of decimal
        | Op of Term * string * Term
        | Fn of string * Term

    type TokenStream = Token list
    
[<AutoOpen>]
module internal Parser =
    open System.Text.RegularExpressions

    let private numberRegex = Regex @"(?<number>(?<sign>\+|\-)?(?<int>\d+)(?<dec>\.\d+)?)" 
    
    let private tokensRegex = Regex @"(?<token>pi|e|sin|cos|tan|log|fact|sqrt|sqrtpi|ceiling|floor|\d*\.?\d+|[a-zA-Z]+|\+|\-|\*|\/|\^|\(|\))"

    let private isDigit (ch: string) = ch.[0] |> System.Char.IsDigit
    
    let private tryParseNumber (ch: string) =
        let mutable value = 0m
        if System.Decimal.TryParse(ch, &value) then Some (Number value) else None
    
    let private readNextToken (stream: TokenStream) =
        match stream with
        | head :: tail -> Some(head, tail)
        | _ -> None

    let tokenize (input: string) : Token list =

        (* active patterns *)
        let (|PiTokenActivePattern|_|) (ch: string) =
            if ch = "pi" then Some Pi else None
        
        let (|EulerTokenActivePattern|_|) (ch: string) =
            if ch = "e" then Some Euler else None
            
        let (|FunctionTokenActivePattern|_|) ch =
            match ch with
            | "sin" | "cos" | "tan" | "log" | "fact" | "sqrt" | "sqrtpi" | "ceiling" | "floor" -> Some (Func ch)    
            | _ -> None

        let (|OperatorTokenActivePattern|_|) ch =
            match ch with
            | "+" -> Some Add
            | "-" -> Some Subtract
            | "*" -> Some Multiply
            | "^" -> Some Power
            | "/" -> Some Division
            | "(" -> Some OpenParen
            | ")" -> Some CloseParen
            | _ -> None
        
        let (|NumericTokenActivePattern|_|) (ch: string) : Token option =
            numberRegex.IsMatch(ch)
                |> function true -> tryParseNumber ch | _ -> None            

        (* tokenization functions *)
        let peekNextToken currentIndex =
            let m = tokensRegex.Match(input, currentIndex)
            if not m.Success then None
            else
                match m.Groups.["token"].Value with
                | PiTokenActivePattern p -> Some p
                | EulerTokenActivePattern e -> Some e
                | FunctionTokenActivePattern t -> Some t
                | OperatorTokenActivePattern t -> Some t
                | NumericTokenActivePattern n -> Some n                
                | ch -> Some (Identifier ch)
    
        let tokens =
            [ for x in tokensRegex.Matches(input) |> Seq.cast<Match> do
                match x.Groups.["token"].Value with
                | PiTokenActivePattern p -> yield p
                | EulerTokenActivePattern e -> yield e
                | FunctionTokenActivePattern t -> yield t
                | OperatorTokenActivePattern t -> yield t
                | NumericTokenActivePattern n -> 
                    yield n
                    if peekNextToken (x.Index + x.Length)
                       |> Option.exists (function Identifier _ -> true | _ -> false) then
                        yield Multiply
                | ch -> yield Identifier(ch)                
            ]
        tokens             
    
    let rec private parseFactor (stream: TokenStream) =
        match readNextToken stream with
        | Some(Number number, stream) -> Const(number), stream
        | Some(Identifier id, stream) -> Var(id), stream
        | Some(Func f, stream) ->
            match readNextToken stream with
            | Some(OpenParen, stream) ->
                let arg, stream' = parseExpr stream
                match readNextToken stream' with
                | Some(CloseParen, stream'') -> Fn(f, arg), stream''
                | _ -> failwith "Expected closing parenthesis"
            | _ -> failwith "Expected opening parenthesis after function"
        | Some(OpenParen, _) -> 
            let term, stream' = parseParen stream
            term, stream'
        | Some(Pi, stream) -> Const (decimal System.Math.PI), stream
        | Some(Euler, stream) -> Const (decimal System.Math.E), stream
        | _ -> failwith "Unexpected token"

    and private parsePower (stream: TokenStream) =
        let lhs, stream' = parseFactor stream
        match readNextToken stream' with
        | Some(Power, stream'') ->
            let rhs, stream''' = parsePower stream''
            Op(lhs, "^", rhs), stream'''
        | _ -> lhs, stream'

    and private parseProduct (stream: TokenStream) =
        let lhs, stream' = parsePower stream
        match readNextToken stream' with
        | Some(Multiply, stream'') ->
            let rhs, stream''' = parseProduct stream''
            Op(lhs, "*", rhs), stream'''
        | Some(Division, stream'') ->
            let rhs, stream''' = parseProduct stream''
            Op(lhs, "/", rhs), stream'''
        | _ -> lhs, stream'

    and parseExpr (stream: TokenStream) =
        let lhs, stream' = parseProduct stream
        match readNextToken stream' with
        | Some(Add, stream'') ->
            let rhs, stream''' = parseExpr stream''
            Op(lhs, "+", rhs), stream'''
        | Some(Subtract, stream'') ->
            let rhs, stream''' = parseExpr stream''
            Op(lhs, "-", rhs), stream'''
        | _ -> lhs, stream'
        
    and private parseParen (stream: TokenStream) =
        match readNextToken stream with
        | Some(OpenParen, stream) ->
            let term, stream' = parseExpr stream
            match readNextToken stream' with
            | Some(CloseParen, stream'') -> term, stream''
            | _ -> failwith "Expected closing parenthesis"
        | _ -> failwith "Expected opening parenthesis"
        
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

    type Context = Map<string, decimal>
    
    let rec private factorial n = 
        if n <= 1m then 1m else n * factorial (n - 1m)
    
    let private pow x y = decimal (System.Math.Pow(float x, float y))
    
    let private divide (x: decimal) (y: decimal) = 
        if y = 0m then failwith "Division by zero"
        else x / y
    
    let rec private evaluate context term : decimal =
        match term with
        | Var v -> 
            match Map.tryFind v context with
            | Some value -> value
            | None -> failwith (sprintf "Variable %s not found" v)
        | Const c -> c
        | Op(left, "+", right) -> (evaluate context left) + (evaluate context right)
        | Op(left, "-", right) -> (evaluate context left) - (evaluate context right)
        | Op(left, "*", right) -> (evaluate context left) * (evaluate context right)
        | Op(left, "/", right) -> divide (evaluate context left) (evaluate context right)
        | Op(left, "^", right) -> pow (evaluate context left) (evaluate context right)
        | Fn("sin", x) -> decimal (sin(float (evaluate context x)))
        | Fn("cos", x) -> decimal (cos(float (evaluate context x)))
        | Fn("tan", x) -> decimal (tan(float (evaluate context x)))
        | Fn("log", x) -> decimal (log(float (evaluate context x)))
        | Fn("sqrt", x) -> decimal (sqrt(float (evaluate context x)))
        | Fn("sqrtpi", x) -> decimal (sqrt(System.Math.PI * float (evaluate context x)))
        | Fn("ceiling", x) -> decimal (ceil(float (evaluate context x)))
        | Fn("floor", x) -> decimal (floor(float (evaluate context x)))
        | Fn("fact", x) -> factorial (evaluate context x)
        | Fn(name,_) -> failwith (sprintf "Unknown function: %s" name)            
        | Op(_, op, _) -> failwith (sprintf "Unknown operator: %s" op)
    
    let evalAst ctx term =
        ctx |> evaluate <| term
    
    let evalExp ctx exp =
        ctx |> evaluate <| (parse exp)