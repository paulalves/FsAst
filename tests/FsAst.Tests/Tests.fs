namespace FsAst

module Tests =
    open Xunit
    open FsUnit.Xunit
    open FsAst.Library
    open System.Diagnostics.CodeAnalysis
    
    let private empty = Map<string, decimal>([]);
    
    module TokenizationCases =
        let basic : obj[] seq =
            [
                yield [| "2 + 2"; [ Number 2m; Add; Number 2m ] |]
                yield [| "2 - 2"; [ Number 2m; Subtract; Number 2m ] |]
                yield [| "2 * 2"; [ Number 2m; Multiply; Number 2m ] |]
                yield [| "2 / 2"; [ Number 2m; Division; Number 2m ] |]                
            ]
        
        [<Theory>]
        [<MemberData(nameof(basic))>]
        let basicTests input expected = input |> tokenize |> should equalSeq expected
            
        let functions : obj[] seq =
            [
                yield [| "sin(0)"; [Func "sin"; OpenParen; Number 0M; CloseParen] |]
                yield [| "cos(0)"; [Func "cos"; OpenParen; Number 0M; CloseParen] |] 
                yield [| "tan(0)"; [Func "tan"; OpenParen; Number 0M; CloseParen] |]
                yield [| "log(1)"; [Func "log"; OpenParen; Number 1M; CloseParen] |]
                yield [| "sqrt(4)"; [Func "sqrt"; OpenParen; Number 4M; CloseParen] |]
                yield [| "pi"; [ Pi ] |]
                yield [| "e"; [ Euler ] |]
                yield [| "fact(5)"; [ Func "fact"; OpenParen; Number 5M; CloseParen ] |]
            ]

        [<Theory>]
        [<MemberData(nameof(functions))>]
        let functionTests input expected = input |> tokenize |> should equalSeq expected
        
        let variables : obj[] seq =
            [
                yield [| "x"; [ Identifier "x" ] |]
                yield [| "variable"; [ Identifier "variable" ] |]                
                yield [| "3x"; [ Number 3m; Multiply; Identifier "x" ] |]                
            ]
        
        [<Theory>]
        [<MemberData(nameof(variables))>]
        let variablesTests input expected = input |> tokenize |> should equalSeq expected
        
        let complex : obj[] seq =
            [
                yield [| "x ^ 2 + 1"; [ Identifier "x"; Power; Number 2m; Add; Number 1m ] |]
                yield [| "x + y ^ x * y"; [ Identifier "x"; Add; Identifier "y"; Power; Identifier "x"; Multiply; Identifier "y" ] |]                
                yield [| "x ^ 3 ^ 2"; [ Identifier "x"; Power; Number 3m; Power; Number 2m ] |]
            ]
        
        [<Theory>]
        [<MemberData(nameof(variables))>]
        let complexTests input expected = input |> tokenize |> should equalSeq expected
        
    module ParsingCases =
        let basic : obj[] seq =
            [
                yield [| "2 + 2"; Op(Const 2m, "+", Const 2m) |]
                yield [| "2 - 2"; Op(Const 2m, "-", Const 2m) |]
                yield [| "2 * 2"; Op(Const 2m, "*", Const 2m) |]
                yield [| "2 / 2"; Op(Const 2m, "/", Const 2m) |]                
            ]
        
        [<Theory>]
        [<MemberData(nameof(basic))>]
        let basicTests input expected = input |> parse |> should equal expected
        
        let functions : obj[] seq =
            [
                yield [| "sin(0)"; Fn("sin", Const 0m) |]
                yield [| "cos(0)"; Fn("cos", Const 0m) |] 
                yield [| "tan(0)"; Fn("tan", Const 0m) |]
                yield [| "log(1)"; Fn("log", Const 1m) |]
                yield [| "sqrt(4)"; Fn("sqrt", Const 4m) |]
                yield [| "fact(5)"; Fn("fact", Const 5m) |]
                yield [| "pi"; Const(decimal System.Math.PI) |]
                yield [| "e"; Const(decimal System.Math.E) |]
            ]

        [<Theory>]
        [<MemberData(nameof(functions))>]
        let functionTests input expected = input |> parse |> should equal expected
    
        let variables : obj[] seq =
            [
                yield [| "x"; Var "x" |]
                yield [| "variable"; Var "variable" |]
                yield [| "3x"; Op(Const 3m, "*", Var "x") |]
            ]
        
        [<Theory>]
        [<MemberData(nameof(variables))>]
        let variablesTests input expected = input |> parse |> should equal expected

        let complex : obj[] seq =
            [
                yield [| "x ^ 2 + 1"; Op (Op (Var "x", "^", Const 2M), "+", Const 1M) |]
                yield [| "x + y ^ x * y"; Op (Var "x", "+", Op (Op (Var "y", "^", Var "x"), "*", Var "y")) |]
                yield [| "x ^ 3 ^ 2"; Op(Var "x", "^", Op(Const 3m, "^", Const 2m)) |]
            ]
        
        [<Theory>]
        [<MemberData(nameof(complex))>]
        let complexTests input expected = input |> parse |> should equal expected
    
    module EvaluationCases =
        
        let basic : obj[] seq =
            [
                yield [| "2 + 2"; empty; 4m |]
                yield [| "2 - 2"; empty; 0m |]
                yield [| "2 * 2"; empty; 4m |]
                yield [| "2 / 2"; empty; 1m |]                
            ]
        
        [<Theory>]
        [<MemberData(nameof(basic))>]        
        let basicTests input ctx expected = evalExp ctx input |> should equal expected
        
        let functions : obj[] seq =
            [
                yield [| "sin(0)"; empty; 0m |]
                yield [| "cos(0)"; empty; 1m |] 
                yield [| "tan(0)"; empty; 0m |]
                yield [| "log(1)"; empty; 0m |]
                yield [| "sqrt(4)"; empty; 2m |]
                yield [| "fact(5)"; empty; 120m |]
                yield [| "pi"; empty; decimal System.Math.PI |]
                yield [| "e"; empty; decimal System.Math.E |]
                yield [| "sin(pi ^ 2)" ; empty; -0.430301217 |]
            ]

        [<Theory>]
        [<MemberData(nameof(functions))>]
        let functionTests input ctx expected = evalExp ctx input |> should be (equalWithin 0.00001 expected)
    
        let variables : obj[] seq =
            [
                yield [| "x"; Map.ofList [ "x", 1m ]; 1m |]
                yield [| "variable"; Map.ofList [ "variable", 2m ]; 2m |]
                yield [| "3x"; Map.ofList [ "x", 2m ]; 6m |]
            ]
        
        [<Theory>]
        [<MemberData(nameof(variables))>]
        let variablesTests input ctx expected = evalExp ctx input |> should equal expected

        let complex : obj[] seq =
            [
                yield [| "x ^ 2 + 1"; Map.ofList [ "x", 2m ]; 5m |]
                yield [| "x + y ^ x * y"; Map.ofList [ "x", 2m; "y", 3m ]; 29m |]
                yield [| "x ^ 3 ^ 2"; Map.ofList [ "x", 2m ]; 512m |]
            ]
        
        [<Theory>]
        [<MemberData(nameof(complex))>]
        let complexTests input ctx expected = evalExp ctx input |> should equal expected
        
    module PrecedenceEvalTests =

        let basic : obj[] seq =
            [
                yield [| "2 + 2 * 2"; empty; 6m |]
                yield [| "(2 + 2) * 2"; empty; 8m |]
                yield [| "((2 + 2) * 2) * (2 + 2) * 2"; empty; 64m |]
            ]

        [<Theory>]
        [<MemberData(nameof(basic))>]
        let basicTests input ctx expected = evalExp ctx input |> should equal expected
        
        let functionsPrecedences : obj[] seq =
            [
                yield [| "tan(3) * sin(0) +  cos(4)"; empty; -0.6536436209 |]
                yield [| "tan(3) * (sin(1) + cos(4))" ; empty; -0.02677414143 |]
                yield [| "tan(3) * (sin(2) + cos(4)) * (sin(1) -1)" ; empty; 0.005777204141 |]
            ]
            
        [<Theory>]
        [<MemberData(nameof(functionsPrecedences))>]
        let functionTests input ctx expected = evalExp ctx input |> should be (equalWithin 0.00001 expected)
        
        let variablesPrecedences : obj[] seq =
            [
                yield [| "x * y - z"; Map.ofList [ "x", 1m; "y", 2m; "z", 3m ]; -1m |]
                yield [| "x * (y - z)"; Map.ofList [ "x", 1m; "y", 2m; "z", 3m ]; -1m |]
                yield [| "x * (y - z) - (y - z)" ; Map.ofList [ "x", 1m; "y", 2m; "z", 3m ]; 0m |]
            ]
            
        [<Theory>]
        [<MemberData(nameof(variablesPrecedences))>]
        let variablesTests input ctx expected = evalExp ctx input |> should equal expected
        
        let complexPrecedences : obj[] seq =
            [
                yield [| "x ^ (2 + 1)"; Map.ofList [ "x", 2m ]; 8m |] 
                yield [| "x + y ^ x * y"; Map.ofList [ "x", 2m; "y", 3m ]; 29m |]
                yield [| "x ^ 3 ^ 2 - 1"; Map.ofList [ "x", 2m ]; 511m |]
                yield [| "(x ^ 3) ^ 2 * (x ^ 3) -1"; Map.ofList [ "x", 2m ]; 511m |]
            ]
        
        [<Theory>]
        [<MemberData(nameof(complexPrecedences))>]
        let complexTests input ctx expected = evalExp ctx input |> should equal expected