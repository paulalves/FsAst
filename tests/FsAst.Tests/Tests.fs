namespace FsAst

module Tests =
    open System.Diagnostics.CodeAnalysis
    open Xunit
    open FsUnit.Xunit
    open FsAst.Library

    let validTokenizationSamples: obj[] seq =
        [ 
          yield [| "5"; [| Number 5 |] |]          
          yield [| "3x"; [| Number 3; Multiply; Identifier "x" |] |]
          yield [| "x ^ 2 + 1"; [| Identifier "x"; Power; Number 2; Add; Number 1 |] |]
          yield [| "x + y ^ x * y"; [| Identifier "x"; Add; Identifier "y"; Power; Identifier "x"; Multiply; Identifier "y" |] |]          
          yield [| "x ^ 2 + 3x - 5"; [| Identifier "x"; Power; Number 2; Add; Number 3; Multiply; Identifier "x"; Subtract; Number 5 |] |]
          yield [| "x ^ 3 ^ 2"; [| Identifier "x"; Power; Number 3; Power; Number 2 |] |]
        ]

    let validParsingSamples: obj[] seq =
        [
          yield [| "x"; Var "x" |]
          yield [| "variable"; Var "variable" |]
          yield [| "3items"; Op(Const 3, "*", Var "items") |]
          yield [| "3x"; Op(Const 3, "*", Var "x") |]
          yield [| "42"; Const 42 |]
          yield [| "x ^ 3"; Op(Var "x", "^", Const 3) |]
          yield [| "x + y"; Op(Var "x", "+", Var "y") |]
          yield [| "x - y"; Op(Var "x", "-", Var "y") |]
          yield [| "x * y"; Op(Var "x", "*", Var "y") |]
          yield [| "x / y"; Op(Var "x", "/", Var "y") |]
          yield [| "3x + y"; Op(Op(Const 3, "*", Var "x"), "+", Var "y") |]
          yield [| "x ^ 2 + y"; Op(Op(Var "x", "^", Const 2), "+", Var "y") |]
          yield [| "2 + 1 * 3"; Op(Const 2, "+", Op(Const 1, "*", Const 3)) |]
          yield [| "x ^ 3 ^ 2"; Op(Var "x", "^", Op(Const 3, "^", Const 2)) |]          
          yield [| "2x * 3y + z"; Op(Op(Const 2, "*", Op(Var "x", "*", Op(Const 3, "*", Var "y"))), "+", Var "z") |]
        ]
    
    let expressionSamples : obj[] seq =
        [
            yield [| "2"; Map<string, int>([]); 2 |]
            yield [| "2 + 2"; Map<string, int>([]); 4 |]
            yield [| "2 - 2"; Map<string, int>([]); 0 |]
            yield [| "2 * 2"; Map<string, int>([]); 4 |]
            yield [| "2 / 2"; Map<string, int>([]); 1 |]            
            yield [| "2 + 2 * 2"; Map<string, int>([]); 6 |]
            yield [| "x + y * 3"; Map.ofList ["x", 1; "y", 2]; 7 |]            
            yield [| "x ^ 2 + 3x - 5"; Map.ofList ["x", 2]; 5 |]            
        ]
    
    [<Theory>]
    [<ExcludeFromCodeCoverage>]
    [<MemberData(nameof (validTokenizationSamples))>]
    let ``when received valid input it should tokenize it`` input expected =
        input
        |> tokenize
        |> should equalSeq expected

    [<Theory>]
    [<ExcludeFromCodeCoverage>]
    [<MemberData(nameof (validParsingSamples))>]
    let ``when received valid input it should parse it`` input expected =
        input
        |> parse
        |> should equal expected
        
    [<Theory>]
    [<ExcludeFromCodeCoverage>]
    [<MemberData(nameof (expressionSamples))>]
    let ``when received valid input it should evaluate it`` (input : string) (context : Map<string, int>) (expected: int) =
        input
        |> evalExp context            
        |> should equal expected