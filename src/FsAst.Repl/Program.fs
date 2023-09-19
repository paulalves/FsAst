module Repl =

    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections

    type Command =
        | Set of string * decimal
        | Get of string
        | Delete of string
        | Eval of string
        | Clear
        | Reset
        | EnableDebug
        | DisableDebug
        | Exit

    let (|ReplCommand|_|) (input: string) =
        let parts = input.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) 

        match parts with
        | [| "set"; var; value |] ->
            match Decimal.TryParse(value) with
            | true, value -> Some(Set(var, value))
            | _ -> None
        | [| "get"; var |] -> Some(Get var)
        | [| "delete"; var |] -> Some(Delete var)
        | [| "clear" |] -> Some Clear
        | [| "reset" |] -> Some Reset
        | [| "debug" |] -> Some EnableDebug
        | [| "undebug" |] -> Some DisableDebug
        | [| "exit" |] -> Some Exit
        | array -> Some(Eval(String.Join(" ", array)))            

    type ReplService() =
        let mutable isDebugMode = false
        let mutable context = Dictionary<string, decimal>()

        let toMap (dict: Dictionary<string, decimal>) =
            dict |> Seq.map (fun (item) -> item.Key, item.Value) |> Map.ofSeq

        member this.HandleCommand cmd =
            match cmd with
            | Set(var, value) ->
                context.[var] <- value
                printfn "Set %s to %f" var value
            | Get var ->
                match context.TryGetValue(var) with
                | true, value -> printfn "%f" value
                | _ -> printfn "Error: Variable %s not found." var
            | Delete var ->
                if context.Remove(var) then
                    printfn "Deleted %s" var
                else
                    printfn "Error: Variable %s not found." var
            | Clear -> Console.Clear()
            | Reset -> context.Clear()
            | EnableDebug ->
                isDebugMode <- true
                printfn "...debug mode enabled."
            | DisableDebug ->
                isDebugMode <- false
                printfn "...debug mode disabled."
            | Eval exp ->
                let tokens = exp |> FsAst.Library.Parser.tokenize
                let ast, _ = tokens |> FsAst.Library.Parser.parseExpr
                if isDebugMode then
                    printfn "Tokens: %A" tokens
                    printfn "AST: %A" ast
                (toMap context) |> FsAst.Library.Eval.evalAst <| ast |>
                    fun r ->
                        if r % 1m = 0m then
                            printfn "%i" (int r)
                        else printfn "%0.2f" r
                        context.["result"] <- r
            | Exit -> System.Environment.Exit(0)

    let run () =
        let service = ReplService()

        printfn "Welcome to the REPL. Type 'help' for a list of commands."

        let rec loop () =
            printf "> "            
            let input = Console.ReadLine().Trim()
            match input with
            | "help" ->
                printfn "\n"
                printfn "Available commands:"
                printfn "\n"
                printfn "- set [var] [value]:.............Set a variable to a value."
                printfn "- get [var]:.....................Get the value of a variable."
                printfn "- delete [var]:..................Delete a variable."
                printfn "- eval [expression]:.............Evaluate an expression."
                printfn "- clear:.........................Clear the console."
                printfn "- reset:.........................Reset the REPL context."
                printfn "- debug:.........................Enable debug mode."
                printfn "- undebug:.......................Disable debug mode."
                printfn "- exit: Exit the REPL."
                printfn "\n"
                loop ()
            | input ->
                match input with
                | ReplCommand cmd -> service.HandleCommand cmd
                | _ -> printfn "Error: Invalid command."
                loop ()
        loop ()

    [<EntryPoint>]
    let main _ =
        run ()
        0
