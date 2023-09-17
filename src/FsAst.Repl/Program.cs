namespace FsAst.Repl
{
  using System;
  using System.Collections.Generic;
  using System.Linq;
  using System.Runtime.Serialization;
  using FsAst.Types;
  using Microsoft.FSharp.Collections;

  public static class Program
  {
    public static void Main()
    {
      var repl = new Repl();
      repl.Run();
    }
  }

  public class Repl
  {
    private readonly ReplService replService;
    private readonly ReplArgumentParser replArgumentParser;

    public Repl()
    {
      this.replService = new ReplService();
      this.replArgumentParser = new ReplArgumentParser();
    }
    
    public void Run()
    {
      Console.WriteLine("Welcome to FsAst REPL");
      Console.WriteLine("Type 'help' to see the list of available commands");
      Console.WriteLine();
      
      while (true)
      {
        Console.Write("> ");
        
        var userInput = Console.ReadLine();
        
        if (string.IsNullOrWhiteSpace(userInput))
        {
          continue;
        }
        try
        {
          PerformUserAction(userInput.Trim());
        }
        catch (Exception e)
        {
          Console.WriteLine(e.Message);
        }
      }
    }
    
    private void PerformUserAction(string input)
    {
      var userInput = input.Trim();
      if (userInput == "help")
      {
        Help();
        return;
      }
      
      var command = this.replArgumentParser.Parse(userInput.Replace(";", string.Empty).Split(' '));
      this.replService.Handle(command);
    }

    private static void Help()
    {
      Console.WriteLine();
      Console.WriteLine("Available commands");
      Console.WriteLine();
      Console.WriteLine("    set <variable> = <number>;");
      Console.WriteLine("    get <variable>;");
      Console.WriteLine("    clear;");
      Console.WriteLine("    reset;");
      Console.WriteLine("    delete <variable>;");
      Console.WriteLine("    eval <expression>;");
      Console.WriteLine("    debug;");
      Console.WriteLine("    undebug;");
      Console.WriteLine("    exit;");
      Console.WriteLine();
      return;
    }
  }

  public abstract class ReplArgument { }
  public class ClearReplArgument : ReplArgument { }
  public class ResetReplArgument : ReplArgument { }
  public class ExitReplArgument : ReplArgument { }
  public class EnableDebugReplArgument : ReplArgument { }
  public class DisableDebugReplArgument : ReplArgument { }
  
  public class SetReplArgument : ReplArgument
  {
    public string Variable { get; }
    public int Value { get; }
    
    public SetReplArgument(string variable, int value)
    {
      this.Variable = variable;
      this.Value = value;
    }
  }
  
  public class GetReplArgument : ReplArgument
  {
    public string Variable { get; }
    public GetReplArgument(string variable)
    {
      this.Variable = variable;
    }
  }
  
  public class DeleteReplArgument : ReplArgument
  {
    public string Variable { get; }
    
    public DeleteReplArgument(string variable)
    {
      this.Variable = variable;
    }
  }
  
  public class EvalReplArgument : ReplArgument
  {
    public string Expression { get; }
    public EvalReplArgument(string expression)
    {
      this.Expression = expression;
    }
  }
  
  public class ReplArgumentParser
  {
    public ReplArgument Parse(string[] input)
    {
      switch (input[0])
      {
        case "set":
          var (variable, value) = ParseSet(input);
          return new SetReplArgument(variable, value);
        case "get":
          if (input.Length != 2)
          {
            throw new ReplParsingException("Invalid number of arguments");
          }
          return new GetReplArgument(input[1]);
        case "clear":
          return new ClearReplArgument();
        case "reset":
          return new ResetReplArgument();
        case "delete":
          if (input.Length != 2)
          {
            throw new ReplParsingException("Invalid number of arguments");
          }
          return new DeleteReplArgument(input[1]);
        case "exit":
          return new ExitReplArgument();
        case "undebug":
          return new DisableDebugReplArgument();
        case "debug":
          return new EnableDebugReplArgument();
        case "eval":
          if (input.Length < 2)
          {
            throw new ReplParsingException("Invalid number of arguments");
          }
          return new EvalReplArgument(string.Join(" ", input.Skip(1)).Replace(";", string.Empty));
        default:
          throw new ReplParsingException("Input is not recognized");
      }
    }

    private (string variable, int value) ParseSet(string[] userInput)
    {
      for (var index = 0; index < userInput.Length; index++)
      {
        var token = userInput[index].Trim();
        if (token == "set")
          continue;

        if (token == ";" || token == string.Empty)
          break;
        
        var variable = userInput[index];

        if (!int.TryParse(userInput[index + 2], out var number))
        {
          throw new ReplParsingException("Invalid number");
        }

        return (variable, number);
      }
      throw new ReplParsingException("Invalid token sequence");
    }
  }

  [Serializable]
  public class ReplParsingException : Exception
  {
    public ReplParsingException(string message) : base(message)
    {
    }
    
    public ReplParsingException(string message, Exception innerException) : base(message, innerException)
    {
    }
    
    public ReplParsingException(SerializationInfo info, StreamingContext context) : base(info, context)
    {
    }
  }
  
  public class ReplService
  {
    private readonly Parser parser;
    private readonly Lexer lexer;
    private readonly Dictionary<string, int> context;
    private bool isDebugMode;
    
    public ReplService()
    {
      this.lexer = new Lexer();
      this.parser = new Parser(lexer);
      this.context = new Dictionary<string, int>();
    }

    public void Handle(ReplArgument replArgument)
    {
      switch (replArgument)
      {
        case SetReplArgument set:
          this.Set(set.Variable, set.Value);
          break;
        case GetReplArgument get:
          Get(get.Variable);
          break;
        case ClearReplArgument clear:
          this.Clear();
          break;
        case ResetReplArgument reset:
          this.Reset();
          break;
        case DeleteReplArgument delete:
          this.Delete(delete.Variable);
          break;
        case EnableDebugReplArgument _:
          this.EnableDebug();
          break;
        case DisableDebugReplArgument _:
          this.DisableDebug();
          break;
        case EvalReplArgument eval:
          this.Eval(eval.Expression);
          break;
        case ExitReplArgument exit:
          Console.WriteLine("Bye!");
          Environment.Exit(0);
          break;
        default:
          Console.WriteLine("The command is not supported");
          break;
      }
    }

    private void EnableDebug()
    {
      this.isDebugMode = true; 
      Console.WriteLine("... debug mode is enabled");
    }
    
    private void DisableDebug()
    {
      this.isDebugMode = false;
      Console.WriteLine("... debug mode is disabled");
    }
    
    private void Set(string var, int number)
    {
      this.context[var] = number; 
    }

    private void Get(string var)
    {
      if (this.context.ContainsKey(var))
      {
        Console.WriteLine(this.context[var]);
        return;
      }
      
      throw new ReplException($"The variable {var} is not defined");
    }

    private void Delete(string var)
    {
      if (!this.context.ContainsKey(var))
      {
        throw new ReplException("Cannot delete undefined variable");
      }
      
      this.context.Remove(var);
    }

    private void Reset()
    {
      ClearContext();
    }

    private void Clear()
    {
      Console.Clear();
    }

    private void Eval(string exp)
    {
      var interpreter = new Interpreter(GetCurrentContext(), this.parser);
      
      if (this.isDebugMode)
      {
        Console.WriteLine("Lex = {0}", this.lexer.Log(exp));
        Console.WriteLine("Parser = {0}", this.parser.Log(exp));
      }

      var val = interpreter.Interpret(exp);
      this.context["result"] = val; 
      Console.WriteLine(val);      
    }
    
    private void ClearContext()
    {
      this.context.Clear();
    }
    
    private FSharpMap<string, int> GetCurrentContext()
    {
      var map = new List<Tuple<string, int>>();
      foreach (var (k, v) in this.context)
      {
        map.Add(new Tuple<string, int>(k, v));
      }
      return new FSharpMap<string, int>(map);
    }
  }
  
  [Serializable]
  public class ReplException : Exception
  {
    public ReplException(string message) : base(message)
    {
    }
    
    public ReplException(string message, Exception innerException) : base(message, innerException)
    {
    }
    
    public ReplException(SerializationInfo info, StreamingContext context) : base(info, context)
    {
    }
  }
}