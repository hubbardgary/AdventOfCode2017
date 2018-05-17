open System.IO
open System.Collections.Generic
open System.Linq
open System.Text.RegularExpressions

let inputFilename = "day08_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path

type instruction(register, operation, amount, conditionReg, comparison, comparisonAmount) =
    member x.Regsiter = register
    member x.Operation = match operation with
                         | "inc" -> (+)
                         | _ -> (-)
    member x.Amount = amount |> int
    member x.ConditionReg = conditionReg
    member x.Comparison = match comparison with
                          | ">" -> (>)
                          | ">=" -> (>=)
                          | "<" -> (<)
                          | "<=" -> (<=)
                          | "==" -> (=)
                          | "!=" -> (<>)
    member x.ComparisonAmount = comparisonAmount |> int

let pattern = @"(?<register>[a-z]*) (?<operation>[a-z]*) (?<amount>-?\d*) if (?<condition_reg>[a-z]*) (?<comparison>[><!=]+) (?<comparison_amount>-?\d*)";

let mutable register = new Dictionary<string, int>()

let initializeRegisters registers =
    registers
    |> Array.map(fun x -> 
        match register.ContainsKey x with
        | false -> register.Add(x, 0)
        | _ -> ())

let evaluate(operator, operand1, operand2) =
    operator operand1 operand2

let mutable overallHighest = 0

let instructions = readInput input
for i in [0..instructions.Length - 1] do
    let matches = Regex.Match(instructions.[i], pattern)
    let ins = new instruction(matches.Groups.Item("register").Value, 
                              matches.Groups.Item("operation").Value, 
                              matches.Groups.Item("amount").Value, 
                              matches.Groups.Item("condition_reg").Value,
                              matches.Groups.Item("comparison").Value,
                              matches.Groups.Item("comparison_amount").Value)
    
    initializeRegisters [| ins.Regsiter; ins.ConditionReg |] |> ignore

    match evaluate(ins.Comparison, register.[ins.ConditionReg], (ins.ComparisonAmount)) with
    | true -> register.[ins.Regsiter] <- ins.Operation register.[ins.Regsiter] ins.Amount
    | false -> ()

    overallHighest <- [| overallHighest; register.Values.Max() |].Max()
    
printfn "Part 1 = %i" (register.Values.Max())
printfn "Part 2 = %i" overallHighest
