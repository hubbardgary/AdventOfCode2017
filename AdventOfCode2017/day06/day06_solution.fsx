open System
open System.IO
open System.Linq

let inputFilename = "day06_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) =
    let text = File.ReadAllText path
    text.Split() |> Array.map int

let rec redistribute (banks:int[], index, amountToRedistribute) =
    match amountToRedistribute with
    | 0 -> banks
    | _ -> banks.[index] <- banks.[index] + 1
           redistribute (banks, (if index >= banks.Count() - 1 then 0 else index + 1), (amountToRedistribute - 1))

let rec solve (banks:int[], previous:ResizeArray<string>) =
    match previous with
    | p when p.Contains(String.Join(",", banks)) -> p.Count
    | _ -> previous.Add(String.Join(",", banks))
           let chosen = banks
                        |> Array.mapi (fun index item -> (index, item))
                        |> Array.maxBy (fun (_, value) -> value)
           let amount = snd chosen
           let index = if fst chosen = banks.Length - 1 then 0 else fst chosen + 1
           banks.[fst chosen] <- 0
           solve ((redistribute (banks, index, amount)), previous)

let banks = readInput input
let part1 = solve (banks, new ResizeArray<string>())
let part2 = solve (banks, new ResizeArray<string>())

printfn "Part 2 = %i" part1
printfn "Part 2 = %i" part2