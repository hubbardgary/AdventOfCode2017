open System.IO

let inputFilename = "day05_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) =
    let lines = File.ReadAllLines input
    [|
        for l in lines do
            yield l |> int
    |]

let solvePart1 = 
    let instructions = readInput inputFilename
    let rec solve (index,steps:int) =
        match index with
        | i when i < (instructions |> Array.length) ->
            let jump = instructions.[index]
            instructions.[index] <- instructions.[index] + 1
            solve ((index + jump),(steps + 1))
        | _ -> steps
    solve (0,0)

printfn "Part 1 = %i" solvePart1

let solvePart2 =
    let instructions = readInput inputFilename
    let rec solve (index,steps:int) =
        match index with
        | i when i < (instructions |> Array.length) ->
            let jump = instructions.[index]
            
            match jump with
            | j when j >= 3 -> instructions.[index] <- instructions.[index] - 1
            | _ -> instructions.[index] <- instructions.[index] + 1
            
            solve ((index + jump),(steps + 1))
        | _ -> steps
    solve (0,0)

printfn "Part 2 = %i" solvePart2
