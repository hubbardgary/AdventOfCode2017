open System.IO

let inputFilename = "day02_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) =
    let lines = File.ReadAllLines(input)
    [|
        for l in lines do
            yield l.Split() |> Array.map int
    |]

let inputArray = readInput inputFilename

let part1 = inputArray
            |> Array.fold (fun acc row -> Array.max row - Array.min row + acc) 0

printfn "Part 1 = %i" part1

let part2 = [| for row in inputArray do
                 for x in row do
                   for y in row do
                     if x <> y && x % y = 0 then yield x / y 
            |]
            |> Array.sum

printfn "Part 2 = %i" part2
