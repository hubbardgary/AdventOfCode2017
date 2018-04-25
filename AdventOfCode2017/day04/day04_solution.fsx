open System
open System.IO

let inputFilename = "day04_input.txt"
let path = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path

let splitPassphrasesIntoWords (passphrases:string[]) = 
    [|
        for p in passphrases do
            yield p.Split()
    |]

let passphrases = splitPassphrasesIntoWords (readInput path)

// Part 1
let part1 = passphrases
            |> Array.fold (fun acc a -> if a |> Seq.distinct |> Seq.length = (a |> Seq.length) then acc + 1 else acc) 0

// Part 2
let sortedString (str:string) = str |> Seq.sort |> String.Concat
let part2 = passphrases
            |> Array.fold (fun acc a -> if a |> Array.map sortedString |> Seq.distinct |> Seq.length = (a |> Seq.length) then acc + 1 else acc) 0

printfn "Part 1 = %i" part1
printfn "Part 2 = %i" part2