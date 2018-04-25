open System
open System.IO

let inputFilename = "day01_input.txt"
let input = File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, inputFilename))

let parseInt (c:char) = System.Char.GetNumericValue(c) |> int

// Part 1
let pairs l =
    let rec getPairs l a =
        match l with
        | [] -> []
        | h :: [] -> [yield h, a]
        | h :: t -> [yield h, List.head t
                     yield! getPairs t a]
    getPairs l (List.head l)

let digitsList = input
                 |> Seq.toList
                 |> List.map parseInt

let part1 = digitsList
            |> pairs
            |> List.fold (fun acc (a, b) -> if a = b then acc + a else acc) 0
             
printfn "Part 1 = %i" part1

// Part 2
let digitsArray = input
                  |> Seq.toArray
                  |> Array.map parseInt

let mid = digitsArray.Length / 2
let left, right = digitsArray |> Array.splitAt mid

let part2 = Array.zip left right
            |> Array.fold (fun acc (a,b) -> if a = b then acc + a else acc) 0
            |> (*) 2

printfn "Part 2 = %i" part2
