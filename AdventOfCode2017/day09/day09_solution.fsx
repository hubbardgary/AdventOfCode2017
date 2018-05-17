open System.IO

let inputFilename = "day09_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput path:string = File.ReadAllText path

let chars = [ for c in (readInput input) do yield c ]

let rec solvePart1 l escape garbage nestLevel score =
    match l with
    | [] -> score
    | x::xs -> match x with
               | '{' when not garbage -> solvePart1 xs false garbage (nestLevel + 1) (score + nestLevel)
               | '}' when not garbage -> solvePart1 xs false garbage (nestLevel - 1) score
               | '<' when not escape -> solvePart1 xs false true nestLevel score
               | '>' when not escape -> solvePart1 xs false false nestLevel score
               | '!' -> solvePart1 xs (not escape) garbage nestLevel score
               | _ -> solvePart1 xs false garbage nestLevel score

let part1 = solvePart1 chars false false 1 0
printfn "Part 1 = %i" part1

// Part 2
let rec solvePart2 l escape garbage score =
    match l with
    | [] -> score
    | x::xs -> match x with
               | _ when escape -> solvePart2 xs false garbage score
               | '!' -> solvePart2 xs true garbage score
               | '>' -> solvePart2 xs false false score
               | _ when garbage -> solvePart2 xs false true (score + 1)
               | '<' -> solvePart2 xs false true score
               | _ -> solvePart2 xs false garbage score
               
let part2 = solvePart2 chars false false 0
printfn "Part 2 = %i" part2
