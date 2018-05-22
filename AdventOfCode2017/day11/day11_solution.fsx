open System.IO

let inputFilename = "day11_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) =
    let text = File.ReadAllText path
    text.Split(',') |> Array.toList

// If we treat diagonal moves as 0.5 steps in each direction, we can add the absolute
// co-ordinates of the final location to get the steps required to get back to the origin.
let move direction p =
    match direction with
    | "n" -> fst p, snd p + 1.0
    | "ne" -> fst p + 0.5, snd p + 0.5
    | "se" -> fst p + 0.5, snd p - 0.5
    | "s" -> fst p, snd p - 1.0
    | "sw" -> fst p - 0.5, snd p - 0.5
    | "nw" -> fst p - 0.5, snd p + 0.5

let rec walk directions visitedPoints =
    match directions with
    | [] -> visitedPoints
    | x::xs -> walk xs (move x (List.head visitedPoints) :: visitedPoints)

let distance point = abs (fst point) + abs (snd point)

let allPoints = walk (readInput input) [(0.0, 0.0)]

// Final location is the first in the list
let part1 = allPoints |> List.head |> distance

// Furthest location is the one that's the greatest distance away
let part2 = allPoints |> List.maxBy(fun i -> distance i) |> distance

printfn "Part 1 = %i" (part1 |> int)
printfn "Part 2 = %i" (part2 |> int)
