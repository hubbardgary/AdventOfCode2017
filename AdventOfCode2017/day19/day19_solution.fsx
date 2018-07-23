open System.IO

let inputFilename = "day19_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)
let readInput (path:string) = File.ReadAllLines path

type direction = North | East | South | West
let route = readInput input
let startIdx = route.[0].IndexOf("|")
let location = 0, startIdx

let rec followPath (route:string[]) location direction visited steps =
    let currentChar = route.[fst location].[snd location]
    
    match currentChar with
    | ' ' -> visited, steps
    | _ -> let newDirection = match currentChar with
                              | '+' when direction = North || direction = South -> if snd location - 1 > -1 && route.[fst location].[snd location - 1] = ' ' then East else West
                              | '+' when direction = East || direction = West -> if fst location - 1 > -1 && route.[fst location - 1].[snd location] = ' ' then South else North
                              | _ -> direction
                       
           let newLocation = match newDirection with
                             | North -> fst location - 1, snd location
                             | South -> fst location + 1, snd location
                             | East -> fst location, snd location + 1
                             | West -> fst location, snd location - 1
           
           let collectedChars = match currentChar with
                                | c when c = '-' || c = '|' || c = '+' -> visited
                                | _ -> visited + currentChar.ToString()
           
           followPath route newLocation newDirection collectedChars (steps + 1)

let solution = followPath route location South "" 0

printfn "Part 1 = %s" (fst solution)
printfn "Part 2 = %i" (snd solution)
