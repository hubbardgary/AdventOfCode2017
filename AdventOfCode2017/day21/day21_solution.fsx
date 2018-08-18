open System
open System.IO
open System.Text

let inputFilename = "day21_input.txt"
let inputPath = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)
let readInput (path:string) = File.ReadAllLines path
let input = readInput inputPath

let flatten (pattern:string[]) = String.Join("/", pattern)
let unflatten (pattern:string) = pattern.Split([| '/' |], StringSplitOptions.RemoveEmptyEntries)

type TransformationType = Rotate90 | Rotate180 | Rotate270 | FlipH | FlipV

let transform2DArray transformation (pattern:string[]) =
    let transform (pattern:string[]) size x y transformation =
        match transformation with
        | Rotate90 -> pattern.[size-y].[x]
        | Rotate180 -> pattern.[size-x].[size-y]
        | Rotate270 -> pattern.[y].[size-x]
        | FlipH -> pattern.[size-x].[y]
        | FlipV -> pattern.[x].[size-y]
    
    let size = (pattern |> Array.length) - 1
    [|
        for x in 0..size do
            let chars = [|
                for y in 0..size do
                    yield transform pattern size x y transformation
            |]
            yield String.Join("", chars)
    |]

let splitInto2x2Arrays (a:string[]) =
    let size = a |> Array.length
    [
        for x in 0..2..size-1 do
            for y in 0..2..size-1 do
                yield [|
                    String.Concat [a.[x].[y]; a.[x].[y+1]];
                    String.Concat [a.[x+1].[y]; a.[x+1].[y+1]];
                |]
    ]

let splitInto3x3Arrays (a:string[]) =
    let size = a |> Array.length
    [
        for x in 0..3..size-1 do
            for y in 0..3..size-1 do
                yield [|
                    String.Concat [a.[x].[y]; a.[x].[y+1]; a.[x].[y+2]];
                    String.Concat [a.[x+1].[y]; a.[x+1].[y+1]; a.[x+1].[y+2]];
                    String.Concat [a.[x+2].[y]; a.[x+2].[y+1]; a.[x+2].[y+2]]
                |]
    ]

let joinSquares (squares:string[] list) =
    let squareSize = squares |> List.length
    let patternSize = squares.[0] |> Array.length
    let blocks = ((sqrt (squareSize |> float)) |> int)
    [|
        for x in 0..blocks..squareSize-1 do
            for y in 0..patternSize-1 do
                let sb = new StringBuilder("")
                for z in 0..blocks-1 do
                    sb.Append(squares.[x+z].[y])
                yield sb.ToString()
    |]

let buildPatternMap (input:string list) = 
    let rec addMap (input:string list) m =
        match input with
        | [] -> m
        | x::xs ->
            let item = x.Split([| " => " |], StringSplitOptions.RemoveEmptyEntries)
            addMap xs (m |> Map.add item.[0] item.[1]
                         |> Map.add (flatten (transform2DArray FlipV (unflatten item.[0]))) item.[1]
                         |> Map.add (flatten (transform2DArray Rotate90 (unflatten item.[0]))) item.[1]
                         |> Map.add (flatten (transform2DArray FlipV (transform2DArray Rotate90 (unflatten item.[0])))) item.[1]
                         |> Map.add (flatten (transform2DArray Rotate180 (unflatten item.[0]))) item.[1]
                         |> Map.add (flatten (transform2DArray FlipV (transform2DArray Rotate180 (unflatten item.[0])))) item.[1]
                         |> Map.add (flatten (transform2DArray Rotate270 (unflatten item.[0]))) item.[1]
                         |> Map.add (flatten (transform2DArray FlipV (transform2DArray Rotate270 (unflatten item.[0])))) item.[1])
    addMap input Map.empty

let start = [|
        ".#.";
        "..#";
        "###"
    |]

let rules = buildPatternMap (input |> Array.toList)

let countCharacters listOfPatterns =
    listOfPatterns
    |> List.sumBy (fun l -> (l |> Array.sumBy (fun a -> (a |> String.collect(fun c -> if c = '#' then "#" else "") |> String.length))))

let rec enhance (patterns:string[] list) (newPatterns:string[] list) i =
    let addPatterns (pattern:string[]) (newPatterns:string[] list) =
        let size = pattern |> Array.length
        let enhanced = rules.[flatten pattern]
        let updatedList = List.append newPatterns [(unflatten enhanced)]
        updatedList

    match i with
    | 0 -> patterns
    | _ -> match patterns with
           | [] -> let joined = joinSquares newPatterns
                   let size = joined |> Array.length
                   if size % 2 = 0 then enhance (splitInto2x2Arrays(joined)) [] (i-1) else enhance (splitInto3x3Arrays(joined)) [] (i-1)
           | x::xs -> enhance xs (addPatterns x newPatterns) i

let part1 = countCharacters (enhance [start] [] 5)
printfn "Part 1 = %i" part1

let part2 = countCharacters (enhance [start] [] 18)
printfn "Part 2 = %i" part2
