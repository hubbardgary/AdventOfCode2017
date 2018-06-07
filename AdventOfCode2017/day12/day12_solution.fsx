open System.IO
open System.Text.RegularExpressions

let inputFilename = "day12_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path

let inputMappings = (readInput input) |> Array.toList

let buildMap (mappings:string list):Map<int, int list> =
    let rec addMap (mapping:string list, m:Map<int, int list>) =
        match mapping with
        | [] -> m
        | x::xs -> let pattern = @"(?<pipe>\d+) <-> (?<conn>.*)"
                   let matches = Regex.Match(x, pattern)
                   let pipe = (matches.Groups.Item("pipe").Value) |> int
                   let conns = matches.Groups.Item("conn").Value
                   addMap (xs, (m |> Map.add pipe (conns.Split(',')
                                  |> Array.map(fun c -> c.Trim())
                                  |> Array.map int
                                  |> Array.toList)))
    addMap (mappings, Map.empty)

let mappings = buildMap (inputMappings)

let rec walkMap connections current = 
    if Set.contains current connections then
        connections
    else
        List.fold walkMap (connections.Add current) mappings.[current]

let part1 = walkMap Set.empty 0 |> Set.count

// Part 2
let programIds = [0..1999]

let rec countGroups knownGroups roots total =
    match roots with
    | [] -> total
    | x::xs -> if knownGroups |> Set.contains x
               then countGroups knownGroups xs total
               else let group = walkMap Set.empty x
                    countGroups (Set.union knownGroups group) xs (total + 1)

let part2 = countGroups Set.empty programIds 0

printfn "Part 1 = %i" part1
printfn "Part 2 = %i" part2
    