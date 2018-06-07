open System.IO

let inputFilename = "day13_input.txt"
let path = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path
let input = (readInput path) |> Array.toList

let firewallMap input =
    let rec addLayer (layers, firewall:Map<int, int>) =
        match layers:string list with
        | [] -> firewall
        | x::xs -> let layer = x.Split(':')
                               |> Array.map (fun x -> x.Trim())
                               |> Array.map int
                   addLayer (xs, (firewall |> Map.add layer.[0] layer.[1]))
    addLayer (input, Map.empty)
        
// Part 1
let firewall = firewallMap input
let calculateSeverity f = f |> Map.filter (fun k v -> k = 0 || v < 2 || (k % (v * 2 - 2) = 0))
                            |> Map.fold (fun acc k v -> k * v + acc) 0

let part1 = calculateSeverity firewall
printfn "Part 1 = %i" part1

// Part 2
let delayByOnePicosecond map:Map<int, int> =
    map |> Map.fold (fun m k v -> m.Add(k + 1, v)) Map.empty

let rec solvePart2 firewall =
    let severity = calculateSeverity firewall
    if severity = 0 then firewall
    else solvePart2 (delayByOnePicosecond firewall)

let zeroSeverityMap = solvePart2 firewall
let part2 = zeroSeverityMap |> Map.toSeq |> Seq.head |> fst
printfn "Part 2 = %i" part2
