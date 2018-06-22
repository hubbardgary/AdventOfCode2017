open System.IO

let inputFilename = "day16_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)
let readInput (path:string) = (File.ReadAllText input).Split(',')

let spin str x =
    let len = str |> String.length
    let start = len - x
    str.[start..] + str.[0..start-1]

let exchange (str:string) (a:int) (b:int) =
    let charA = str.[a]
    let charB = str.[b]
    str |> String.map (fun x -> if x = charA then charB else if x = charB then charA else x)

let partner (str:string) (a:char) (b:char) =
    str |> String.map (fun x -> if x = a then b else if x = b then a else x)

let rec performDance str moves i =
    let performMove str (move:string) =
        match move.[0] with
        | 's' -> spin str (move.[1..] |> int)
        | 'x' -> let pos = (move.[1..]).Split('/') 
                           |> Array.map int
                 exchange str pos.[0] pos.[1]
        | 'p' -> let chars = (move.[1..]).Split('/')
                             |> Array.map char
                 partner str chars.[0] chars.[1]
        
    if i = (moves |> Array.length) then str
    else
        let state = performMove str moves.[i]
        performDance state moves (i+1)

let rec performDanceRoutine str moves seen round totalRounds =
    if round = totalRounds then str
    else
        if seen |> Array.contains str then
            // This is the point at which the patterns start repeating
            let repetitionCycle = seen |> Array.length
            let endState = totalRounds % repetitionCycle
            seen.[endState]
        else
            performDanceRoutine (performDance str moves 0) moves (Array.append seen [| str |]) (round+1) totalRounds


let moves = readInput inputFilename
let chars = "abcdefghijklmnop"

let part1 = performDance chars moves 0
printfn "Part 1 = %s" part1

let part2 = performDanceRoutine chars moves Array.empty 0 1_000_000_000
printfn "Part 2 = %s" part2