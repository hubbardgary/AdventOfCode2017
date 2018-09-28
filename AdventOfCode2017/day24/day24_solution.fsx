open System.IO

let inputFilename = "day24_input.txt"
let path = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path

let processInput (input:string[]) = 
    [
        for item in input do
            let ports = item.Split('/')
            yield ports.[0] |> int, ports.[1] |> int
    ]

let buildBridges components =
    let allBridges = new ResizeArray<(int*int) list>()
    
    let rec growBridge components currentBridge nextPort =
        let possibles = components |> List.filter (fun x -> fst x = nextPort || snd x = nextPort)
        if possibles = [] then allBridges.Add currentBridge
        for i in 0..(possibles |> List.length) - 1 do
            let next = possibles.[i]
            growBridge (components |> List.filter (fun x -> x <> next)) (currentBridge @ [next]) (if fst next = nextPort then snd next else fst next)
    
    let startPieces = components |> List.filter (fun x -> fst x = 0 || snd x = 0)
    
    for i in 0..(List.length startPieces) - 1 do
        //printfn "Finding bridges starting with component %i" i
        let startPiece = startPieces.[i]
        growBridge (components |> List.filter (fun x -> x <> startPiece)) [startPiece] (if fst startPiece = 0 then snd startPiece else fst startPiece)
    
    allBridges

let components = processInput (readInput path)
let bridges = buildBridges components

let part1 = bridges 
            |> Seq.map (fun a -> (a |> List.sumBy(fun (x:int*int) -> fst x + snd x)))
            |> Seq.max
printfn "Part 1 = %i" part1

let part2 = bridges
            |> Seq.map (fun a -> (a |> List.length), (a |> List.sumBy(fun (x:int*int) -> fst x + snd x)))
            |> Seq.sortBy (fun x -> -(fst x), -(snd x))
            |> Seq.head
printfn "Part 2 = %i" (snd part2)
