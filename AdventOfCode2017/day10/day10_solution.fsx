open System
open System.IO

let inputFilename = "day10_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)
let readInput (path:string) = File.ReadAllText path

let readLengthsAsInts input = (readInput input).Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                              |> Array.map int

let getEndIdx start size len =
    let endIdx = start + size - 1
    endIdx % len

let buildSublist(list:int[], start, size) =
    match size with
    | 0 -> [| |]
    | _ -> let stop = getEndIdx start size list.Length
           match stop < start with
           | true -> Array.concat[| list.[start..list.Length - 1] ; list.[0..stop] |]
           | _ -> list.[start..stop]

let reverseList l = l |> Array.rev

let reconstructList(list:int[], sublist:int[], startIdx) =
    let endIdx = getEndIdx startIdx sublist.Length list.Length
    match sublist.Length with
    | 0 -> list
    | 1 -> list
    | _ when list.Length = sublist.Length -> if startIdx = 0
                                             then sublist
                                             else Array.concat [| sublist.[sublist.Length - startIdx..sublist.Length - 1] ; sublist.[0..sublist.Length - startIdx - 1] |]
    | _ -> if endIdx < startIdx
           then Array.concat [| sublist.[sublist.Length - endIdx - 1..sublist.Length - 1] ; list.[endIdx + 1..startIdx - 1] ; sublist.[0..sublist.Length - endIdx - 2] |]
           else Array.concat [| list.[0..startIdx - 1] ; sublist.[0..sublist.Length - 1] ; list.[endIdx + 1..list.Length - 1] |]

let rec solvePart1(list, lengths:int[], pos, skip, i) =
    if i = lengths.Length
    then list
    else
        let sublist = buildSublist(list, pos, lengths.[i])
        let newList = reconstructList(list, (sublist |> reverseList), pos)
        solvePart1(newList, lengths, (pos + lengths.[i] + skip) % (list |> Array.length), (skip + 1), (i + 1))

let lengths = readLengthsAsInts input
let list = [| 0..255 |]
let finalList = solvePart1(list, lengths, 0, 0, 0)

printfn "Part 1 = %i" (finalList.[0] * finalList.[1])

// Part 2
let readLengthsAsAscii filename = (readInput filename).ToCharArray() |> Array.map (fun c -> c |> int)
let additionalLengths = [| 17; 31; 73; 47; 23 |]
let part2Lengths = Array.concat[| readLengthsAsAscii input; additionalLengths |]

let rec solvePart2(list:int[], lengths:int[], pos, skip, i, round) =
    if round = 64 then list
    else
        if i = lengths.Length
        then
            solvePart2(list, lengths, pos, skip, 0, round + 1)
        else
            let sublist = buildSublist(list, pos, lengths.[i])
            let newList = reconstructList(list, (sublist |> reverseList), pos)
            solvePart2(newList, lengths, (pos + lengths.[i] + skip) % (list |> Array.length), (skip + 1), (i + 1), round)

let getDenseHash sparseHash =
    let rec buildDenseHash(elements:int[], denseHash:int[]) =
        match elements.Length with
        | 0 -> denseHash
        | _ -> let first16 = elements.[0..15]
               buildDenseHash(elements.[16..(elements.Length - 1)], Array.concat [| denseHash ; [| (first16 |> Array.fold(fun s c -> s ^^^ c) 0) |] |])

    buildDenseHash(sparseHash, [| |])

let sparseHash = solvePart2(list, part2Lengths, 0, 0, 0, 0)
let denseHash = getDenseHash sparseHash
let knotHash = denseHash |> Array.fold (fun s c -> s + sprintf "%02x" c) ""

printfn "Part 2 - %s" knotHash
