//
// Knot hash functions from day 10 part 2
//
let getKnotHash input =
    let getEndIdx start  size  len =
        let endIdx = start + size - 1
        endIdx % len
    
    let rec getSparseHash(list:int[], lengths:int[], pos, skip, i, round) =
    
        let buildSublist list start size =
            match size with
            | 0 -> [||]
            | _ -> let listLen = list |> Array.length
                   let stop = getEndIdx start  size  listLen
                   match stop < start with
                   | true -> Array.concat[| list.[start..listLen - 1] ; list.[0..stop] |]
                   | _ -> list.[start..stop]
                   
        let reconstructList list sublist startIdx =
            let listLen = list |> Array.length
            let subLen = sublist |> Array.length
            let endIdx = getEndIdx startIdx subLen listLen
            match subLen with
            | 0 -> list
            | 1 -> list
            | _ when listLen = subLen -> if startIdx = 0
                                         then sublist
                                         else Array.concat [| sublist.[subLen - startIdx..subLen - 1] ; sublist.[0..subLen - startIdx - 1] |]
            | _ -> if endIdx < startIdx
                   then Array.concat [| sublist.[subLen - endIdx - 1..subLen - 1] ; list.[endIdx + 1..startIdx - 1] ; sublist.[0..subLen - endIdx - 2] |]
                   else Array.concat [| list.[0..startIdx - 1] ; sublist.[0..subLen - 1] ; list.[endIdx + 1..listLen - 1] |]
                   
        if round = 64 then list
        else
            if i = (lengths |> Array.length)
            then
                getSparseHash(list, lengths, pos, skip, 0, round + 1)
            else
                let sublist = buildSublist list pos lengths.[i]
                let newList = reconstructList list (sublist |> Array.rev) pos
                getSparseHash(newList, lengths, (pos + lengths.[i] + skip) % (list |> Array.length), (skip + 1), (i + 1), round)
    
    let getDenseHash sparseHash =
        let rec buildDenseHash(elements, denseHash:int[]) =
            match (elements |> Array.length) with
            | 0 -> denseHash
            | _ -> let first16 = elements.[0..15]
                   let length = elements |> Array.length
                   buildDenseHash(elements.[16..(length - 1)], Array.concat [| denseHash ; [| (first16 |> Array.fold(fun s c -> s ^^^ c) 0) |] |])
        buildDenseHash(sparseHash, [| |])
    
    let list = [| 0..255 |]
    let additionalLengths = [| 17; 31; 73; 47; 23 |]
    
    getSparseHash(list, Array.concat[| input; additionalLengths |], 0, 0, 0, 0)
    |> getDenseHash
    |> Array.fold (fun s c -> s + sprintf "%02x" c) ""

//
// Day 14 starts here
//
let input = "jzgqcdpd"

let hex2bin hex =
    match hex with
    | '0' -> "0000" | '1' -> "0001" | '2' -> "0010" | '3' -> "0011" 
    | '4' -> "0100" | '5' -> "0101" | '6' -> "0110" | '7' -> "0111"
    | '8' -> "1000" | '9' -> "1001" | 'a' -> "1010" | 'b' -> "1011"
    | 'c' -> "1100" | 'd' -> "1101" | 'e' -> "1110" | 'f' -> "1111"
    | _ -> ""

let inputs = [|
        for i in 0..127 do
            yield ((input + "-" + i.ToString()).ToCharArray() |> Array.map (fun c -> c |> int))
    |]

let rec getBinaryKnotHashes(i, binHashes) =
    match i with
    | 128 -> binHashes
    | _ -> let knotHash = getKnotHash inputs.[i]
           let binHash = knotHash |> String.collect (fun c -> hex2bin c)
           getBinaryKnotHashes((i + 1), Array.concat[| binHashes ; [| binHash |] |])

let binHashes = getBinaryKnotHashes(0, [||])
let part1 = binHashes |> String.concat "" |> String.collect (fun c -> if c = '1' then "1" else "") |> String.length
printfn "Part 1 = %i" part1

// Part 2
let countGroups (hashes:string[]) =
    let rec findGroup x y set =
        // Find all points in this group and add them to the set of counted points so we don't count them twice
        let mutable s = set
        let adjacentPoints = 
            [| (x-1, y); (x, y-1); (x+1, y); (x, y+1) |]
            |> Array.filter (fun (a, b) -> a > -1 && a < 128 && b > -1 && b < 128 && binHashes.[a].[b] = '1')
            |> Array.filter (fun (a, b) -> not (s |> Set.contains (a,b)))
            
        adjacentPoints |> Array.iter(fun a ->
            s <- s |> Set.add a
            s <- findGroup (fst a) (snd a) s)
        s

    let mutable set = Set.empty
    let mutable groups = 0
    
    for x in 0..127 do
        for y in 0..127 do
            match hashes.[x].[y] with
            | '1' -> 
                // If we haven't already counted this point, find all points in this group and mark them as counted
                if not (set |> Set.contains (x, y)) then
                    set <- findGroup x y set
                    groups <- groups + 1
                else ()
            | _ -> ()
    groups

let part2 = countGroups binHashes
printfn "Part 2 = %i" part2
