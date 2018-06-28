let input = 367

let rec cyclicBuffer items =
    seq {
        for i in items do
            yield i
        yield! cyclicBuffer items
    }

let step buffer stepSize i =
    let currentPosition = buffer |> Array.findIndex (fun x -> x = i-1)
    let cyclic = cyclicBuffer buffer
    let insertionPoint = cyclic |> Seq.item (currentPosition + stepSize)
    let insertionIdx = cyclic |> Seq.findIndex (fun x -> x = insertionPoint)
    Array.concat [| buffer.[0..insertionIdx] ; [| i |] ; buffer.[insertionIdx+1..] |]

let rec spinLockPart1 (buffer:int[]) stepSize i maxRounds =
    if i = maxRounds
    then buffer
    else
        let buf = step buffer stepSize (i+1)
        spinLockPart1 buf stepSize (i+1) maxRounds

let finalBuffer = spinLockPart1 [| 0 |] input 0 2017
let lastValueIdx = finalBuffer |> Array.findIndex (fun x -> x = 2017)
let part1 = finalBuffer.[lastValueIdx+1]

printfn "Part 1 = %i" part1

// Part 2
// No need to maintain the buffer this time - we just need to track the last item inserted
// after the 0 value, which will always remain at index 0.
let nextInsert currentPos stepSize round = (currentPos + 1 + stepSize) % round

let rec spinLockPart2 currentPos valAfterZero stepSize round maxRounds =
    match round = maxRounds with
    | true -> valAfterZero
    | false -> 
        let insertionPoint = nextInsert currentPos stepSize round
        match insertionPoint with
        | 0 -> spinLockPart2 insertionPoint round stepSize (round+1) maxRounds
        | _ -> spinLockPart2 insertionPoint valAfterZero stepSize (round+1) maxRounds

let part2 = spinLockPart2 0 0 input 1 50_000_000

printfn "Part 2 = %i" part2
