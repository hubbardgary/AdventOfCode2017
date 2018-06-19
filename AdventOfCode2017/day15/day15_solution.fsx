open System
open System.IO
open System.Text.RegularExpressions

let inputFilename = "day15_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)
let readInput (path:string) = File.ReadAllText path
let seeds = Regex.Matches((readInput input), @"\d+")

let trimAndPad str size =
    let len = str |> String.length
    let substr = str.[(if len > size then len-size else 0)..len-1]
    let substrLen = substr |> String.length
    if substrLen < size then (String.replicate (size-substrLen) "0") + substr else substr

let countMatches sequences =
    sequences
    |> Seq.filter (fun (a:int64 * int64) -> (trimAndPad (Convert.ToString(fst a, 2)) 16 = trimAndPad (Convert.ToString(snd a, 2)) 16))
    |> Seq.length

let generator max multiplier factor (value, round) =
    let divisor = 2_147_483_647L
    let rec getNext value = 
        let nextVal = (value * multiplier) % divisor
        let remainder = nextVal % factor
        match remainder with
        | 0L -> nextVal
        | _ -> getNext nextVal

    if round = max then
        None
    else
        let next = getNext value
        let newState = (next, round+1)
        Some (next, newState)
   
let genAInput = seeds.[0].Value |> int64
let genBInput = seeds.[1].Value |> int64
let genAMultiplier = 16_807L
let genBMultiplier = 48_271L

// Part 1
let generatorAp1 max multiplier = Seq.unfold (generator max multiplier 1L) (genAInput, 0)
let generatorBp1 max multiplier = Seq.unfold (generator max multiplier 1L) (genBInput, 0)

let seqAp1 = generatorAp1 40_000_000 genAMultiplier
let seqBp1 = generatorBp1 40_000_000 genBMultiplier

let seqPairsP1 = Seq.zip seqAp1 seqBp1

printfn "Part 1 = %i" (countMatches seqPairsP1)

// Part 2
let generatorAp2 max multiplier = Seq.unfold (generator max multiplier 4L) (genAInput, 0)
let generatorBp2 max multiplier = Seq.unfold (generator max multiplier 8L) (genBInput, 0)

let seqAp2 = generatorAp2 5_000_000 genAMultiplier
let seqBp2 = generatorBp2 5_000_000 genBMultiplier

let seqPairsP2 = Seq.zip seqAp2 seqBp2

printfn "Part 2 - %i" (countMatches seqPairsP2)