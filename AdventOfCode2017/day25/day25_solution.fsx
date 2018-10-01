open System
open System.IO
open System.Text.RegularExpressions

[<Literal>]
let NewValue = 0
[<Literal>]
let Direction = 1
[<Literal>]
let NextState = 2

let inputFilename = "day25_input.txt"
let path = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllText path

let parseSteps input = 
    let pattern = "Perform a diagnostic checksum after (?<steps>\d+) steps."
    (Regex.Matches(input, pattern) 
     |> Seq.cast<Match>
     |> Seq.head).Groups.Item("steps").Value
     |> int

let parseStates (input:string) = 
    input.Split([| "\r\n\r\n" |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.skip 1
    |> Array.map (fun (x:string) -> let lines = x.Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
                                    let key = lines.[0]
                                    let instructionLines = lines |> Array.filter (fun (y:string) -> y.Contains("-"))
                                    let chunks = instructionLines |> Array.chunkBySize 3
                                                                  |> Array.map (fun (x:string[]) -> 
                                                                      x |> Array.map (fun y -> y.Replace(".", "").Split(' ') |> Seq.last))
                                    key.Substring(9, 1), chunks)
    |> dict

let input = readInput path
let steps = parseSteps input
let states = parseStates input

let rec solve tape currentState currentStep idx totalSteps =

    let getNextIdx idx moveDirection =
        match moveDirection with
        | "left" -> idx - 1
        | "right" -> idx + 1
        | _ -> -1
    
    if currentStep = totalSteps
    then
        tape |> Array.sum
    else
        let instruction = states.[currentState].[tape.[idx]]
        tape.[idx] <- (instruction.[NewValue] |> int)
        solve tape instruction.[NextState] (currentStep + 1) (getNextIdx idx instruction.[Direction]) totalSteps
        
let part1 = solve (Array.create steps 0) "A" 0 (steps / 2) steps
printfn "Part 1 = %i" part1
