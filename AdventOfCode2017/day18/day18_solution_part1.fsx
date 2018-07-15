open System
open System.IO
open System.Collections.Generic

let inputFilename = "day18_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path

let registers = new Dictionary<string, int64>()
let mutable nextInstruction = 0
let mutable lastPlayedSound = 0L
let mutable terminate = false

let setRegister reg value = registers.[reg] <- value
let incrementInstruction i = nextInstruction <- nextInstruction + i

let parseInt64 str =
    match Int64.TryParse(str) with
    | (true, int64) -> Some(int64)
    | _ -> None

let set reg x =
    let i = parseInt64 x
    match i with
    | None -> if registers.ContainsKey(reg) then setRegister reg registers.[x] else setRegister reg 0L
    | Some(i) -> setRegister reg i
    incrementInstruction 1

let updateRegister reg x op =
    let i = parseInt64 x
    match i with
    | None -> if registers.ContainsKey(reg) then setRegister reg (op registers.[reg] registers.[x]) else setRegister reg (op 0L registers.[x])
    | Some(i) -> if registers.ContainsKey(reg) then setRegister reg (op registers.[reg] i) else setRegister reg (op 0L i)
    incrementInstruction 1

let add reg x = updateRegister reg x (+)
let mul reg x = updateRegister reg x (*)
let modulo reg x = updateRegister reg x (%)

let rcv reg =
    if registers.ContainsKey(reg) then
        if registers.[reg] > 0L then terminate <- true
    incrementInstruction 1
    
let jgz testvalue jump =
    let isInt = parseInt64 testvalue
    match isInt with
    | None -> if registers.[testvalue] > 0L
              then let a = parseInt64 jump
                   let offset = (if a.IsNone then registers.[jump] else a.Value) |> int
                   incrementInstruction offset
              else incrementInstruction 1
    | Some(isInt) -> if isInt > 0L then incrementInstruction (jump |> int) else incrementInstruction 1

let snd reg =
    lastPlayedSound <- registers.[reg]
    incrementInstruction 1

let executeInstruction (i:string) =
    let parts = i.Split()
    match parts.[0] with
    | "set" -> set parts.[1] parts.[2]
    | "add" -> add parts.[1] parts.[2]
    | "mul" -> mul parts.[1] parts.[2]
    | "mod" -> modulo parts.[1] parts.[2]
    | "rcv" -> rcv parts.[1]
    | "jgz" -> jgz parts.[1] parts.[2]
    | "snd" -> snd parts.[1]
    | _ -> ()
   

let part1 =
    let instructions = readInput input
    let rec execute i =
        executeInstruction instructions.[nextInstruction]
        match terminate with
        | true -> lastPlayedSound
        | _ -> execute nextInstruction
    execute nextInstruction

printfn "Part 1 = %i" part1
