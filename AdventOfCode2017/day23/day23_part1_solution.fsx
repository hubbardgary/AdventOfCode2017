open System
open System.IO
open System.Collections.Generic

let inputFilename = "day23_input.txt"
let path = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path

let registers = new Dictionary<string, int64>(dict [ ("a", 0L); ("b", 0L); ("c", 0L); ("d", 0L); ("e", 0L); ("f", 0L); ("g", 0L); ("h", 0L) ])

let mutable nextInstruction = 0

let incrementInstruction i = nextInstruction <- nextInstruction + i

let parseInt64 str =
    match Int64.TryParse(str) with
    | (true, int64) -> Some(int64)
    | _ -> None

let set reg x =
    let i = parseInt64 x
    match i with
    | None -> registers.[reg] <- registers.[x]
    | Some(i) -> registers.[reg] <- i
    incrementInstruction 1

let updateRegister reg x op =
    let i = parseInt64 x
    match i with
    | None -> registers.[reg] <- (op registers.[reg] registers.[x])
    | Some(i) -> registers.[reg] <- (op registers.[reg] i)
    incrementInstruction 1

let sub reg x = updateRegister reg x (-)
let mul reg x = updateRegister reg x (*)
    
let jnz testvalue jump =
    let isInt = parseInt64 testvalue
    match isInt with
    | None -> if registers.[testvalue] <> 0L
              then let a = parseInt64 jump
                   let offset = (if a.IsNone then registers.[jump] else a.Value) |> int
                   incrementInstruction offset
              else incrementInstruction 1
    | Some(isInt) -> if isInt <> 0L then incrementInstruction (jump |> int) else incrementInstruction 1

let part1 =
    let instructions = readInput path
    let rec execute i mulCalls =
        if i < (instructions |> Array.length) then
            let parts = instructions.[nextInstruction].Split()
            match parts.[0] with
            | "set" -> set parts.[1] parts.[2]
            | "sub" -> sub parts.[1] parts.[2]
            | "mul" -> mul parts.[1] parts.[2]
            | "jnz" -> jnz parts.[1] parts.[2]
            | _ -> ()
            execute nextInstruction (if parts.[0] = "mul" then mulCalls + 1 else mulCalls)
        else
            mulCalls
    execute nextInstruction 0

printfn "Part 1 = %i" part1