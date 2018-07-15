open System
open System.IO
open System.Collections.Generic
open System.Linq

let inputFilename = "day18_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) = File.ReadAllLines path

let runPrograms p0reg p1reg p0q p1q =
    let mutable p0Blocked = false
    let mutable p1Blocked = false
    let mutable valuesSentFromP1 = 0
    let instructions = readInput input

    let parseInt64 str =
        match Int64.TryParse(str) with
        | true, int64 -> Some(int64)
        | _ -> None
    
    let setRegister (register:Dictionary<string, int64>) reg value = register.[reg] <- value
    
    let set reg x (register:Dictionary<string, int64>) =
        // x could be an int value or a register reference
        let isInt = parseInt64 x
        match isInt with
        | None -> if register.ContainsKey(x) then setRegister register reg register.[x] else setRegister register reg 0L
        | Some(isInt) -> setRegister register reg isInt
    
    let updateRegister (register:Dictionary<string, int64>) reg x op =
        let isInt = parseInt64 x
        match isInt with
        | None -> if register.ContainsKey(reg) then setRegister register reg (op register.[reg] register.[x]) else setRegister register reg (op 0L register.[x])
        | Some(isInt) -> if register.ContainsKey(reg) then setRegister register reg (op register.[reg] isInt) else setRegister register reg (op 0L isInt)
    
    let add reg x register = updateRegister register reg x (+)
    let mul reg x register = updateRegister register reg x (*)
    let modulo reg x register = updateRegister register reg x (%)
    
    let rcv reg (register:Dictionary<string, int64>) (q:Queue<int64>) =
        match q.Count with
        | 0 -> true // thread is blockeed
        | _ -> setRegister register reg (q.Dequeue())
               false
        
    let jgz testvalue jump (register:Dictionary<string, int64>) =
        let isInt = parseInt64 testvalue
        match isInt with
        | None -> if register.[testvalue] > 0L
                  then let a = parseInt64 jump
                       let offset = (if a.IsNone then register.[jump] else a.Value) |> int
                       offset
                  else 1
        | Some(isInt) -> if isInt > 0L then (jump |> int) else 1
    
    let send reg (register:Dictionary<string, int64>) (q:Queue<int64>) = 
        let isInt = parseInt64 (reg.ToString())
        match isInt with
        | None -> q.Enqueue(register.[reg])
        | Some(isInt) -> q.Enqueue(isInt)
    
    let executeInstruction i register sendQ receiveQ threadId =
        let parts = instructions.[i].Split()
        match parts.[0] with
        | "set" -> set parts.[1] parts.[2] register; i + 1
        | "add" -> add parts.[1] parts.[2] register; i + 1
        | "mul" -> mul parts.[1] parts.[2] register; i + 1
        | "mod" -> modulo parts.[1] parts.[2] register; i + 1
        | "jgz" -> i + jgz parts.[1] parts.[2] register
        | "snd" -> send parts.[1] register sendQ
                   valuesSentFromP1 <- if threadId = 1 then valuesSentFromP1 + 1 else valuesSentFromP1
                   i + 1
        | "rcv" -> match rcv parts.[1] register receiveQ with
                   | true -> if threadId = 0
                             then p0Blocked <- true
                             else p1Blocked <- true
                             i
                   | false -> if threadId = 0
                              then p0Blocked <- false
                              else p1Blocked <- false
                              i + 1
        | _ -> 0
    
    let rec step instruction0 instruction1 =
        let next0 = if instruction0 < instructions.Count() then executeInstruction instruction0 p0reg p1q p0q 0 else instruction0
        let next1 = if instruction1 < instructions.Count() then executeInstruction instruction1 p1reg p0q p1q 1 else instruction1
        
        if next0 >= instructions.Count() || next1 > instructions.Count() || (p0Blocked && p1Blocked)
        then valuesSentFromP1
        else step next0 next1
        
    step 0 0
    
// initialise registers and message queues
let p0reg = new Dictionary<string, int64>()
let p1reg = new Dictionary<string, int64>()
p0reg.Add("p", 0L)
p1reg.Add("p", 1L)
let p0q = new Queue<int64>()
let p1q = new Queue<int64>()

let part2 = runPrograms p0reg p1reg p0q p1q

printfn "Part 2 = %i" part2
