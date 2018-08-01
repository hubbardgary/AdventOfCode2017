open System.IO
open System.Linq
open System.Text.RegularExpressions

let inputFilename = "day20_input.txt"
let inputPath = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)
let readInput (path:string) = File.ReadAllLines path
let input = readInput inputPath

let pattern = @"p=<(?<position>-?\d+,-?\d+,-?\d+)>, v=<(?<velocity>-?\d+,-?\d+,-?\d+)>, a=<(?<acceleration>-?\d+,-?\d+,-?\d+)>"

type coord = { X:int64; Y:int64; Z:int64 }
type particle = { Index:int; Position:coord; Velocity:coord; Acceleration:coord; }

let particles =
    [|
        for i in [0..input.Count() - 1] do
            let matchCollection = Regex.Matches(input.[i], pattern)
            let a = matchCollection |> Seq.cast<Match>
            let m = a.Select(fun x -> x.Groups).First()
            let position = m.Item("position").Value.Split(',') |> Array.map int64
            let velocity = m.Item("velocity").Value.Split(',') |> Array.map int64
            let acceleration = m.Item("acceleration").Value.Split(',') |> Array.map int64
            yield {
                Index = i;
                Position = { X = position.[0]; Y = position.[1]; Z = position.[2] };
                Velocity = { X = velocity.[0]; Y = velocity.[1]; Z = velocity.[2] };
                Acceleration = { X = acceleration.[0]; Y = acceleration.[1]; Z = acceleration.[2] }
            }
    |]

let nextTick currentState =
    [|
        for i in [0..(currentState |> Array.length) - 1] do
            let particle = currentState.[i]
            let newVelocity = { X = particle.Velocity.X + particle.Acceleration.X; Y = particle.Velocity.Y + particle.Acceleration.Y; Z = particle.Velocity.Z + particle.Acceleration.Z }
            yield {
                Index = particle.Index;
                Position = { X = particle.Position.X + newVelocity.X; Y = particle.Position.Y + newVelocity.Y; Z = particle.Position.Z + newVelocity.Z };
                Velocity = { X = newVelocity.X; Y = newVelocity.Y; Z = newVelocity.Z };
                Acceleration = particle.Acceleration
            }
    |]

let removeCollisions particles =
    particles
    |> Seq.ofArray
    |> Seq.groupBy (fun p -> p.Position.X.ToString() + "," + p.Position.Y.ToString() + "," + p.Position.Z.ToString())
    |> Seq.filter (fun (k, v) -> v |> Seq.length = 1)
    |> Seq.map (fun (k, v) -> v |> Seq.item 0)
    |> Seq.toArray

let rec solvePart1 state ticks =
    match ticks with
    | 0 -> state |> Array.minBy (fun x -> abs x.Position.X + abs x.Position.Y + abs x.Position.Z)
    | _ -> solvePart1 (nextTick state) (ticks - 1)

// The answer to Part 1 is removed as a collision in Part 2, so we need to run the simulation separately for each part of the problem.
let rec solvePart2 state ticks =
    match ticks with
    | 0 -> state |> Array.length
    | _ -> solvePart2 (nextTick (removeCollisions state)) (ticks - 1)

let part1 = (solvePart1 particles 500).Index
let part2 = solvePart2 particles 200

printfn "Part 1 = %i" part1
printfn "Part 2 = %i" part2
