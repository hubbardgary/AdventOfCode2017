open System.IO

let inputFilename = "day22_input.txt"
let inputPath = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)
let readInput (path:string) = File.ReadAllLines path
let input = readInput inputPath

type Direction = North | East | South | West
type Turn = Left | Right | Reverse

let getNewDirection direction turn =
    match direction with
    | North -> match turn with
               | Left -> West
               | Right -> East
               | Reverse -> South
    | East -> match turn with
              | Left -> North
              | Right -> South
              | Reverse -> West
    | South -> match turn with
               | Left -> East
               | Right -> West
               | Reverse -> North
    | West -> match turn with
              | Left -> South
              | Right -> North
              | Reverse -> East

let move location direction =
    match direction with
    | North -> fst location - 1, snd location
    | East -> fst location, snd location + 1
    | South -> fst location + 1, snd location
    | West -> fst location, snd location - 1

let initialisegrid grid (input:string[]) =
    let offset = ((grid |> Array2D.length1) - 25) / 2
    for x in 0..24 do
        for y in 0..24 do
            grid.[offset+x, offset+y] <- input.[x].[y] |> char
    grid

let rec step (grid:char[,]) location direction infections i =
    match i with
    | 0 -> infections
    | _ -> match grid.[fst location, snd location] with
           | '.' -> let d = getNewDirection direction Left
                    grid.[fst location, snd location] <- 'W'
                    step grid (move location d) d infections (i-1)
           | 'W' -> grid.[fst location, snd location] <- '#'
                    step grid (move location direction) direction (infections+1) (i-1)
           | '#' -> let d = getNewDirection direction Right
                    grid.[fst location, snd location] <- 'F'
                    step grid (move location d) d infections (i-1)
           | _ -> let d = getNewDirection direction Reverse
                  grid.[fst location, snd location] <- '.'
                  step grid (move location d) d infections (i-1)

let emptygrid = Array2D.create 425 425 '.'
let grid = initialisegrid emptygrid input
let point = 212,212

let answer = step grid point North 0 10000000
printfn "Part 2 = %i" answer