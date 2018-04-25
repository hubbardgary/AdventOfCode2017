open System

let input = 325489 |> float

// The bottom right corner of the spiral will be the next odd square number from our input
let nearestSquare = Math.Ceiling(Math.Sqrt input)
let nearestOddSquare = if nearestSquare % 2.0 = 0.0 then nearestSquare + 1.0 else nearestSquare
let spiralWidth = nearestOddSquare
let manhattenDistanceFromCorner = spiralWidth - 1.0

// Get all corners of this ring of the spiral
let corners = 
    let bottomRight = spiralWidth * spiralWidth
    let bottomLeft = bottomRight - spiralWidth + 1.0
    let topLeft = bottomLeft - spiralWidth + 1.0
    let topRight = topLeft - spiralWidth + 1.0
    [topRight; topLeft; bottomLeft; bottomRight]

let cornerList = corners

// sort cornerList by distance from our input, closest first
let closestCorner = (cornerList
                    |> List.sortBy (fun (x:float) -> (Math.Abs (input - x)))).[0]

let manhattenDistance = manhattenDistanceFromCorner - (Math.Abs (closestCorner - input)) |> int

printfn "Part 1 = %i" manhattenDistance

// Part 2
let dimensions = spiralWidth |> int
let spiral:int[,] = Array2D.zeroCreate dimensions dimensions
let mutable x = Math.Floor(nearestOddSquare / 2.0) |> int
let mutable y = x
let mutable n = 1

type Direction = Left | Right | Up | Down

let GetDirection currentDirection:Direction = 
    match currentDirection with
    | Direction.Right -> if spiral.[x-1,y] = 0 then Direction.Up else currentDirection
    | Direction.Up -> if spiral.[x,y-1] = 0 then Direction.Left else currentDirection
    | Direction.Left -> if spiral.[x+1,y] = 0 then Direction.Down else currentDirection
    | _ -> if spiral.[x,y+1] = 0 then Direction.Right else currentDirection

let GetValue x y =
    spiral.[x-1,y-1] + spiral.[x+1,y-1] + spiral.[x,y-1] + 
    spiral.[x-1,y] + spiral.[x+1,y] + 
    spiral.[x-1,y+1] + spiral.[x+1,y+1] + spiral.[x,y+1]

spiral.[x,y] <- n
y <- y + 1
spiral.[x,y] <- n

let mutable direction = Direction.Right
while n <= (input |> int) do
    direction <- GetDirection direction
    
    match direction with
    | Direction.Right -> y <- y + 1
    | Direction.Up -> x <- x - 1
    | Direction.Left -> y <- y - 1
    | _ -> x <- x + 1
    
    n <- GetValue x y
    spiral.[x,y] <- n

printfn "Part 2 = %i" n

// Print spiral
let rec padString (numString:string) =
    if numString.Length < 6 then padString(" " + numString) else numString

let padNum x = padString (x.ToString())

// Our grid is likely mostly zeroes, so just print the middle section where we built our spiral
let sprialWidth = Math.Abs((Math.Floor(nearestOddSquare / 2.0) |> int) - x)
let spiralStart = (dimensions / 2) - sprialWidth
let spiralEnd = (dimensions / 2) + sprialWidth
for a in [spiralStart..spiralEnd] do
    for b in [spiralStart..spiralEnd] do
        printf "%s|" (padNum spiral.[a,b])
    printfn ""
 
//end print spiral
