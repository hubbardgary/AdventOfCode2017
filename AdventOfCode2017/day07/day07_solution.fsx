open System
open System.IO
open System.Linq
open System.Text.RegularExpressions

let inputFilename = "day07_input.txt"
let input = Path.Combine(__SOURCE_DIRECTORY__, inputFilename)

let readInput (path:string) =
    File.ReadAllText path

type program = { Name:string; Weight:int; Children:string[] }

let data = readInput input
let items = data.Split([|'\n'; '\r'|], StringSplitOptions.RemoveEmptyEntries)
let pattern = @"(?<program>\w+) \((?<weight>\d+)\)( -> )?(?<children>.*)?"
let programs = new ResizeArray<program>()

for i in [0..items.Count() - 1] do
    let matchCollection = Regex.Matches(items.[i], pattern)
    let a = matchCollection |> Seq.cast<Match>
    let m = a.Select(fun x -> x.Groups).First()
    let program = { Name = m.Item("program").Value;
                    Weight = (m.Item("weight").Value |> int);
                    Children = m.Item("children").Value.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) }
    
    programs.Add program

let parents = programs.Where(fun x -> x.Children.Count() > 0).ToArray()

// We want the parent that isn't anyone's child
let allChildren = new ResizeArray<string>()
parents |> Array.iter (fun p -> allChildren.AddRange(p.Children) |> ignore)
let root = parents |> Array.find(fun p -> allChildren.Contains(p.Name) = false)
printfn "Part 1: %s" root.Name

// Part 2
[<AllowNullLiteral>]
type node (name:string, weight:int) =
    let children = new ResizeArray<node>()
    member x.Name = name
    member x.Weight = weight
    member x.Children = children

    member x.SumChildWeights(node:node, currentTotal:int) =
        let mutable runningTotal = currentTotal
        if node.Children.Count > 0 then
            for i = 0 to node.Children.Count - 1 do
                runningTotal <- x.SumChildWeights(node.Children.[i], runningTotal)
        runningTotal + node.Weight

    member x.SumOfChildWeights
        with get() =
            if x.Children = null then x.Weight else
                let mutable sum = 0
                x.SumChildWeights(x, sum)

let rec growTree (node:node, programs:ResizeArray<program>) =
    let childNames = programs.Where(fun p -> p.Name = node.Name).First().Children
    for i = 0 to childNames.Count() - 1 do
        let childName = childNames.[i]
        let child = programs.Where(fun p -> p.Name = childName).First()
        let childNode = new node(child.Name, child.Weight)
        growTree(childNode, programs)
        node.Children.Add(childNode)

let rec getUnbalancedBranch(node:node):node =
    let mutable unbalanced:node = null
    let mutable childCount = 0
    while unbalanced = null && childCount < node.Children.Count do
        unbalanced <- getUnbalancedBranch(node.Children.[childCount])
        childCount <- childCount + 1

    if unbalanced <> null then unbalanced else if node.Children.Select(fun c -> c.SumOfChildWeights).Distinct().Count() > 1 then node else null

let tree = new node(root.Name, root.Weight)

growTree(tree, programs)
let unbalancedBranch = getUnbalancedBranch(tree)

let weights = 
    unbalancedBranch.Children.Select(fun c -> 
        c.SumOfChildWeights).OrderByDescending(fun x -> 
            unbalancedBranch.Children.Where(fun y -> 
                y.SumOfChildWeights = x).Count()).Distinct()

let difference = weights.First() - weights.Last()
let answer = unbalancedBranch.Children.Where(fun x -> x.SumOfChildWeights = weights.Last()).First().Weight + difference

printfn "Part 2: %i" answer