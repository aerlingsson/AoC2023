open System
open System.IO

let readLines() = File.ReadAllLines("Day8Input.txt")
 
let navigate (nodeToFind: string) (nodeConnections: Map<(string * char), string>) (instructions: char seq) =
  let rec navigate' (node: string) (instructionIdx: int) (stepsTaken: int) =
    let instruction = Seq.item instructionIdx instructions
    let nextInstructionIdx = if instructionIdx = Seq.length instructions - 1 then 0 else instructionIdx + 1
    let nextNode = nodeConnections.[(node, instruction)]
    if nextNode = nodeToFind then
      stepsTaken
    else
      navigate' nextNode nextInstructionIdx (stepsTaken + 1)
  
  navigate' "AAA" 0 1

let part1() =
  let lines = readLines()
  let instructions = Seq.item 0 lines
  let nodes =
    lines
    |> Seq.skip 2
    |> Seq.map (fun node ->
      let nodeAndConnections = node.Split(" = ")
      let node = nodeAndConnections.[0]
      node, nodeAndConnections.[1].Replace("(", "").Replace(")", "").Split(", ")
    )
    |> Seq.fold (fun (nodeConnections: Map<(string * char), string>) (node: string, connections: string array) ->
      let nodeConnections' = nodeConnections.Add((node, 'L'), connections.[0])
      nodeConnections'.Add((node, 'R'), connections.[1])
    ) Map.empty
  
  navigate "ZZZ" nodes instructions
