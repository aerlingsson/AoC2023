open System.IO

let readLines() = File.ReadAllLines("Day8Input.txt")

let parseLine (line: string) =
  let nodeAndConnections = line.Split(" = ")
  let node = nodeAndConnections.[0]
  node, nodeAndConnections.[1].Replace("(", "").Replace(")", "").Split(", ")

let createNodeConnections (nodeConnections: Map<(string * char), string>) (node: string, connections: string array) =
  let nodeConnections' = nodeConnections.Add((node, 'L'), connections.[0])
  nodeConnections'.Add((node, 'R'), connections.[1])

let buildNodes = Seq.skip 2 >> Seq.map parseLine >> Seq.fold createNodeConnections Map.empty

let part1() =
  let navigate (nodeConnections: Map<(string * char), string>) (instructions: char seq) =
    let rec navigate' (node: string) (instructionIdx: int) (stepsTaken: int) =
      let instruction = Seq.item instructionIdx instructions
      let nextInstructionIdx = if instructionIdx = Seq.length instructions - 1 then 0 else instructionIdx + 1
      let nextNode = nodeConnections.[(node, instruction)]
      if nextNode = "ZZZ" then
        stepsTaken
      else
        navigate' nextNode nextInstructionIdx (stepsTaken + 1)
    
    navigate' "AAA" 0 1

  let lines = readLines()
  let instructions = Seq.item 0 lines
  let nodes = buildNodes lines
  
  navigate nodes instructions

let part2() =
  let rec gcd (a: int64) (b: int64) = if a = 0 then b else gcd (b % a) a
  let lcm (a: int64) (b: int64) = (a / gcd a b) * b
  let lcmAll (numbers: int64 seq) = Seq.fold lcm 1 numbers

  let findCycles (instructions: char seq) (nodeConnections: Map<(string * char), string>) =
    let rec findCycle' (instructionIdx: int) (stepsTaken: int) (isFirstIteration: bool) (node: string) =
      let instruction = Seq.item instructionIdx instructions
      let nextInstructionIdx = if instructionIdx = Seq.length instructions - 1 then 0 else instructionIdx + 1
      let nextNode = nodeConnections.[(node, instruction)]
      if nextNode.EndsWith("Z") && isFirstIteration then
        findCycle' nextInstructionIdx 1 false nextNode
      else if nextNode.EndsWith("Z") then
        stepsTaken
      else
        findCycle' nextInstructionIdx (stepsTaken + 1) isFirstIteration nextNode

    let allEndingOnA = Map.keys nodeConnections |> Seq.map fst |> Seq.distinct |> Seq.filter (fun node -> node.EndsWith("A"))
    allEndingOnA |> Seq.map (findCycle' 0 1 true)

  let lines = readLines()
  let instructions = Seq.item 0 lines
  let nodes = buildNodes lines

  nodes |> findCycles instructions |> Seq.map int64 |> lcmAll