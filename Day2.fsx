open System.IO

let maxRed = 12
let maxGreen = 13
let maxBlue = 14

let readLines() = File.ReadAllLines("Day2Input.txt")

let parseSet (set: string) =
  let colors = set.Split(", ")

  colors |> Seq.map (fun c ->
    match c.Split(' ') with
    | [| count; "red" |] -> "red", int count
    | [| count; "green" |] -> "green", int count
    | [| count; "blue" |] -> "blue", int count
    | _ -> failwith $"Invalid color"
  )

let parse (line: string) =
  let gameIdGamesSplit = line.Split(": ")
  let gameId = gameIdGamesSplit.[0].Split(' ')[1] |> int
  let sets = gameIdGamesSplit.[1].Split("; ") |> Seq.map parseSet |> Seq.collect id

  gameId, sets

let countColor (color: string) (sets: (string * int) seq) =
  sets |> Seq.filter (fun (c, _) -> c = color) |> Seq.map snd |> Seq.max

let countColors (sets: (string * int) seq) =
  let highestRed = countColor "red" sets
  let highestGreen = countColor "green" sets
  let highestBlue = countColor "blue" sets

  highestRed, highestGreen, highestBlue

let part1() =
  readLines()
  |> Seq.map parse
  |> Seq.filter (fun (gameId, sets) ->
    let red, green, blue = countColors sets
    red <= maxRed && green <= maxGreen && blue <= maxBlue
  )
  |> Seq.map fst
  |> Seq.sum

let part2() =
  readLines()
  |> Seq.map parse
  |> Seq.map snd
  |> Seq.map countColors
  |> Seq.map (fun (red, green, blue) -> red * green * blue)
  |> Seq.sum