open System
open System.IO
open System.Text.RegularExpressions

type Race = { Time: int64; Record: int64}

let readLines() = File.ReadAllLines("Day6Input.txt")

let digits (line: string) = Regex.Matches(line, "[0-9]+") |> Seq.map (fun (m: Match) -> m.Value |> int)

let calcDistance (timeHeldMs: int64) (raceTimeMs: int64) =
  (raceTimeMs - timeHeldMs) * timeHeldMs

let solutionsCount (race: Race) =
  [1L..race.Time - 1L]
  |> Seq.filter (fun timeHeld -> calcDistance timeHeld race.Time > race.Record)
  |> Seq.length

let part1() =
  readLines()
  |> Seq.map digits
  |> fun matchLists -> Seq.zip (Seq.item 0 matchLists) (Seq.item 1 matchLists)
  |> Seq.map (fun (time, distance) -> {Time = time; Record = distance})
  |> Seq.map (solutionsCount)
  |> Seq.reduce (*)

let part2() =
  let race =
    readLines()
    |> Seq.map digits
    |> Seq.map (fun digitsInLine -> digitsInLine |> Seq.map string |> fun s -> String.Join("", s) |> int64)
    |> fun timeAndRecord -> { Time = Seq.item 0 timeAndRecord; Record = Seq.item 1 timeAndRecord }
 
  solutionsCount race
