open System
open System.IO

type Mapping = { Source: int64; Destination: int64; Range: int64 }
type Mappings =
  {
    SeedToSoil: Mapping seq
    SoilToFertilizer: Mapping seq
    FertilizerToWater: Mapping seq
    WaterToLight: Mapping seq
    LightToTemperature: Mapping seq
    TemperatureToHumidity: Mapping seq
    HumidityToLocation: Mapping seq
  } 

let readLines() = File.ReadLines("Day5Input.txt")

let toMapping (values: int64 list) = { Destination = values.[0]; Source = values.[1]; Range = values.[2] }
let toMappings (mappings: Mapping list list) =
  { SeedToSoil = mappings.[0]
    SoilToFertilizer = mappings.[1]
    FertilizerToWater = mappings.[2]
    WaterToLight = mappings.[3]
    LightToTemperature = mappings.[4]
    TemperatureToHumidity = mappings.[5]
    HumidityToLocation = mappings.[6] }

let getMappings (lines: string seq) =
  String.Join("\n", Seq.skip 2 lines).Split("\n\n")
  |> Array.toList
  |> List.map (fun x ->
    x.Split(":\n").[1].Split("\n")
    |> Array.toList
    |> List.map (fun y -> y.Split(" ") |> Array.toList |> List.map int64)
    |> List.map toMapping)
  |> toMappings

let valueIsInRange (rangeStart: int64) (rangeLength: int64) (value: int64) =
  value >= rangeStart && value <= (rangeStart + (rangeLength-1L))

let seedToLocation (mappings: Mappings) (seed: int64) =
  let sourceToDestination (mappings: Mapping seq) (value: int64) =
    match Seq.tryFind (fun m -> valueIsInRange m.Source m.Range value) mappings with
    | Some mapping -> mapping.Destination + (value - mapping.Source)
    | None -> value

  seed
  |> sourceToDestination mappings.SeedToSoil
  |> sourceToDestination mappings.SoilToFertilizer
  |> sourceToDestination mappings.FertilizerToWater
  |> sourceToDestination mappings.WaterToLight
  |> sourceToDestination mappings.LightToTemperature
  |> sourceToDestination mappings.TemperatureToHumidity
  |> sourceToDestination mappings.HumidityToLocation

let part1() =
  let lines = readLines()
  let seeds =
    lines
    |> Seq.head
    |> fun x -> x.Split(": ").[1].Split(" ")
    |> Seq.map int64
  
  let mappings = getMappings lines
  
  seeds
  |> Seq.map (seedToLocation mappings)
  |> Seq.min

let destinationToSource (mappings: Mapping seq) (value: int64) =
  match Seq.tryFind (fun m -> valueIsInRange m.Destination m.Range value) mappings with
  | Some mapping -> mapping.Source + (value - mapping.Destination)
  | None -> value

// This could probably be prettier
let invertMap (mappings: Mapping list) (outputEndpoints: int64 list) =
    let mapEndpoints =
        mappings
        |> List.map (fun m -> [(m.Destination, m.Source); (m.Destination + m.Range - 1L, m.Source + m.Range - 1L)])
        |> List.concat
        |> List.sort

    let outputSourceEndpoints =
        outputEndpoints
        |> List.map (destinationToSource mappings)
        |> List.sort

    let inputSourceEndpoints =
        mapEndpoints
        |> List.map (fun (_, x) -> x)
        |> Set.ofList
        |> Set.toSeq
        |> Seq.sort
        |> Seq.toList

    let inputSourceEndpoints =
      if List.head inputSourceEndpoints > 0 then
        0L :: inputSourceEndpoints.Head - 1L :: inputSourceEndpoints
      else
        inputSourceEndpoints

    let inputSourceEndpoints =
      if List.last inputSourceEndpoints < Int64.MaxValue then
        inputSourceEndpoints @ [Seq.last inputSourceEndpoints + 1L; Int64.MaxValue]
      else
        inputSourceEndpoints

    outputSourceEndpoints
    |> Set.ofList
    |> Set.union (Set.ofList inputSourceEndpoints)
    |> Set.toSeq
    |> Seq.sort
    |> Seq.toList

// Part 2 is more or less stolen from https://www.reddit.com/r/adventofcode/comments/18b4b0r/comment/kc3q9c6/
let part2() =
  let lines = readLines()
  let mappings = getMappings lines

  let seedsWithRanges =
    lines
    |> Seq.head
    |> fun x -> x.Split(": ").[1].Split(" ")
    |> Seq.map int64
    |> Seq.chunkBySize 2
    |> Seq.map (fun x -> (x.[0], x.[1]))
  
  let seedEndpoints =
    [0L; Int64.MaxValue]
    |> invertMap (Seq.toList mappings.HumidityToLocation)
    |> invertMap (Seq.toList mappings.TemperatureToHumidity)
    |> invertMap (Seq.toList mappings.LightToTemperature)
    |> invertMap (Seq.toList mappings.WaterToLight)
    |> invertMap (Seq.toList mappings.FertilizerToWater)
    |> invertMap (Seq.toList mappings.SoilToFertilizer)
    |> invertMap (Seq.toList mappings.SeedToSoil)
  
  let seedEndpoints = seedEndpoints |> Seq.filter(fun se -> Seq.exists (fun (seedStart, seedRange) -> valueIsInRange seedStart seedRange se) seedsWithRanges)

  Seq.concat [seedEndpoints; Seq.collect (fun (seedStart, seedRange) -> [seedStart; seedStart + seedRange - 1L]) seedsWithRanges]
  |> Seq.distinct
  |> Seq.sort
  |> Seq.map (seedToLocation mappings)
  |> Seq.min
