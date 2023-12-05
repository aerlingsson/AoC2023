open System
open System.IO

type Mapping = { Source: int64; Destination: int64; Range: int64 }
type Mappings = { SeedToSoil: Mapping seq; SoilToFertilizer: Mapping seq; FertilizerToWater: Mapping seq; WaterToLight: Mapping seq; LightToTemperature: Mapping seq; TemperatureToHumidity: Mapping seq; HumidityToLocation: Mapping seq }

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

let isInRange (mapping: Mapping) (value: int64) = value >= mapping.Source && value <= mapping.Source + mapping.Range

let sourceToDestination (mappings: Mapping seq) (value: int64) =
  printfn ""
  match Seq.tryFind (fun m -> isInRange m value) mappings with
  | Some mapping ->
      printfn $"seed: {value}, mapping: {mapping}, destination: {mapping.Destination + (value - mapping.Source)}"
      mapping.Destination + (value - mapping.Source)
  | None ->
    printfn $"seed: {value}, mapping: None, destination: {value}"
    value

let part1() =
  let lines = readLines()
  let seeds =
    lines
    |> Seq.head
    |> fun x -> x.Split(": ").[1].Split(" ")
    |> Seq.map int64
  
  let mappings =
    String.Join("\n", Seq.skip 2 lines).Split("\n\n")
    |> Array.toList
    |> List.map (fun x -> x.Split(":\n").[1].Split("\n") |> Array.toList |> List.map (fun y -> y.Split(" ") |> Array.toList |> List.map int64) |> List.map toMapping)
    |> toMappings
  
  seeds
  |> Seq.map (fun seed ->
    seed
    |> sourceToDestination mappings.SeedToSoil
    |> sourceToDestination mappings.SoilToFertilizer
    |> sourceToDestination mappings.FertilizerToWater
    |> sourceToDestination mappings.WaterToLight
    |> sourceToDestination mappings.LightToTemperature
    |> sourceToDestination mappings.TemperatureToHumidity
    |> sourceToDestination mappings.HumidityToLocation
  )
  |> Seq.min
