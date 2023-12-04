open System
open System.IO

let readLines () = File.ReadAllLines("Day4Input.txt")

let parseLine (line: string) =
  let cardNumberValues = line.Split(": ")
  let winningMyNumbers = cardNumberValues[1].Split(" | ")
  let winningNumbers, myNumbers = winningMyNumbers[0], winningMyNumbers[1]
  let numbersToInts (numberStr: string) =
    numberStr.Split(" ")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (String.IsNullOrEmpty >> not)
    |> Seq.map int

  numbersToInts winningNumbers, numbersToInts myNumbers

let part1() =
  readLines()
  |> Seq.map parseLine
  |> Seq.map (fun (winningNumbers, myNumbers) ->
    let myWinning = myNumbers |> Seq.filter (fun myNumber -> winningNumbers |> Seq.exists ((=) myNumber))
    match Seq.length myWinning with
    | 0 -> 0 |> float
    | 1 -> 1 |> float
    | length -> (float 2) ** (float length - float 1)
  )
  |> Seq.sum
  |> int
