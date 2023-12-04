open System
open System.IO

type Card =
  {
    Idx: int
    WinningNumbers: int seq
    MyNumbers: int seq
  }

let readLines () = File.ReadAllLines("Day4Input.txt")

let parseLine (idx: int, line: string) =
  let winningMyNumbers = line.Split(": ").[1].Split(" | ")
  let winningNumbers, myNumbers = winningMyNumbers[0], winningMyNumbers[1]
  let numbersToInts (numberStr: string) =
    numberStr.Split(" ")
    |> Seq.map (fun x -> x.Trim())
    |> Seq.filter (String.IsNullOrEmpty >> not)
    |> Seq.map int

  {
    Idx = idx
    WinningNumbers = numbersToInts winningNumbers
    MyNumbers = numbersToInts myNumbers
  }

let getMyWinningNumbers (card: Card) =
  card.MyNumbers |> Seq.filter (fun myNumber -> card.WinningNumbers |> Seq.exists ((=) myNumber))

let part1() =
  readLines()
  |> Seq.indexed
  |> Seq.map parseLine
  |> Seq.map (fun card ->
    let myWinning = getMyWinningNumbers card

    match Seq.length myWinning with
    | 0 -> 0 |> float
    | 1 -> 1 |> float
    | length -> (float 2) ** (float length - float 1)
  )
  |> Seq.sum
  |> int
 
let part2() =
  let cards = readLines() |> Seq.indexed |> Seq.map parseLine |> Seq.toList
  let cardCounts = cards |> Seq.groupBy (fun card -> card.Idx) |> Seq.map (fun (idx, cards) -> idx, Seq.length cards) |> Map.ofSeq

  Seq.fold (fun (cardCounts: Map<int, int>) (card: Card) ->
    let myWinningsCount = card |> getMyWinningNumbers |> Seq.length

    Seq.fold (fun (cardCounts: Map<int, int>) (toUpdate: int) ->
        let idxToUpdate = card.Idx + toUpdate
        let updateCount (count: int option) = Option.map (fun x -> x + cardCounts[card.Idx]) count
        let updatedCardCounts = Map.change idxToUpdate updateCount cardCounts
        updatedCardCounts
    ) cardCounts [1..myWinningsCount]
  ) cardCounts cards
  |> Map.toSeq
  |> Seq.sumBy snd
