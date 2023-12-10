open System
open System.IO

type HandType =
  | FiveOfAKind = 6
  | FourOfAKind = 5
  | FullHouse = 4
  | ThreeOfAKind = 3
  | TwoPair = 2
  | OnePair = 1
  | HighCard = 0

type Hand = { Cards: int seq; Bid: int; HandType: HandType }

let readLines() = File.ReadAllLines("Day7Input.txt")

let cardToInt (jsAreJokers: bool) (c: string) =
  match c with
  | "T" -> 10
  | "J" -> if jsAreJokers then 1 else 11
  | "Q" -> 12
  | "K" -> 13
  | "A" -> 14
  | _ -> int c

let cardCounts (jsAreJokers: bool) (cards: int seq) =
  if jsAreJokers then
    let mostOf =
      if cards |> Seq.exists (fun card -> card <> 1) then
        cards |> Seq.filter (fun card -> card <> 1) |> Seq.countBy id |> Seq.maxBy (fun (value, count) -> (count, value)) |> fst
      else cards |> Seq.countBy id |> Seq.maxBy (fun (value, count) -> (count, value)) |> fst

    cards
    |> Seq.map (fun card -> if card = 1 then mostOf else card)
    |> Seq.countBy id
  else cards |> Seq.countBy id

let toHand ((cardCounts: (int * int) seq), (cards: int seq, bid: int)) =
  if Seq.exists (fun (_, count) -> count = 5) cardCounts then { Cards = cards; Bid = bid; HandType = HandType.FiveOfAKind }
  else if Seq.exists (fun (_, count) -> count = 4) cardCounts then { Cards = cards; Bid = bid; HandType = HandType.FourOfAKind }
  else if Seq.exists (fun (_, count) -> count = 3) cardCounts && Seq.exists (fun (_, count) -> count = 2) cardCounts then { Cards = cards; Bid = bid; HandType = HandType.FullHouse }
  else if Seq.exists (fun (_, count) -> count = 3) cardCounts then { Cards = cards; Bid = bid; HandType = HandType.ThreeOfAKind }
  else
    let pairs = cardCounts |> Seq.filter (fun (_, count) -> count = 2) |> Seq.length
    match pairs with
    | 2 -> { Cards = cards; Bid = bid; HandType = HandType.TwoPair }
    | 1 -> { Cards = cards; Bid = bid; HandType = HandType.OnePair }
    | _ -> { Cards = cards; Bid = bid; HandType = HandType.HighCard }

let compareHand (a: int seq) (b: int seq) =
  let rec compare (x: int seq) (y: int seq) =
    match Seq.toList x, Seq.toList y with
    | x :: _, y :: _ when x > y -> 1
    | x :: _, y :: _ when x < y -> -1
    | _ :: xs, _ :: ys -> compare xs ys
    | _ -> 0

  compare a b

let sort (a: Hand) (b: Hand) =
  match a.HandType, b.HandType with
  | x, y when x = y -> compareHand a.Cards b.Cards
  | x, y when x > y -> 1
  | x, y when x < y -> -1
  | _ -> 0

let parseLine (jsAreJokers: bool) (line: string) =
  line.Split(" ")
  |> fun words -> words[0].ToCharArray() |> Seq.map Char.ToString, int words.[1]
  |> fun (cards, bid) -> cards |> Seq.map (cardToInt jsAreJokers), bid
  |> fun (cards, bid) -> cardCounts jsAreJokers cards, (cards, bid)
  |> toHand

let sum (hands: Hand seq) =
  hands
  |> Seq.sortWith sort
  |> Seq.indexed
  |> Seq.sumBy (fun (idx, hand) -> (idx + 1) * hand.Bid)

let part1() =
  readLines()
  |> Seq.map (parseLine false)
  |> sum

let part2() =
  readLines()
  |> Seq.map (parseLine true)
  |> sum