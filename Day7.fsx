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

type Hand = { Cards: int seq; Bid: int; handType: HandType }

let readLines() = File.ReadAllLines("Day7Input.txt")

let cardToInt (c: string) =
  match c with
  | "T" -> 10
  | "J" -> 11
  | "Q" -> 12
  | "K" -> 13
  | "A" -> 14
  | _ -> int c

let toHand (cards: int seq, bid: int) =
  let cardCounts = Seq.countBy id cards

  if Seq.exists (fun (_, count) -> count = 5) cardCounts then { Cards = cards; Bid = bid; handType = HandType.FiveOfAKind }
  else if Seq.exists (fun (_, count) -> count = 4) cardCounts then { Cards = cards; Bid = bid; handType = HandType.FourOfAKind }
  else if Seq.exists (fun (_, count) -> count = 3) cardCounts && Seq.exists (fun (_, count) -> count = 2) cardCounts then { Cards = cards; Bid = bid; handType = HandType.FullHouse }
  else if Seq.exists (fun (_, count) -> count = 3) cardCounts then { Cards = cards; Bid = bid; handType = HandType.ThreeOfAKind }
  else
    let pairs = cardCounts |> Seq.filter (fun (_, count) -> count = 2) |> Seq.length
    match pairs with
    | 2 -> { Cards = cards; Bid = bid; handType = HandType.TwoPair }
    | 1 -> { Cards = cards; Bid = bid; handType = HandType.OnePair }
    | _ -> { Cards = cards; Bid = bid; handType = HandType.HighCard }

let compareHand (a: int seq) (b: int seq) =
  let rec compare (x: int seq) (y: int seq) =
    match Seq.toList x, Seq.toList y with
    | x :: _, y :: _ when x > y -> 1
    | x :: _, y :: _ when x < y -> -1
    | _ :: xs, _ :: ys -> compare xs ys
    | _ -> 0

  compare a b

let part1() =
  readLines()
  |> Seq.map (fun line -> line.Split(" "))
  |> Seq.map (fun words -> words[0].ToCharArray() |> Seq.map Char.ToString, int words.[1])
  |> Seq.map (fun (cards, bid) -> cards |> Seq.map cardToInt, bid)
  |> Seq.map toHand
  |> Seq.sortWith (fun a b -> 
    match a.handType, b.handType with
    | x, y when x = y -> compareHand a.Cards b.Cards
    | x, y when x > y -> 1
    | x, y when x < y -> -1
    | _ -> 0
  )
  |> Seq.indexed
  |> Seq.sumBy (fun (idx, hand) -> (idx + 1) * hand.Bid)
