open System
open System.IO

type CharPosition = { c: char; line: int; column: int}
type Digit = { value: int; line: int; columns: int seq}

let readLines() = File.ReadAllLines("Day3Input.txt")

let isSymbolOrDigit (c: char) = c <> '.'

let DigitsAndSymbols (lineNumber: int) (line: string) =
  line
  |> Seq.indexed
  |> Seq.filter (fun (_, c) -> isSymbolOrDigit c)
  |> Seq.map (fun (column, c) -> { c = c; line = lineNumber; column = column })

let isAdjacent a b = abs(a - b) <= 1
let isAdjacentAny bs a = bs |> Seq.exists (fun b -> isAdjacent a b)

let charPositionsToDigit (charPositions: CharPosition list) =
  let joinString (separator: string) (chars: char seq) = String.Join(separator, chars)

  { value = charPositions |> Seq.rev |> Seq.map (fun c -> c.c) |> joinString "" |> int
    line = charPositions |> Seq.head |> fun c -> c.line
    columns = charPositions |> Seq.map (fun c -> c.column) |> Seq.rev }

// This is slow as hell
let buildDigits (created: Digit list, inProgress: CharPosition list, remaining: CharPosition seq) (digit: CharPosition) =
  if Seq.isEmpty remaining || Seq.isEmpty (Seq.tail remaining) then
    (charPositionsToDigit inProgress :: created, [], remaining)
  else
    let nextAfterDigit = remaining |> Seq.tail |> Seq.head
    let remaining' = Seq.tail remaining
    match inProgress with
    | [] -> (created, [digit], remaining')
    | _ ->
      if nextAfterDigit.line = digit.line && nextAfterDigit.column = digit.column + 1 then
        (created, nextAfterDigit :: inProgress, remaining')
      else
        (charPositionsToDigit inProgress :: created, [nextAfterDigit], remaining')

let isAdjacentToSymbol (symbols: CharPosition seq) (d: Digit) =
  symbols |> Seq.exists (fun s -> isAdjacent d.line s.line && isAdjacentAny d.columns s.column)

let part1() =
  let charPositions =
    readLines()
    |> Seq.indexed
    |> Seq.collect (fun (lineNumber, line) -> DigitsAndSymbols lineNumber line)
  
  let symbols = charPositions |> Seq.filter (fun c -> not(Char.IsDigit c.c))

  let digits =
    charPositions
    |> Seq.filter (fun c -> Char.IsDigit c.c)
    |> Seq.sortBy (fun c -> c.line, c.line)
    |> fun cs -> Seq.fold buildDigits ([], [Seq.head cs], cs) cs
    |> fun (created, _, _) -> created

  digits |> Seq.filter (isAdjacentToSymbol symbols) |> Seq.sumBy (fun d -> d.value)