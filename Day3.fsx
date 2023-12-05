open System
open System.IO

type CharPosition = { C: char; Line: int; Column: int}
type Digit = { Value: int; Line: int; Columns: int seq}

let readLines() = File.ReadAllLines("Day3Input.txt")

let isSymbolOrDigit (c: char) = c <> '.'

let digitsAndSymbols (lineNumber: int) (line: string) =
  line
  |> Seq.indexed
  |> Seq.filter (fun (_, c) -> isSymbolOrDigit c)
  |> Seq.map (fun (column, c) -> { C = c; Line = lineNumber; Column = column })

let charPositions (lines: string seq) =
  lines
  |> Seq.indexed
  |> Seq.collect (fun (lineNumber, line) -> digitsAndSymbols lineNumber line)

let isAdjacent a b = abs(a - b) <= 1
let isAdjacentAny bs a = bs |> Seq.exists (fun b -> isAdjacent a b)

let charPositionsToDigit (charPositions: CharPosition list) =
  let joinString (separator: string) (chars: char seq) = String.Join(separator, chars)

  { Value = charPositions |> Seq.rev |> Seq.map (fun c -> c.C) |> joinString "" |> int
    Line = charPositions |> Seq.head |> fun c -> c.Line
    Columns = charPositions |> Seq.map (fun c -> c.Column) |> Seq.rev }

// This is slow as hell - why?
let buildDigits (created: Digit list, inProgress: CharPosition list, remaining: CharPosition seq) (digit: CharPosition) =
  if Seq.isEmpty remaining || Seq.isEmpty (Seq.tail remaining) then
    (charPositionsToDigit inProgress :: created, [], remaining)
  else
    let nextAfterDigit = remaining |> Seq.tail |> Seq.head
    let remaining' = Seq.tail remaining
    match inProgress with
    | [] -> (created, [digit], remaining')
    | _ ->
      if nextAfterDigit.Line = digit.Line && nextAfterDigit.Column = digit.Column + 1 then
        (created, nextAfterDigit :: inProgress, remaining')
      else
        (charPositionsToDigit inProgress :: created, [nextAfterDigit], remaining')

let calcDigits (charPositions: CharPosition seq) =
  charPositions
  |> Seq.filter (fun c -> Char.IsDigit c.C)
  |> Seq.sortBy (fun c -> c.Line, c.Line)
  |> fun cs -> Seq.fold buildDigits ([], [Seq.head cs], cs) cs
  |> fun (created, _, _) -> created

let part1() =
  let charPositions = readLines() |> charPositions
  let symbols = charPositions |> Seq.filter (fun c -> not(Char.IsDigit c.C))
  let digits = calcDigits charPositions

  let isAdjacentToSymbol (symbols: CharPosition seq) (d: Digit) =
    symbols |> Seq.exists (fun s -> isAdjacent d.Line s.Line && isAdjacentAny d.Columns s.Column)

  digits |> Seq.filter (isAdjacentToSymbol symbols) |> Seq.sumBy (fun d -> d.Value)

let part2() =
  let charPositions = readLines() |> charPositions
  let gears = charPositions |> Seq.filter (fun c -> c.C = '*')
  let digits = calcDigits charPositions

  let adjacentDigits (digits: Digit seq) (gear: CharPosition) =
   digits |> Seq.filter (fun d -> isAdjacent d.Line gear.Line && isAdjacentAny d.Columns gear.Column)

  gears
  |> Seq.map (adjacentDigits digits)
  |> Seq.filter (fun gearDigits -> Seq.length gearDigits = 2)
  |> Seq.map (fun ds -> ds |> Seq.map (fun d -> d.Value) |> Seq.reduce (*) )
  |> Seq.sum
