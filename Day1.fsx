open System
open System.IO

module String =
  let private indexAndValueOfAnyBy idxFun comparisonFun (strs: string seq) (str: string) =
    let wordsAndIdxs =
      strs
      |> Seq.map (fun x -> x, idxFun str x)
      |> Seq.filter (fun (_, idx) -> idx <> -1)
    
    if Seq.isEmpty wordsAndIdxs then
      Error()
    else
      Ok(comparisonFun wordsAndIdxs)

  let indexAndValueOfAny (strs: string seq) (str: string) =
    indexAndValueOfAnyBy
      (fun str x -> str.IndexOf(x))
      (fun wordsAndIdxs -> Seq.minBy (fun (_, idx) -> idx) wordsAndIdxs)
      strs
      str
 
  let lastIndexAndValueOfAny (strs: string seq) (str: string) =
    indexAndValueOfAnyBy
      (fun str x -> str.LastIndexOf(x))
      (fun wordsAndIdxs -> Seq.maxBy (fun (_, idx) -> idx) wordsAndIdxs)
      strs
      str

let readLines() = File.ReadAllLines("Day1Input.txt")

let sumOfCalibrationValues (lines: string seq) =
  lines
  |> Seq.map (fun x -> 
    let digits = x |> String.filter Char.IsDigit
    let first, last = Seq.head digits, Seq.last digits
    $"{first}{last}" |> Int32.Parse
  )
  |> Seq.sum

let replaceFirstAndLastWordsWithDigits (line: string) =
  let wordsToDigits =
    Map.empty
      .Add("one", "1")
      .Add("two", "2")
      .Add("three", "3")
      .Add("four", "4")
      .Add("five", "5")
      .Add("six", "6")
      .Add("seven", "7")
      .Add("eight", "8")
      .Add("nine", "9")

  let words = Map.keys wordsToDigits

  let indexAndValueOfAnyAndReplace (idxAndValueFun: string seq -> string -> Result<string * int, unit>) (words: string seq) (line: string) =
    match idxAndValueFun words line with
    | Ok (word, idx) -> line.Remove(idx, (String.length word) - 1).Insert(idx, wordsToDigits.[word]) // -1 as last letter of a digit word might be the first of the next word
    | Error _ -> line
  
  line |> indexAndValueOfAnyAndReplace String.indexAndValueOfAny words |> indexAndValueOfAnyAndReplace String.lastIndexAndValueOfAny words

let part1 = readLines >> sumOfCalibrationValues
let part2 = readLines >> Seq.map replaceFirstAndLastWordsWithDigits >> sumOfCalibrationValues