open System.IO

module Map =
    let combine = Map.fold (fun acc key value -> Map.add key value acc)

module String =
    let firstOf (strs: string seq) (s: string) =
        strs
        |> Seq.map (fun str -> (str, s.IndexOf(str)))
        |> Seq.filter (fun (_, i) -> i >= 0)
        |> Seq.minBy snd
        |> fst

    let lastOf (strs: string seq) (s: string) =
        strs
        |> Seq.map (fun str -> (str, s.LastIndexOf(str)))
        |> Seq.filter (fun (_, i) -> i >= 0)
        |> Seq.maxBy snd
        |> fst

let numbersOLD = "0123456789".ToCharArray()

let getCalibrationValueOLD (s: string) =
    let first = s.IndexOfAny(numbersOLD)
    let last = s.LastIndexOfAny(numbersOLD)
    $"%c{s.[first]}%c{s.[last]}" |> int

let numbers =
    Seq.init 10 id
    |> Seq.map (fun i -> ($"%d{i}", i))
    |> Map.ofSeq
    |> Map.combine (
        Map
            [ ("zero", 0)
              ("one", 1)
              ("two", 2)
              ("three", 3)
              ("four", 4)
              ("five", 5)
              ("six", 6)
              ("seven", 7)
              ("eight", 8)
              ("nine", 9) ]
    )

let getCalibrationValue (s: string) =
    let first = String.firstOf numbers.Keys s
    let last = String.lastOf numbers.Keys s
    numbers.[first] * 10 + numbers.[last]

File.ReadLines("input")
|> Seq.map getCalibrationValue
|> Seq.sum
|> printfn "%d"
