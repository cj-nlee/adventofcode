module Tests

open System
open System.IO
open System.Linq
open Faqt
open FParsec
open Xunit
open Xunit.Abstractions

module Seq =
    let intersect (xs:'a seq) (ys: 'a seq) = xs.Intersect(ys) // lol thx c#

type Card =
    { ID: int
      HaveNums: int list
      WinNums: int list }
    
type 'a Countable =
    { Item: 'a
      mutable Count: int }
    
let pNumber = pint32 .>> spaces

let pCard =
    pstring "Card" >>. spaces >>. pNumber .>> pstring ":" .>> spaces
    .>>. (many pNumber)
    .>> spaces .>> pstring "|" .>> spaces
    .>>. (many pNumber)
    .>> spaces
    |>> (fun ((id, haveNums), winNums) ->
        { ID = id
          HaveNums = haveNums
          WinNums = winNums })

let pCards: Parser<Card list, unit> = many pCard .>> eof

let getWinningNumberCount card =
    Seq.intersect card.HaveNums card.WinNums
    |> Seq.length

let getCardScore card =
    let count = getWinningNumberCount card
    if count = 0 then
        0
    else
        Convert.ToInt32(Math.Pow(2.0, count - 1 |> float))

let getScore cards =
    cards |> Seq.sumBy getCardScore

let getWaterfallScore cards =
    let cards' = cards |> Seq.map (fun x -> { Item = x; Count = 1 }) |> Seq.toArray
    let rec inner cardsToCheck i =
        match cardsToCheck with
        | [] -> ()
        | { Item = card; Count = c }::xs ->
            let score = getWinningNumberCount card
            for j in (i + 1)..(i + score) do
                cards'.[j].Count <- cards'.[j].Count + c
            inner xs (i + 1)
    inner (cards' |> List.ofArray) 0
    cards' |> Seq.sumBy (_.Count)

type TestFixture(output: ITestOutputHelper) =
    [<Fact>]
    let ``Parses one card`` () =
        let cards = run pCards "Card 1: 1 2 3 | 4 5 6"

        match cards with
        | Success(s, _, _) ->
            s
                .Should()
                .Be(
                    [ { ID = 1
                        HaveNums = [ 1; 2; 3 ]
                        WinNums = [ 4; 5; 6 ] } ]
                )
        | Failure _ -> failwith "Failed to parse"
        
    [<Fact>]
    let ``Parses a few cards`` () =
        let cards = run pCards "Card 1: 1 2 3 | 4 5 6\nCard 2: 1 2 3 | 4 5 6\nCard 3: 1 2 3 | 4 5 6\n"

        match cards with
        | Success(s, _, _) ->
            s
                .Should()
                .Be(
                    [ { ID = 1
                        HaveNums = [ 1; 2; 3 ]
                        WinNums = [ 4; 5; 6 ] }
                      { ID = 2
                        HaveNums = [ 1; 2; 3 ]
                        WinNums = [ 4; 5; 6 ] }
                      { ID = 3
                        HaveNums = [ 1; 2; 3 ]
                        WinNums = [ 4; 5; 6 ] } ]
                )
        | Failure _ -> failwith "Failed to parse"
        
    [<Fact>]
    let ``Gets score for one card with no overlap`` () =
        let cards = [ { ID = 1
                        HaveNums = [ 1; 2; 3 ]
                        WinNums = [ 4; 5; 6 ] } ]
        let score = getScore cards
        score.Should().Be(0)
        
    [<Fact>]
    let ``Gets score for one card with one overlap`` () =
        let cards = [ { ID = 1
                        HaveNums = [ 1; 2; 3 ]
                        WinNums = [ 3; 5; 6 ] } ]
        let score = getScore cards
        score.Should().Be(1)
        
    [<Fact>]
    let ``Gets score for one card with all overlap`` () =
        let cards = [ { ID = 1
                        HaveNums = [ 1; 2; 3 ]
                        WinNums = [ 1; 2; 3 ] } ]
        let score = getScore cards
        score.Should().Be(4)
        
    [<Fact>]
    let ``Gets score for example`` () =
        let cards = File.ReadAllText("example") |> run pCards
        match cards with
        | Success(s, _, _) ->
            let score = getScore s
            score.Should().Be(13)
        | Failure _ -> failwith "Failed to parse"
        
    [<Fact>]
    let ``Gets score for input`` () =
        let cards = File.ReadAllText("input") |> run pCards
        match cards with
        | Success(s, _, _) ->
            let score = getScore s
            score.Should().Be(26443)
        | Failure _ -> failwith "Failed to parse"
        
    [<Fact>]
    let ``Gets waterfall score for example`` () =
        let cards = File.ReadAllText("example") |> run pCards
        match cards with
        | Success(s, _, _) ->
            let score = getWaterfallScore s
            score.Should().Be(30)
        | Failure _ -> failwith "Failed to parse"
        
    [<Fact>]
    let ``Gets waterfall score for input`` () =
        let cards = File.ReadAllText("input") |> run pCards
        match cards with
        | Success(s, _, _) ->
            let score = getWaterfallScore s
            score.Should().Be(6284877)
        | Failure _ -> failwith "Failed to parse"
        
    [<Fact>]
    let ``No cards in the example have 0 score`` () =
        let cards = File.ReadAllText("example") |> run pCards
        match cards with
        | Success(s, _, _) ->
            let scores = s |> Seq.map getCardScore
            let areAnyNonZero = scores |> Seq.forall ((<>) 0)
            areAnyNonZero.Should().BeFalse()
        | Failure _ -> failwith "Failed to parse"
        
    [<Fact>]
    let ``No cards in the input have 0 score`` () =
        let cards = File.ReadAllText("input") |> run pCards
        match cards with
        | Success(s, _, _) ->
            let scores = s |> Seq.map getCardScore
            let areAnyNonZero = scores |> Seq.forall ((<>) 0)
            areAnyNonZero.Should().BeFalse()
        | Failure _ -> failwith "Failed to parse"
