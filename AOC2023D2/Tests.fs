module Tests

open System.IO
open Faqt
open System
open System.Text.RegularExpressions
open Xunit
open Xunit.Abstractions

module String =
    let split (c: char) (s: string) =
        s.Split(c, StringSplitOptions.RemoveEmptyEntries)

    let trim (s: string) = s.Trim()

type Show = { Cubes: (string * int) list }

module Show =
    let parse s =
        { Cubes =
            s
            |> String.split ','
            |> Seq.map (String.split ' ' >> (fun a -> (String.trim a[1], int <| String.trim a[0])))
            |> Seq.toList }

type Game = { ID: int; Shows: Show list }

module Game =
    let parse s =
        let p = Regex.Match(s, "^Game (\d+):(.+)$")
        let n = int p.Groups.[1].Value
        let m = p.Groups.[2].Value

        { ID = n
          Shows = m |> String.split ';' |> Seq.map Show.parse |> Seq.toList }

    let getMaxes (g: Game) =
        g.Shows
        |> Seq.collect (_.Cubes)
        |> Seq.groupBy fst
        |> Seq.map (fun gp -> (fst gp, snd gp |> Seq.map snd |> Seq.max))
        |> Map.ofSeq

    let isPossible (tehBigShow: Show) (g: Game) =
        let maxes = getMaxes g
        tehBigShow.Cubes |> Seq.forall (fun (c, n) -> maxes.[c] <= n)

    let getPower (g: Game) =
        let maxes = getMaxes g
        maxes |> Map.toSeq |> Seq.map snd |> Seq.fold (*) 1

type TestFixture(output: ITestOutputHelper) =
    [<Fact>]
    let ``Games parse correctly`` () =
        let game =
            Game.parse "Game 1: 2 green, 6 blue, 7 red; 12 green, 6 blue, 3 red; 5 red, 18 green, 4 blue"

        let expected =
            { ID = 1
              Shows =
                [ { Cubes = [ "green", 2; "blue", 6; "red", 7 ] }
                  { Cubes = [ "green", 12; "blue", 6; "red", 3 ] }
                  { Cubes = [ "red", 5; "green", 18; "blue", 4 ] } ] }

        game.Should().Be(expected)

    [<Fact>]
    let ``Calculates game cube maxes correctly`` () =
        let maxes =
            Game.getMaxes
                { ID = 1
                  Shows =
                    [ { Cubes = [ "green", 2; "blue", 1550; "red", 7 ] }
                      { Cubes = [ "green", 12; "blue", 6; "red", 3 ] }
                      { Cubes = [ "red", 5; "green", 18; "blue", 4 ] } ] }

        let expected = Map [ "green", 18; "blue", 1550; "red", 7 ]
        maxes.Should().Be(expected)

    [<Fact>]
    let ``Calculates game cube maxes correctly when some shows only have a few cubes and some are cubeless`` () =
        let maxes =
            Game.getMaxes
                { ID = 1
                  Shows =
                    [ { Cubes = [ "green", 2; "blue", 1550; "red", 7 ] }
                      { Cubes = [ "green", 12 ] }
                      { Cubes = [] } ] }

        let expected = Map [ "green", 12; "blue", 1550; "red", 7 ]
        maxes.Should().Be(expected)

    [<Fact>]
    let ``Game has moar cubes than teh big show`` () =
        let tehBigShow = { Cubes = [ "red", 12; "green", 13; "blue", 14 ] }

        let game =
            { ID = 1
              Shows =
                [ { Cubes = [ "green", 2; "blue", 1550; "red", 7 ] }
                  { Cubes = [ "green", 12; "blue", 6; "red", 3 ] }
                  { Cubes = [ "red", 5; "green", 18; "blue", 4 ] } ] }

        let isValid = Game.isPossible tehBigShow game
        isValid.Should().BeFalse()

    [<Fact>]
    let ``Game has same number of cubes to teh big show`` () =
        let tehBigShow = { Cubes = [ "red", 12; "green", 13; "blue", 14 ] }

        let game =
            { ID = 1
              Shows =
                [ { Cubes = [ "green", 2; "blue", 14; "red", 7 ] }
                  { Cubes = [ "green", 12; "blue", 6; "red", 12 ] }
                  { Cubes = [ "red", 5; "green", 13; "blue", 4 ] } ] }

        let isValid = Game.isPossible tehBigShow game
        isValid.Should().BeTrue()

    [<Fact>]
    let ``Game has fewer cubes than teh big show`` () =
        let tehBigShow = { Cubes = [ "red", 12; "green", 13; "blue", 14 ] }

        let game =
            { ID = 1
              Shows =
                [ { Cubes = [ "green", 2; "blue", 1; "red", 7 ] }
                  { Cubes = [ "green", 3; "blue", 6; "red", 7 ] }
                  { Cubes = [ "red", 5 ] } ] }

        let isValid = Game.isPossible tehBigShow game
        isValid.Should().BeTrue()

    [<Fact>]
    let ``Gets the sum of all the valid games`` () =
        let tehSum =
            File.ReadLines("input")
            |> Seq.map Game.parse
            |> Seq.filter (Game.isPossible { Cubes = [ "red", 12; "green", 13; "blue", 14 ] })
            |> Seq.sumBy (_.ID)

        tehSum.Should().Be(2076)

    [<Fact>]
    let ``Gets the power of a very weak game`` () =
        let game =
            { ID = 1
              Shows = [ { Cubes = [ "red", 1; "green", 1; "blue", 1 ] } ] }

        let power = Game.getPower game
        power.Should().Be(1)

    [<Fact>]
    let ``Gets the power of a fairly impressive game`` () =
        let game =
            { ID = 1
              Shows = [ { Cubes = [ "red", 200; "green", 9; "blue", 5 ] } ] }

        let power = Game.getPower game
        power.Should().Be(9000)

    [<Fact>]
    let ``Gets the power of the input`` () =
        let tehSum =
            File.ReadLines("input")
            |> Seq.map Game.parse
            |> Seq.map Game.getPower
            |> Seq.sum

        tehSum.Should().Be(70950)
