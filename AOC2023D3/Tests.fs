module Tests

open System
open System.IO
open Faqt
open Xunit
open Xunit.Abstractions

type Break = { Line: int }

type Part = { Line: int; ColStart: int; ColEnd: int; ID: string }

let between (x, y) i = i >= x && i <= y

module Part =
    let isPointAdjacent (p: Part) (line: int) (col: int) =
        between (p.Line - 1, p.Line + 1) line && between (p.ColStart - 1, p.ColEnd + 1) col

type Symbol = { Line: int; Col: int; Char: char }

type Token =
| Break of Break
| Part of Part
| Symbol of Symbol

type Schematic = { Tokens: Token list }

module Schematic =
    let parse (text: string) =
        let rec extract (tokens: Token list) (line: int) (col: int) (chars: char list) =
            match chars with
            | c::cs when Char.IsDigit c ->
                match tokens with
                | []           -> extract [Part { Line = line; ColStart = col; ColEnd = col; ID = $"%c{c}" }]         line (col + 1) cs
                | Break  _::_  -> extract (Part { Line = line; ColStart = col; ColEnd = col; ID = $"%c{c}" }::tokens) line (col + 1) cs
                | Symbol _::_  -> extract (Part { Line = line; ColStart = col; ColEnd = col; ID = $"%c{c}" }::tokens) line (col + 1) cs
                | Part   x::xs -> extract (Part { x with ColEnd = col; ID = $"%s{x.ID}%c{c}" }::xs)                   line (col + 1) cs
            | '\n'::cs ->
                extract ((Break { Line = line })::tokens) (line + 1) 0 cs
            | '.'::cs ->
                match tokens with
                | Part _::_ -> extract ((Break { Line = line })::tokens) line (col + 1) cs
                | _         -> extract tokens                            line (col + 1) cs
            | c::cs ->
                extract (Symbol { Line = line; Col = col; Char = c }::tokens) line (col + 1) cs
            | [] -> tokens
        { Tokens = text |> Seq.toList |> extract [] 0 0 }
        
    let getAdjacentParts (sch: Schematic) =
        let rec getAdjacentParts' (tokens: Token list) (adjacent: Part list) =
            match tokens with
            | [] -> adjacent
            | Break _::xs -> getAdjacentParts' xs adjacent
            | Part  x::xs ->
                if sch.Tokens |> List.exists (function Symbol s -> Part.isPointAdjacent x s.Line s.Col | _ -> false) then
                    getAdjacentParts' xs (x::adjacent)
                else
                    getAdjacentParts' xs adjacent
            | Symbol _::xs -> getAdjacentParts' xs adjacent
        getAdjacentParts' sch.Tokens []
        
    let getGears (sch: Schematic) =
        let gearSymbols = sch.Tokens |> Seq.choose (function Symbol s when s.Char = '*' -> Some s | _ -> None)
        let rec getPartsNextToThisGear (tokens: Token list) (gear: Symbol) (parts: Part list) =
            match tokens with
            | [] -> parts
            | Break _::xs -> getPartsNextToThisGear xs gear parts
            | Part  x::xs ->
                if Part.isPointAdjacent x gear.Line gear.Col then
                    getPartsNextToThisGear xs gear (x::parts)
                else
                    getPartsNextToThisGear xs gear parts
            | Symbol _::xs -> getPartsNextToThisGear xs gear parts
        gearSymbols
        |> Seq.map (fun g -> getPartsNextToThisGear sch.Tokens g [])
        |> Seq.filter (fun gs -> gs.Length = 2)
        |> Seq.map (fun gs -> gs |> Seq.map (fun g -> int g.ID) |> Seq.reduce (*))
        |> Seq.sum

type TestFixture(output: ITestOutputHelper) =
    [<Theory>]
    [<InlineData(11, 46)>] // up
    [<InlineData(13, 46)>] // down
    [<InlineData(12, 45)>] // left
    [<InlineData(12, 47)>] // right
    [<InlineData(12, 46)>] // shouldn't happen..
    [<InlineData(11, 45)>] // up left
    [<InlineData(11, 47)>] // up right
    [<InlineData(13, 45)>] // down left
    [<InlineData(13, 47)>] // down right
    let ``Point is near single character part`` (x: int, y: int) =
        let part = { Line = 12; ColStart = 46; ColEnd = 46; ID = "1" }
        let adjacent = Part.isPointAdjacent part x y
        adjacent.Should().BeTrue()
        
    [<Theory>]
    [<InlineData(11, 45, true)>]
    [<InlineData(11, 46, true)>]
    [<InlineData(11, 47, true)>]
    [<InlineData(11, 48, true)>]
    [<InlineData(11, 49, true)>]
    [<InlineData(12, 45, true)>]
    [<InlineData(12, 46, true)>]
    [<InlineData(12, 47, true)>]
    [<InlineData(12, 48, true)>]
    [<InlineData(12, 49, true)>]
    [<InlineData(13, 45, true)>]
    [<InlineData(13, 46, true)>]
    [<InlineData(13, 47, true)>]
    [<InlineData(13, 48, true)>]
    [<InlineData(13, 49, true)>]
    
    [<InlineData(10, 45, false)>]
    [<InlineData(10, 46, false)>]
    [<InlineData(10, 47, false)>]
    [<InlineData(10, 48, false)>]
    [<InlineData(10, 49, false)>]
    [<InlineData(14, 45, false)>]
    [<InlineData(14, 46, false)>]
    [<InlineData(14, 47, false)>]
    [<InlineData(14, 48, false)>]
    [<InlineData(14, 49, false)>]
    
    [<InlineData(10, 44, false)>]
    [<InlineData(11, 44, false)>]
    [<InlineData(12, 44, false)>]
    [<InlineData(13, 44, false)>]
    [<InlineData(14, 44, false)>]
    
    [<InlineData(10, 50, false)>]
    [<InlineData(11, 50, false)>]
    [<InlineData(12, 50, false)>]
    [<InlineData(13, 50, false)>]
    [<InlineData(14, 50, false)>]
    let ``Points near and not near a multi character part`` (x: int, y: int, near: bool) =
        let part = { Line = 12; ColStart = 46; ColEnd = 48; ID = "123" }
        let adjacent = Part.isPointAdjacent part x y
        adjacent.Should().Be(near)
        
    [<Theory>]
    [<InlineData(10, 46)>] // up
    [<InlineData(0, 46)>] // up more
    [<InlineData(14, 46)>] // down
    [<InlineData(25454, 46)>] // down more
    [<InlineData(12, 44)>] // left
    [<InlineData(12, 19)>] // left more
    [<InlineData(12, 48)>] // right
    [<InlineData(12, 4845)>] // right more
    let ``Point is not near`` (x: int, y: int) =
        let part = { Line = 12; ColStart = 46; ColEnd = 46; ID = "1" }
        let adjacent = Part.isPointAdjacent part x y
        adjacent.Should().BeFalse()
        
    [<Fact>]
    let ``Parses single character part`` () =
        let text = "1"
        let schematic = Schematic.parse text
        schematic.Tokens.Should().Be([ Part { Line = 0; ColStart = 0; ColEnd = 0; ID = "1" } ])
        
    [<Fact>]
    let ``Parses just one part`` () =
        let text = "123"
        let schematic = Schematic.parse text
        schematic.Tokens.Should().Be([ Part { Line = 0; ColStart = 0; ColEnd = 2; ID = "123" } ])
        
    [<Fact>]
    let ``Parses two parts on different lines`` () =
        let text = "123\n456"
        let schematic = Schematic.parse text
        schematic.Tokens.Should().Be([
            Part { Line = 1; ColStart = 0; ColEnd = 2; ID = "456" }
            Break { Line = 0 }
            Part { Line = 0; ColStart = 0; ColEnd = 2; ID = "123" }
        ])
        
    [<Fact>]
    let ``Parses two parts on very different lines`` () =
        let text = ".1233.\n\n...\n.....456"
        let schematic = Schematic.parse text
        schematic.Tokens.Should().Be([
            Part { Line = 3; ColStart = 5; ColEnd = 7; ID = "456" }
            Break { Line = 2 }
            Break { Line = 1 }
            Break { Line = 0 }
            Break { Line = 0 }
            Part { Line = 0; ColStart = 1; ColEnd = 4; ID = "1233" }
        ])
        
    [<Fact>]
    let ``Gets parts that are right next to symbols`` () =
        let text = "123$"
        let schematic = Schematic.parse text
        let adjacent = Schematic.getAdjacentParts schematic
        adjacent.Should().Be([ { Line = 0; ColStart = 0; ColEnd = 2; ID = "123" } ])
        
    [<Fact>]
    let ``No parts are near symbols`` () =
        let text = "123.$"
        let schematic = Schematic.parse text
        let adjacent = Schematic.getAdjacentParts schematic
        adjacent.Should().Be([])
        
    [<Fact>]
    let ``Two parts are diagonally adjacent to the same symbol`` () =
        let text = "123\n...$\n....456"
        let schematic = Schematic.parse text
        let adjacent = Schematic.getAdjacentParts schematic
        adjacent.Should().Be([
            { Line = 0; ColStart = 0; ColEnd = 2; ID = "123" }
            { Line = 2; ColStart = 4; ColEnd = 6; ID = "456" }
        ])
        
    [<Fact>]
    let ``Only one part is diagonally adjacent to the symbol`` () =
        let text = "123\n....$\n.....456"
        let schematic = Schematic.parse text
        let adjacent = Schematic.getAdjacentParts schematic
        adjacent.Should().Be([
            { Line = 2; ColStart = 5; ColEnd = 7; ID = "456" }
        ])
        
    [<Fact>]
    let ``One gear next to another`` () =
        let text = "123*456"
        let schematic = Schematic.parse text
        let gears = Schematic.getGears schematic
        gears.Should().Be(123*456)
        
    [<Fact>]
    let ``Gets the example answer`` () =
        let input = File.ReadAllText("example")
        let schematic = Schematic.parse input
        let adjacent = Schematic.getAdjacentParts schematic
        let partSum = adjacent |> List.sumBy (fun p -> int p.ID)
        partSum.Should().Be(4361)
        
    [<Fact>]
    let ``Gets the input answer`` () =
        let input = File.ReadAllText("input")
        let schematic = Schematic.parse input
        let adjacent = Schematic.getAdjacentParts schematic
        let partSum = adjacent |> List.sumBy (fun p -> int p.ID)
        partSum.Should().BeGreaterThan(67451) |> ignore
        partSum.Should().BeLessThan(535946) |> ignore
        partSum.Should().Be(533784)
        
    [<Fact>]
    let ``Gets the example answer for part 2`` () =
        let input = File.ReadAllText("example")
        let schematic = Schematic.parse input
        let gears = Schematic.getGears schematic
        gears.Should().Be(467835)
        
    [<Fact>]
    let ``Gets the input answer for part 2`` () =
        let input = File.ReadAllText("input")
        let schematic = Schematic.parse input
        let gears = Schematic.getGears schematic
        gears.Should().Be(78826761)
