#! /usr/bin/env -S dotnet fsi

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toArray

[<StructuredFormatDisplay("({X},{Y})")>]
type Coordinate =
    { X: int
      Y: int }

    member s.add(dx, dy) : Coordinate = { X = s.X + dx; Y = s.Y + dy }

let neighbours (c: Coordinate) =
    [ for dx in [ -1 .. 1 ] do
          for dy in [ -1 .. 1 ] do
              if dx <> 0 || dx <> dy then
                  yield dx, dy ]
    |> List.map c.add

let coords w h : Coordinate seq =
    seq {
        for y in [ 0 .. h - 1 ] do
            for x in [ 0 .. w - 1 ] do
                yield { X = x; Y = y }
    }

let toGrid = Array.map (Seq.map ((=) '@') >> Seq.toArray)

let toOccupied lines =
    let grid = toGrid lines
    let h = Array.length grid
    let w = Array.head grid |> Array.length
    let hasRoll c = grid.[c.Y].[c.X]
    coords w h |> Seq.filter hasRoll |> set

let isAccessible occupied c =
    let isOccupied c = Set.contains c occupied
    isOccupied c && neighbours c |> Seq.filter isOccupied |> Seq.length < 4

let canRemove occupied =
    occupied |> Seq.filter (isAccessible occupied) |> set

let part1 = toOccupied >> canRemove >> Set.count

let part2 =
    let removeSome occupied =
        let removable = canRemove occupied

        if Set.isEmpty removable then
            None
        else
            Some(removable, Set.difference occupied removable)

    toOccupied >> Seq.unfold removeSome >> Seq.map Set.count >> Seq.sum

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
