#! /usr/bin/env -S dotnet fsi

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

[<StructuredFormatDisplay("({X},{Y})")>]
type Coordinate =
    { X: int
      Y: int }

    member s.add(dx, dy) : Coordinate = { X = s.X + dx; Y = s.Y + dy }

    member s.neighbours: Coordinate list =
        [ for dx in [ -1 .. 1 ] do
              for dy in [ -1 .. 1 ] do
                  if dx <> 0 || dx <> dy then
                      yield dx, dy ]
        |> List.map s.add

type Shape = Coordinate array

type Region =
    { width: int
      height: int
      counts: int list }

let parseShape (lines: string array) : Shape =
    let h, w = Array.length lines, String.length (Array.head lines)

    [| for x in 0 .. w - 1 do
           for y in 0 .. h - 1 do
               if lines[y][x] = '#' then
                   yield { X = x; Y = y } |]

let parseRegion (line: string) =
    match line.Split ": " with
    | [| sizeString; countsString |] ->
        let width, height =
            match sizeString.Split "x" with
            | [| x; y |] -> int x, int y
            | _ -> failwith "invalid size"

        let counts = countsString.Split " " |> Array.map int |> Array.toList

        { width = width
          height = height
          counts = counts }
    | _ -> failwith "Invalid region"

let parse lines =
    let numberOfShapes = 6
    let shapeLines = 5
    let shapeSize = 3

    let getShape i =
        lines |> List.skip (i * shapeLines + 1) |> List.take shapeSize |> List.toArray

    let shapes =
        [ 0 .. (numberOfShapes - 1) ]
        |> List.map (getShape >> parseShape)
        |> List.toArray

    let regions =
        lines |> List.skip (numberOfShapes * shapeLines) |> List.map parseRegion

    let shapeCells = Array.map Array.length shapes

    let minimumRegionArea r =
        List.mapi (fun i c -> c * shapeCells[i]) r.counts |> List.sum

    let regionArea r = r.width * r.height
    let viableArea r = regionArea r >= minimumRegionArea r

    regions |> List.filter viableArea |> List.length

let part1 = parse

lines |> part1 |> printfn "Part1: %A"
