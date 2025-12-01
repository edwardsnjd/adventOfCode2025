#! /usr/bin/env -S dotnet fsi

open System.Text.RegularExpressions

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let parse (line: string) =
    match line[0] with
    | 'L' -> int (line.Substring 1) * -1
    | 'R' -> int (line.Substring 1)
    | _ -> failwith "unknown"

let initialDial = 50

let turnDial v turn = ((v + turn) % 100 + 100) % 100

let turnDialWithTracking (v, zeros) turn =
    // Flip to always assess crossing 100s in +ve direction
    let from, until = if turn > 0 then v, v + turn else 100 - v, 100 - v - turn
    let from100s = from / 100
    let until100s = until / 100
    let newZeros = until100s - from100s
    turnDial v turn, zeros + newZeros

let part1 lines =
    let turns = List.map parse lines
    let states = List.scan turnDial initialDial turns
    states |> List.filter ((=) 0) |> List.length

let part2 lines =
    let turns = List.map parse lines
    let states = List.scan turnDialWithTracking (initialDial, 0) turns
    states |> List.last |> snd

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
