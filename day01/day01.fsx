#! /usr/bin/env -S dotnet fsi

open System.Text.RegularExpressions

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let parse (next: string) =
    match next[0] with
    | 'L' -> int (next.Substring 1) * -1
    | 'R' -> int (next.Substring 1)
    | _ -> failwith "unknown"

let initialDial = 50

let turnDial v turn = ((v + turn) % 100 + 100) % 100

let turnDialWithTracking (v, extras) turn =
    let v' = turnDial v turn

    let passing0s =
        // Flip to always assess crossing 100s in +ve direction
        let from, until = if turn > 0 then v, v + turn else 100 - v, 100 - v - turn
        let from100s = from / 100
        let until100s = until / 100
        until100s - from100s

    let doubleCountAdjustment = if passing0s > 0 && v' = 0 then -1 else 0
    let extras' = extras + passing0s + doubleCountAdjustment

    v', extras'

let part1 lines =
    let turns = List.map parse lines
    let vals = List.scan turnDial initialDial turns
    let zeros = vals |> List.filter ((=) 0) |> List.length
    zeros

let part2 lines =
    let turns = List.map parse lines
    let vals = List.scan turnDialWithTracking (initialDial, 0) turns
    let zeros = vals |> List.map fst |> List.filter ((=) 0) |> List.length
    let extras = vals |> List.last |> snd
    zeros + extras

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
