#! /usr/bin/env -S dotnet fsi

open System.Text.RegularExpressions

let line =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList
    |> Seq.head

let parse (line: string) =
    line
    |> (fun s -> s.Split ",")
    |> Array.map (fun s -> s.Split "-" |> Array.map int64)
    |> Array.map (fun a -> a[0], a[1])
    |> Array.toList

let isInvalid1 (s: string) = Regex.IsMatch(s, @"^(.+)\1$")
let isInvalid2 (s: string) = Regex.IsMatch(s, @"^(.+)\1{1,}$")

let invalidIds predicate (from, until) =
    [ from..until ] |> List.filter (string >> predicate)

let part1 line =
    parse line |> List.collect (invalidIds isInvalid1) |> List.sum

let part2 line =
    parse line |> List.collect (invalidIds isInvalid2) |> List.sum

line |> part1 |> printfn "Part1: %A"
line |> part2 |> printfn "Part2: %A"
