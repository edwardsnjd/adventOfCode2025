#! /usr/bin/env -S dotnet fsi

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let maxWithIndex = List.indexed >> List.sortByDescending snd >> List.head

let toDigits = Seq.map (string >> int) >> Seq.toList
let fromDigits = List.map string >> String.concat "" >> int64

let rec maxNDigits n digits =
    if n < 1 then
        []
    else
        let pool = List.take (List.length digits - n + 1) digits
        let indexOfMax, maxValue = maxWithIndex pool
        let digits' = List.skip (indexOfMax + 1) digits in
        maxValue :: maxNDigits (n - 1) digits'

let joltage numDigits = toDigits >> maxNDigits numDigits >> fromDigits

let part1 = List.map (joltage 2) >> List.sum
let part2 = List.map (joltage 12) >> List.sum

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
