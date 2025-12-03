#! /usr/bin/env -S dotnet fsi

let tap fn x =
    fn x
    x

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let joltage numDigits line =
    let inputDigits = Seq.map (string >> int) line |> Seq.toList

    let rec groups (n: int) (digits: int list) : int list list =
        match n with
        | 0 -> List.empty
        | 1 -> digits |> List.map (fun n -> [ n ])
        | _ ->
            digits
            |> List.mapi (fun i d -> d, List.skip (i + 1) digits)
            |> List.collect (fun (current, rest) -> groups (n - 1) rest |> List.map (fun r -> current :: r))

    let toNum digits =
        digits |> List.map string |> String.concat "" |> int64

    inputDigits |> groups numDigits |> Seq.map toNum |> Seq.max

let part1 lines =
    lines |> List.map (joltage 2) |> List.sum

let part2 lines =
    lines |> List.map (joltage 12) |> List.sum

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
