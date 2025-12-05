#! /usr/bin/env -S dotnet fsi

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let parseAll lines =
    let parse (ranges, ids) (line: string) =
        match line.Split "-" with
        | [| "" |] -> ranges, ids
        | [| id |] -> ranges, int64 id :: ids
        | [| from; until |] -> (int64 from, int64 until) :: ranges, ids
        | _ -> ranges, ids

    List.fold parse ([], []) lines

let inRange id (from, until) = from <= id && id <= until

let mergeRanges rs (from, until) =
    match rs with
    | [] -> [ (from, until) ]
    | (a, b) :: rest ->
        if from <= b then
            (a, max b until) :: rest
        else
            (from, until) :: rs

let part1 lines =
    let ranges, ids = parseAll lines
    let validIds = Seq.filter (fun id -> List.exists (inRange id) ranges) ids
    Seq.length validIds

let part2 lines =
    let ranges, _ = parseAll lines
    let mergedRanges = ranges |> List.sort |> List.fold mergeRanges []
    mergedRanges |> Seq.map (fun (a, b) -> b - a + 1L) |> Seq.sum

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
