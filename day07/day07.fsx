#! /usr/bin/env -S dotnet fsi

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let split (position: int) (beam: bool) (line: string) : int list =
    match line.[position] with
    | 'S' -> [ position ]
    | '^' -> if beam then [ position - 1; position + 1 ] else []
    | '.' -> if beam then [ position ] else []
    | _ -> failwith "unexpected char"

let handle (beamsBefore: Set<int>, splitsBefore: int) (line: string) : (Set<int> * int) =
    line
    |> Seq.indexed
    |> Seq.fold
        (fun (beams, splits) (i, _) ->
            let positions' = split i (Set.contains i beamsBefore) line
            let beams' = Set.union (set positions') beams
            let splits' = if List.length positions' > 1 then splits + 1 else splits
            beams', splits')
        (Set.empty, splitsBefore)

let memoizeRec (f: ('a -> 'b) -> 'a -> 'b) =
    let cache = System.Collections.Generic.Dictionary<'a, 'b>()

    let rec inner x =
        match cache.TryGetValue x with
        | true, value -> value
        | false, _ ->
            let value = f inner x
            cache.[x] <- value
            value

    inner

let totalPaths =
    memoizeRec (fun self (lines, pos) ->
        match lines with
        | [] -> 1L
        | line :: rest ->
            let positions = split pos true line
            List.map (fun p -> self (rest, p)) positions |> List.sum)

let part1 = List.fold handle (Set.empty, 0) >> snd

let part2 lines =
    let s = List.head lines |> fun (s: string) -> s.IndexOf 'S'
    totalPaths (lines, s)

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
