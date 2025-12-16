#! /usr/bin/env -S dotnet fsi

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let parseLine (line: string) =
    let parts = line.Split ": "
    parts.[0], parts.[1].Split " " |> Array.toList

let parse lines =
    lines
    |> List.map parseLine
    |> List.fold (fun acc (node, deps) -> Map.add node deps acc) Map.empty

let countPaths from target maxDepth (connections: Map<string, string list>) =
    let rec search frontier total =
        match frontier with
        | [] -> total
        | (node, depth) :: rest ->
            let total' = if node = target then total + 1L else total

            let frontier' =
                if depth >= maxDepth then
                    rest
                else
                    let connected =
                        match Map.tryFind node connections with
                        | Some deps -> deps |> List.map (fun n -> n, depth + 1)
                        | None -> []

                    connected @ rest

            search frontier' total'

    search [ from, 0 ] 0L

let part1 lines =
    let connections = parse lines
    countPaths "you" "out" 100000 connections

let part2 lines =
    let connections = parse lines

    // NOTE: Depths are taken from real input, but example is smaller so fits
    [ countPaths "svr" "fft" 10 connections
      countPaths "fft" "dac" 18 connections
      countPaths "dac" "out" 9 connections ]
    |> List.fold (*) 1L

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
