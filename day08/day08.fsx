#! /usr/bin/env -S dotnet fsi

let parse (line: string) =
    line.Split ","
    |> function
        | [| a; b; c |] -> int a, int b, int c
        | _ -> failwith "nope"

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map parse
    |> Seq.toList

type Coord = int * int * int

let hypot = Seq.map float >> Seq.map (fun x -> x * x) >> Seq.sum >> sqrt

let separation ((ax, ay, az): Coord) ((bx, by, bz): Coord) =
    let dx = bx - ax
    let dy = by - ay
    let dz = bz - az
    hypot [ dx; dy; dz ]

let allPairs values =
    let len = List.length values

    seq {
        for i in [ 0 .. len - 1 ] do
            for j in [ i + 1 .. len - 1 ] do
                yield values[i], values[j]
    }

let withItem folder =
    fun acc item -> item, folder (snd acc) item

let toCircuits circuits (a, b) =
    let existingCircuits =
        List.filter (fun c -> Set.contains a c || Set.contains b c) circuits

    match existingCircuits with
    | [] ->
        let circuit' = set [ a; b ]
        circuit' :: circuits
    | [ c ] ->
        let circuit' = c |> Set.add a |> Set.add b
        circuit' :: (List.filter ((<>) c) circuits)
    | [ c1; c2 ] ->
        let circuit' = Set.union c1 c2 |> Set.add a |> Set.add b
        circuit' :: (circuits |> List.filter ((<>) c1) |> List.filter ((<>) c2))
    | _ -> failwith "err"

let part1 (junctions: Coord list) =
    let sortedPairs = allPairs junctions |> Seq.sortBy (fun (a, b) -> separation a b)
    let circuits = sortedPairs |> Seq.take 10 |> Seq.fold toCircuits []
    let circuitSizes = circuits |> List.map Set.count |> List.sortDescending

    circuitSizes |> List.take 3 |> List.fold (*) 1

let part2 (junctions: Coord list) =
    let sortedPairs = allPairs junctions |> Seq.sortBy (fun (a, b) -> separation a b)

    let initState = ((0, 0, 0), (0, 0, 0)), []
    let circuits = sortedPairs |> Seq.scan (withItem toCircuits) initState

    let total = List.length junctions
    let coversAll cir = List.head cir |> Set.count = total
    let isFullyConnected cir = List.length cir = 1 && coversAll cir

    let pair =
        circuits
        |> Seq.skipWhile (fun (_, cir) -> not (isFullyConnected cir))
        |> Seq.head
        |> fst

    let ((ax, _, _), (bx, _, _)) = pair
    int64 ax * int64 bx

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
