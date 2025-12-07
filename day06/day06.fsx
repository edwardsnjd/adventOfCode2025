#! /usr/bin/env -S dotnet fsi

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toArray

let words (line: string) =
    line.Trim().Split(' ', System.StringSplitOptions.RemoveEmptyEntries)

let rotate lines =
    let h = Array.length lines
    let w = String.length (Array.head lines)

    [ for x in [ 0 .. w - 1 ] do
          [ for y in [ 0 .. h - 1 ] do
                yield lines.[y].[x] ] ]

let parse1 lines =
    let grid = Array.map words lines
    let h = Array.length grid
    let w = Array.length (Array.head grid)

    // Calculation per column of words
    [ for x in [ 0 .. w - 1 ] do
          let operation = grid.[h - 1].[x].[0]
          let operands = [ for y in [ h - 2 .. -1 .. 0 ] -> int64 grid.[y].[x] ]
          operation, operands ]

let parse2 lines =
    let toOperand chars =
        chars
        |> Seq.take (List.length chars - 1)
        |> Seq.toArray
        |> System.String
        |> _.Trim()
        |> int64

    // Calculation per group separated by character of spaces
    let update (calcs, operation: char option, operands) line =
        match line with
        | chars when List.forall ((=) ' ') chars ->
            let calcs' = (operation.Value, operands) :: calcs
            calcs', None, []
        | chars when List.last chars <> ' ' ->
            let operation' = Some(List.last chars)
            let operands' = toOperand chars :: []
            calcs, operation', operands'
        | chars ->
            let operands' = toOperand chars :: operands
            calcs, operation, operands'

    let initialState = [], None, []
    let lines' = rotate lines @ [ [ ' ' ] ] // Blank line acts as end of group marker
    let calcs, _, _ = List.fold update initialState lines'
    calcs

let calculate (operation, operands) =
    match operation with
    | '*' -> List.fold (*) 1L operands
    | '+' -> List.fold (+) 0L operands
    | _ -> failwith $"Unexpected calculation {operation} {operands}"

let part1 = parse1 >> List.map calculate >> List.sum
let part2 = parse2 >> List.map calculate >> List.sum

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
