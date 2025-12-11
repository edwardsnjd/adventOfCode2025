#! /usr/bin/env -S dotnet fsi

open System.Text.RegularExpressions

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.toList

let words (line: string) =
    line.Split(',', System.StringSplitOptions.RemoveEmptyEntries)

let fooRegex = Regex @"^\[(.*)\] (.*) \{(.*)\}$"

let parse line =
    let m = fooRegex.Match line

    let lights = m.Groups.[1].Value
    let count = String.length lights
    let bitValue i = 1 <<< count - i - 1

    let a =
        lights
        |> Seq.mapi (fun i c ->
            match c with
            | '.' -> 0
            | '#' -> bitValue i
            | _ -> failwith "nope")
        |> Seq.fold (+) 0

    let b =
        m.Groups.[2].Value.Split ' '
        |> Array.map (fun s -> s.[1 .. s.Length - 2])
        |> Array.map (fun s -> s.Split ',')
        |> Array.map (Array.map int)
        |> Array.map (Array.map bitValue >> Seq.fold (+) 0)
        |> Set.ofArray

    // let c = m.Groups.[3].Value

    a, b

let rec combinationsOfN buttons n =
    seq {
        if n = 0 then
            yield []
        else
            match buttons with
            | [] -> ()
            | item :: rest ->
                for restCombos in combinationsOfN rest (n - 1) do
                    yield item :: restCombos

                yield! combinationsOfN rest n
    }

let allCombos buttons =
    let buttonsL = Set.toList buttons

    seq {
        for n in 0 .. buttonsL.Length do
            yield! combinationsOfN buttonsL n
    }

let step lights button = lights ^^^ button
let result buttons = Seq.fold step 0 buttons

let solve (target, buttons) =
    allCombos buttons
    |> Seq.filter (result >> (=) target)
    |> Seq.head
    |> List.length

let part1 lines =
    List.map parse lines |> List.map solve |> List.sum

lines |> part1 |> printfn "Part1: %A"
