#! /usr/bin/env -S dotnet fsi

open System.Text.RegularExpressions

type Slots = int list
type Machine = int list * Set<int list>

let fooRegex = Regex @"^\[(.*)\] (.*) \{(.*)\}$"

let parse line : Machine =
    let m = fooRegex.Match line

    let lights = m.Groups.[1].Value
    let count = String.length lights

    let a =
        lights
        |> Seq.map (function
            | '.' -> 0
            | '#' -> 1
            | _ -> failwith "nope")
        |> Seq.toList

    let b =
        m.Groups.[2].Value.Split ' '
        |> Array.map (fun s -> s.[1 .. s.Length - 2])
        |> Array.map (fun s -> s.Split ',' |> Array.map int)
        |> Array.map (fun vs ->
            [ for i in 0 .. count - 1 do
                  yield if Array.contains i vs then 1 else 0 ])
        |> Set.ofArray

    // let c = m.Groups.[3].Value

    a, b

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map parse
    |> Seq.toList

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

let flip fn a b = fn b a

let step lights button =
    List.zip lights button |> List.map (fun (a, b) -> a + b)

let result count buttons =
    let initial = List.replicate count 0
    Seq.fold step initial buttons

let solve ((target, buttons): Machine) =
    let count = List.length target

    allCombos buttons
    |> Seq.filter (result count >> List.map (flip (%) 2) >> (=) target)
    |> Seq.head
    |> List.length

let part1 machines = machines |> List.map solve |> List.sum

lines |> part1 |> printfn "Part1: %A"
