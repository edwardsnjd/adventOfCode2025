#! /usr/bin/env -S dotnet fsi

open System.Text.RegularExpressions

type Slots = int list
type Buttons = int list list
type Machine = Slots * Buttons * Slots

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
        |> Array.toList

    let c = m.Groups.[3].Value.Split ',' |> Array.map int |> Array.toList

    a, b, c

let lines =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map parse
    |> Seq.toList

let rec combinationsOfN buttons n : Buttons seq =
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

let rec combinationsOfNPresses buttons n : Buttons seq =
    seq {
        if n = 0 then
            yield []
        else
            for i in 0 .. List.length buttons - 1 do
                let button = buttons.[i]

                for restCombos in combinationsOfNPresses buttons (n - 1) do
                    yield button :: restCombos
    }

let allCombos (buttons: Buttons) : Buttons seq =
    seq {
        for n in 0 .. buttons.Length do
            yield! combinationsOfN buttons n
    }

let step lights button =
    List.zip lights button |> List.map (fun (a, b) -> a + b)

let after buttons initial : Slots = Seq.fold step initial buttons

let emptySlots count = List.replicate count 0

let result count buttons : Slots = after buttons (emptySlots count)

let modulus v n = n % v

let solve1 ((target, buttons, _): Machine) =
    let count = List.length target

    allCombos buttons
    |> Seq.filter (result count >> List.map (modulus 2) >> (=) target)
    |> Seq.head
    |> List.length

let tap fn x =
    fn x
    x

let isValidFor (target: Slots) (result: Slots) =
    List.zip target result |> List.forall (fun (a, b) -> a >= b)

let solve2 ((_, buttons, joltage): Machine) : int =
    printfn "joltage:\n%A" joltage
    printfn "buttons:\n%A" buttons

    let count = List.length joltage

    let rec possiblePresses (target: Slots) idx (current: Slots) : Buttons seq =
        printfn "possiblePresses %A %A %A" target idx current
        let length = target.Length

        seq {
            if idx >= length then
                yield []
            else
                let numberOfPressesNeeded = target.[idx] - current.[idx]
                let solvedIndices = List.filter (fun i -> target.[i] = 0) [ 0 .. length - 1 ]
                let affectsIndex i button = List.item i button |> (=) 1

                let relevantButtons =
                    buttons
                    |> List.filter (affectsIndex idx)
                    |> List.filter (fun b -> solvedIndices |> List.forall (fun i -> not (affectsIndex i b)))
                    |> List.sortByDescending List.length

                printfn "candidates: %d of %d" numberOfPressesNeeded relevantButtons.Length

                let candidatePresses =
                    combinationsOfNPresses relevantButtons numberOfPressesNeeded//  |> Seq.toList

                // printfn "candidates: -> %d" candidatePresses.Length

                for bs in candidatePresses do
                    let current' = after bs current

                    if isValidFor target current' then
                        // printfn "valid %A %A" target current'

                        for bs' in possiblePresses target (idx + 1) current' do
                            yield bs @ bs'
                    else
                        printfn "invalid %A %A" target current'
        }

    emptySlots count
    |> possiblePresses joltage 0
    |> Seq.sortBy List.length
    |> Seq.head
    |> List.length
    |> tap (printfn "results:\n%A")

let part1 machines = machines |> List.map solve1 |> List.sum

let part2 machines = machines |> List.map solve2 |> List.sum
// let part2 machines =
//     machines
//     |> List.map (fun ((target, buttons, other): Machine) -> buttons.Length, List.max other)
//     |> List.toArray

lines |> part1 |> printfn "Part1: %A"
lines |> part2 |> printfn "Part2: %A"
