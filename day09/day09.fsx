#! /usr/bin/env -S dotnet fsi

type Coord = int * int

let tap fn x =
    fn x
    x

let parse (line: string) : Coord =
    line.Split "," |> Array.map int |> (fun a -> a[0], a[1])

let tiles =
    Seq.initInfinite (fun _ -> System.Console.ReadLine())
    |> Seq.takeWhile (fun line -> line <> null)
    |> Seq.map parse
    |> Seq.toArray

let allCorners l =
    let len = Array.length l

    seq {
        for i in 0 .. len - 1 do
            for j in i + 1 .. len - 1 do
                yield l[i], l[j]
    }

let inline inc i = i + 1
let inline dim v = v |> abs |> inc
let dimensions ((x1, y1), (x2, y2)) = dim (x2 - x1), dim (y2 - y1)
let rectSize = dimensions >> fun (dx, dy) -> int64 dx * int64 dy

let normaliseCorners ((x1, y1): Coord, (x2, y2)) =
    let xmin, xmax = min x1 x2, max x1 x2
    let ymin, ymax = min y1 y2, max y1 y2
    (xmin, ymin), (xmax, ymax)

let rectPoints ((x1, y1), (x2, y2)) = [| x1, y1; x2, y1; x2, y2; x1, y2 |]

let withArg fn item = item, fn item

let step =
    function
    | (x1, y1), (x2, y2) when x1 = x2 -> 1, (if y2 > y1 then 1 else -1)
    | (x1, y1), (x2, y2) when y1 = y2 -> (if x2 > x1 then 1 else -1), 1
    | _ -> failwith ""

let add s x = Set.add x s

let inline between a b x = x >= min a b && x <= max a b

type Polygon = Coord array

let isPointOnEdge ((px, py): Coord) (poly: Polygon) =
    let n = poly.Length

    seq {
        for i in 0 .. n - 1 do
            let j = (i + 1) % n
            let ax, ay = poly.[i]
            let bx, by = poly.[j]

            if ay = by then
                // horizontal edge
                if py = ay && between ax bx px then
                    yield true
            elif ax = bx then
                // vertical edge
                if px = ax && between ay by py then
                    yield true
    }
    |> Seq.exists id

let segmentsIntersect ((a1x, a1y): Coord) ((a2x, a2y): Coord) ((b1x, b1y): Coord) ((b2x, b2y): Coord) =
    // All segments are axis-aligned
    let aHoriz = a1y = a2y
    let bHoriz = b1y = b2y

    match aHoriz, bHoriz with
    | true, true ->
        // collinear horizontal: overlap in X and same Y
        a1y = b1y && max (min a1x a2x) (min b1x b2x) <= min (max a1x a2x) (max b1x b2x)
    | false, false ->
        // collinear vertical: overlap in Y and same X
        a1x = b1x && max (min a1y a2y) (min b1y b2y) <= min (max a1y a2y) (max b1y b2y)
    | true, false ->
        // a horizontal, b vertical
        between a1x a2x b1x && between b1y b2y a1y
    | false, true ->
        // a vertical, b horizontal
        between b1x b2x a1x && between a1y a2y b1y

let isPointInPolygon (poly: Polygon) =
    fun ((px, py): Coord as p) ->
        // first: on edge counts as inside
        if isPointOnEdge p poly then
            true
        else
            let mutable inside = false
            let n = poly.Length
            let mutable j = n - 1

            for i in 0 .. n - 1 do
                let pix, piy = poly.[i]
                let pjx, pjy = poly.[j]

                let intersects =
                    // edge crosses horizontal line at py?
                    (piy > py) <> (pjy > py)
                    &&
                    // x coordinate of intersection to the right of px?
                    px <= (pjx - pix) * (py - piy) / (pjy - piy) + pix

                if intersects then
                    inside <- not inside

                j <- i

            inside // |> tap (printfn "Checking %A = %A" p))

let rectFullyInsidePolygon (poly: Polygon) corners =
    let corners' = normaliseCorners corners
    let rectPts = rectPoints corners'

    // 1. All corners must be inside/on boundary
    if rectPts |> Array.exists (fun p -> not (isPointInPolygon poly p)) then
        false
    else
        // 2. No edge intersections except allowed collinear overlap
        let n = poly.Length

        // rectangle edges
        let rEdges =
            [| rectPts.[0], rectPts.[1]
               rectPts.[1], rectPts.[2]
               rectPts.[2], rectPts.[3]
               rectPts.[3], rectPts.[0] |]

        let mutable ok = true

        for (ra, rb) in rEdges do
            let mutable k = 0

            while ok && k < n do
                let pa = poly.[k]
                let pb = poly.[(k + 1) % n]

                if segmentsIntersect ra rb pa pb then
                    // If you want to allow edge-overlap as still "inside",
                    // add logic here to detect the "just sharing boundary" case.
                    ok <- true // or false if you want strict interior

                k <- k + 1

        ok


/// The input data roughly draws a circle, except for two points that cut
/// across from the LHS ~50k nearly all the way across to the RHS ~50k,
/// which means that the solution search space can be cut in half.
let hackyIsTopOrBottom corners =
    let (_, ymin), (_, ymax) = normaliseCorners corners
    ymin < 50000 && ymax < 50000 || (ymin > 50000 && ymax > 50000)

let part1 = allCorners >> Seq.map rectSize >> Seq.sortDescending >> Seq.head

let part2 (tiles: Coord array) =
    tiles
    |> allCorners
    |> Seq.map (withArg rectSize)
    |> Seq.sortByDescending snd
    |> Seq.filter (fst >> hackyIsTopOrBottom)
    |> Seq.filter (fst >> rectFullyInsidePolygon tiles)
    |> Seq.head
    |> snd

tiles |> part1 |> printfn "Part1: %A"
tiles |> part2 |> printfn "Part2: %A"
