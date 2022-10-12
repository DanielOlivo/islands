# README

Simplest library to find separated areas on 2d bool array.

```fsharp
open Islands

// data preparations
let lines = [
    "---"
    "-xx"
    "-x-"
]

let parseLine row (line : string) = 
    line 
    |> Seq.mapi(fun col c -> 
        match c with 
        | 'x' -> (row,col),false
        | _ -> (row,col),true)

/// transform list of string into bool[,]
let init (lines : string list) = 
    let (height, width) = lines.Length, lines.[0].Length
    let coords = 
        lines 
        |> List.mapi (parseLine)
        |> List.map (Seq.toList)
        |> List.collect id
        |> Map.ofList
    let mapped = Array2D.init height width (fun row col -> coords.[row,col])
    mapped

// get check function to get info if two coordinates are on the same are and number of separated areas
let (checker,amountOfAreas) = lines |> init |> Islands.getChecker

// amountOfAreas = 2

let coord1 = (0,0) // row/col, top left
let coord2 = (0,2) // top right 
let coord3 = (2,2) // bottom right

(checker coord1 coord2) // true
(checker coord1 coord3) // false

```