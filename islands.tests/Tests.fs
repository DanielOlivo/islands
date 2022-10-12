module Tests

open System
open Xunit 
open Islands

let parseLine row (line : string) = 
    line 
    |> Seq.mapi(fun col c -> 
        match c with 
        | '-' -> (row,col),false
        | _ -> (row,col),true)

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

let parseLines (lines : string list) = 
    let (height, width) = lines.Length, lines.[0].Length
    let coords = seq{
        for y in {0..height-1} do
            for x in {0..width-1} -> y,x
    }
    coords 
    |> Seq.map(fun (y,x) -> lines.[y].[x], (y,x))
    |> Seq.filter (fun (ch,_) -> ch <> '-')
    |> Seq.groupBy (fst)
    |> Seq.mapi (fun i (_,coord) -> i, coord |> Seq.map snd |> Seq.toList)
    |> Map.ofSeq
    |> fun data -> data,(height,width)

let lines1 = [
    "aaa"
    "a--"
    "a-b"
]

let lines2 = [
    "aaaaaaaaaaaaaaaaaaaaaaa"
    "aaaaaa----------------a"
    "aaaaaa-aaaaaaaaaaaaaaaa"
    "aaaaaa-aaaaaaaaaa------"
    "aaaaaa-aaaaaaaaaa-bbbbb"
    "aaaaaa-aaaaaaaaaa-bbbbb"
]

[<Fact>]
let ``twoIslands3x3`` () = 
    let lines = [
        "xxx"
        "x--"
        "x-x"
    ]
    let freeSpace = init lines
    let (checker, count) = Islands.getChecker freeSpace

    let checkCount = count = 2
    let checkCoords = [(0,0);(0,1);(0,2);(1,0); (2,0)] |> List.tryFind(checker (2,2)) |> Option.isNone
    Assert.True(checkCount && checkCoords)

[<Fact>]
let ``smallCheck``() = 
    let (coords, size) = parseLines lines1
    let (_,count) = lines1 |> init |> Islands.getChecker
    Assert.Equal(coords |> Map.keys |> Seq.length, count)

[<Fact>]
let ``bigMapCountCheck`` () = 
    let (coords, size) = parseLines lines2
    let (_, count) =  lines2 |> init |> Islands.getChecker
    Assert.Equal(coords |> Map.keys |> Seq.length, count)
