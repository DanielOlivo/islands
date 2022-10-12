namespace Islands

module Islands = 
    let private getCoords (height, width) = 
        {0..height - 1}
        |> Seq.map(fun row -> {0..width-1} |> Seq.map(fun col -> row,col))
        |> Seq.collect id

    let private isWithin (height,width) (y,x) = 
        not (y < 0 || y >= height || x < 0 || x >= width) 

    let inline private get (map : 'a[,]) (y,x) = map.[y,x]
    let inline private set (map : 'a[,]) value (y,x) = map.[y,x] <- value
    let inline private replace (map : 'a[,]) coords  b  a= 
        coords |> Seq.iter(fun (y,x) -> if map.[y,x] = a then map.[y,x] <- b)
    let private fromStartPoint (freeMap : bool[,]) coords = 
        coords |> Seq.skipWhile(fun (y,x) -> not freeMap.[y,x])

    let private deltaNeighbours = seq{
        0,1
        1,0
        0,-1
        0,-1
    }

    let private getNeighbourCoords (y,x) = deltaNeighbours |> Seq.map(fun (dy, dx) -> y + dy, x + dx)

    let private (|Forbidden|EmptyAlone|Empty|Visited|VisitedAlone|) (freeSpace, islands, sizes, coord) = 
        match get freeSpace coord with 
        | false -> Forbidden coord
        | true -> 
            let id = get islands coord
            let neighbours = 
                coord
                |> getNeighbourCoords
                |> Seq.filter (isWithin sizes)
                |> Seq.filter (get freeSpace)
                |> Seq.map (fun coord -> coord, get islands coord)
                |> Seq.toList
            let visited = neighbours |> List.filter(fun (_,_id) -> _id > 0)
            let free = neighbours |> List.filter(fun (_,_id) -> _id = 0) |> List.map fst
            match id, visited, free with 
            | 0, [], unvisited -> EmptyAlone (coord, unvisited)
            | x, [] , unvisited -> VisitedAlone((coord,x), unvisited)
            | 0, visited, unvisited -> 
                Empty (coord, unvisited, visited)
            | x, visited, unvisited -> 
                Visited ((coord, get islands coord), visited, unvisited)

    let getChecker (freeSpace : bool[,]) = 
        let (height,width) = Array2D.length1 freeSpace, Array2D.length2 freeSpace//lines.Length, lines.[0].Length
        let sizes = (height, width)
        //let freeSpace = init lines
        let freeGetter = get freeSpace
        let islands = Array2D.init height width (fun _ _ -> 0)

        let coordsWithStartPoint = 
            (height,width)
            |> getCoords
            |> fromStartPoint freeSpace

        coordsWithStartPoint
        |> Seq.head 
        |> set islands 1

        let coords = coordsWithStartPoint |> Seq.tail

        let progress = 
            coords 
            |> Seq.scan(fun acc coord -> 
                match (freeSpace,islands,sizes,coord) with 
                | Forbidden forbiddenCell -> 
                    //printfn "Forbidden at %A" coord 
                    acc + 1
                | EmptyAlone (_,unvisited) ->
                    //printfn "EMPTY ALONE"
                    (coord :: unvisited) |> List.iter(set islands acc)
                    acc
                | Empty (_, unvisited, visited) -> 
                    //printfn "EMPTY"
                    let ids = visited |> List.map snd |> List.distinct |> List.sort
                    let mainId = ids |> List.head
                    let othersIds = ids |> List.tail
                    //printfn "main %A;\t others %A" mainId othersIds
                    set islands mainId coord
                    othersIds |> List.iter (replace islands coords mainId)
                    unvisited |> List.iter (set islands mainId)
                    acc
                | Visited (_,visited,unvisited) -> 
                    let ids = 
                        visited 
                        |> List.map snd 
                        |> fun vs -> (get islands coord) :: vs
                        |> List.distinct
                        |> List.sort
                    let mainId = ids |> List.head
                    let others = ids |> List.tail
                    others |> List.iter (replace islands coords mainId)
                    unvisited |> List.iter (set islands mainId)
                    acc
                | VisitedAlone (_,unvisited) -> 
                    (coord :: unvisited) |> List.iter(set islands 1)
                    acc    
            ) 1
            |> Seq.toList

        let checker (y1,x1) (y2,x2) = 
            let (a,b) = islands.[y1,x1], islands.[y2,x2]
            if a = 0 || b = 0 then false 
            else islands.[y1,x1] = islands.[y2,x2]
        let islandsCount = 
            coords |> Seq.map(get islands) |> Seq.filter((<>) 0) |> Seq.distinct |> Seq.length

        checker,islandsCount