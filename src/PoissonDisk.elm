module PoissonDisk exposing (sample)

import Array exposing (Array)
import Array.Extra
import Dict exposing (Dict)
import Random


type Grid
    = Grid
        { width : Float
        , height : Float
        , radius : Float
        , cellSize : Float
        , gridWidth : Int
        , gridHeight : Int
        , grid : Dict Int ( Float, Float )
        }


initialize : Float -> Float -> Float -> Grid
initialize width height radius =
    let
        cellSize =
            radius * sqrt 0.5

        gridWidth =
            ceiling (width / cellSize)

        gridHeight =
            ceiling (height / cellSize)
    in
    Grid
        { width = width
        , height = height
        , radius = radius
        , cellSize = cellSize
        , gridWidth = ceiling (width / cellSize)
        , gridHeight = ceiling (height / cellSize)
        , grid = Dict.empty
        }


set : ( Float, Float ) -> Grid -> Grid
set ( x, y ) (Grid grid) =
    Grid { grid | grid = Dict.insert (grid.gridWidth * floor (y / grid.cellSize) + floor (x / grid.cellSize)) ( x, y ) grid.grid }


getRadius : Grid -> Float
getRadius (Grid grid) =
    grid.radius


isNeighborhoodEmpty : ( Float, Float ) -> Grid -> Bool
isNeighborhoodEmpty ( cx, cy ) (Grid { grid, cellSize, gridWidth, gridHeight, radius, width, height }) =
    let
        i_ =
            floor (cx / cellSize)

        j_ =
            floor (cy / cellSize)

        is =
            List.range (max 0 (i_ - 2)) (min (i_ + 3) gridWidth - 1)

        os =
            List.range (max 0 (j_ - 2)) (min (j_ + 3) gridHeight - 1) |> List.map (\j -> j * gridWidth)

        radius2 =
            radius ^ 2
    in
    0
        <= cx
        && cx
        < width
        && 0
        <= cy
        && cy
        < height
        && List.all
            (\o ->
                List.all
                    (\i ->
                        case Dict.get (o + i) grid of
                            Just ( sx, sy ) ->
                                (sx - cx) ^ 2 + (sy - cy) ^ 2 >= radius2

                            Nothing ->
                                True
                    )
                    is
            )
            os


values : Grid -> List ( Float, Float )
values (Grid { grid }) =
    Dict.values grid


k =
    30


sample : Int -> Random.Generator (List ( Float, Float ))
sample =
    sampleCustom { x = ( 0, 1 ), y = ( 0, 1 ) }


sampleCustom : { x : ( Float, Float ), y : ( Float, Float ) } -> Int -> Random.Generator (List ( Float, Float ))
sampleCustom { x, y } number =
    let
        width =
            Tuple.second x - Tuple.first x

        height =
            Tuple.second y - Tuple.first y

        radius2 =
            width * height / (toFloat number * 1.5)

        radius =
            sqrt radius2

        cellSize =
            radius * sqrt 0.5

        gridWidth =
            ceiling (width / cellSize)

        gridHeight =
            ceiling (height / cellSize)

        queue0 =
            Array.empty
    in
    Random.map3
        (\seed fx fy ->
            go2 seed NewPoint (Array.push ( fx, fy ) Array.empty) (set ( fx, fy ) (initialize width height radius))
                |> List.map
                    (\( px, py ) ->
                        ( Tuple.first x + px, Tuple.first y + py )
                    )
        )
        Random.independentSeed
        (Random.float (width / 2 - radius) (width / 2 + radius))
        (Random.float (height / 2 - radius) (height / 2 + radius))


type Mode
    = NewPoint
    | Neighborhood Int Int ( Float, Float )


go2 seed mode queue grid =
    case mode of
        NewPoint ->
            let
                n =
                    Array.length queue
            in
            if n == 0 then
                values grid

            else
                let
                    ( parentIndex, newSeed ) =
                        Random.step (Random.int 0 n) seed
                in
                go2 newSeed (Neighborhood k parentIndex (Array.get parentIndex queue |> Maybe.withDefault ( 0 / 0, 0 / 0 ))) queue grid

        Neighborhood attempts parentIndex ( px, py ) ->
            let
                radius =
                    getRadius grid

                ( point, newSeed ) =
                    Random.step
                        (Random.map2
                            (\a d ->
                                ( px + d * cos a, py + d * sin a )
                            )
                            (Random.float 0 (2 * pi))
                            (Random.float radius (2 * radius))
                        )
                        seed
            in
            if isNeighborhoodEmpty point grid then
                go2 newSeed NewPoint (Array.push point queue) (set point grid)

            else if attempts > 0 then
                go2 newSeed (Neighborhood (attempts - 1) parentIndex ( px, py )) queue grid

            else
                go2 newSeed NewPoint (Array.Extra.removeAt parentIndex queue) grid
