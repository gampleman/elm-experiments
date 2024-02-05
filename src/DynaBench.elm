module DynaBench exposing (..)


type alias Operation =
    { name : String

    -- , signature : Type
    , usecase : Usecase
    , datastructure : String
    , complexity : Complexity
    , source : String
    , benchmark : Int -> () -> ()
    }


type Usecase
    = Prepend
    | Append
    | Concat
    | Uniq
    | Sort


type Complexity
    = Constant
    | Logarithmic
    | Linear
    | Quasilinear
    | Quadratic
    | Cubic
    | Exponential
    | Factorial


datastructure : String -> List { name : String, usecase : Usecase, complexity : Complexity, source : String, setup : Int -> a, benchmark : a -> Int -> b } -> List Operation
datastructure dsName ops =
    List.map
        (\op ->
            { name = op.name
            , usecase = op.usecase
            , datastructure = dsName
            , complexity = op.complexity
            , source = op.source
            , benchmark =
                \n ->
                    let
                        setup =
                            op.setup n
                    in
                    \() ->
                        always () (op.benchmark setup n)
            }
        )
        ops


datastructures =
    [ datastructure "List"
        [ { name = "::"
          , usecase = Prepend
          , complexity = Constant
          , source = "https://github.com/elm/core/blob/1.0.5/src/Elm/Kernel/List.js#L12-L13"
          , setup = \n -> List.range 0 n
          , benchmark = \l _ -> 42 :: l
          }
        ]
    ]


buildBenchmark : Usecase -> Int -> List Operation -> Benchmark
buildBenchmark usecase size ops =
    List.map
        (\op ->
            ( op.datastructure ++ ": " ++ op.name, op.benchmark size )
        )
        ops
        |> Benchmark.scale (usecaseToString usecase)
