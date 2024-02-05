module ExtensibleUnions exposing (E2, E3, E4, With(..), case2, case3, case4, extractFirst, extractSecond, extractThird, first, second, third)


type With a b
    = This a
    | That b


type alias E2 a rest =
    With a rest


type alias E3 a b rest =
    With a (With b rest)


type alias E4 a b c rest =
    With a (E3 b c rest)


case2 : (a -> result) -> (b -> result) -> E2 a b -> result
case2 aBranch bBranch data =
    case data of
        This val ->
            aBranch val

        That val ->
            bBranch val


case3 : (a -> result) -> (b -> result) -> (c -> result) -> E3 a b c -> result
case3 aBranch bBranch cBranch data =
    case2 aBranch (case2 bBranch cBranch) data


case4 : (a -> result) -> (b -> result) -> (c -> result) -> (d -> result) -> E4 a b c d -> result
case4 aBranch bBranch cBranch dBranch data =
    case2 aBranch (case2 bBranch (case2 cBranch dBranch)) data


nothing : a -> Maybe b
nothing =
    always Nothing


extractFirst : E2 a rest -> Maybe a
extractFirst =
    case2 Just nothing


extractSecond : E3 a b rest -> Maybe b
extractSecond =
    case2 nothing extractFirst


extractThird : E4 a b c rest -> Maybe c
extractThird =
    case2 nothing extractSecond


first : a -> E2 a rest
first =
    This


second : b -> E3 a b rest
second val =
    That (This val)


third : c -> E4 a b c rest
third val =
    That (That (This val))
