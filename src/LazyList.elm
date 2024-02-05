module LazyList exposing (..)

import Html exposing (a)


type LazyList a
    = Nil
    | Cons a (LazyList a)
    | Thunk (() -> LazyList a)


fromList : List a -> LazyList a
fromList =
    List.foldl Cons Nil


map : (a -> b) -> LazyList a -> LazyList b
map fn lst =
    case lst of
        Nil ->
            Nil

        Cons v rest ->
            Cons (fn v) (map fn rest)

        Thunk thnk ->
            Thunk (thnk >> map fn)


take : Int -> LazyList a -> LazyList a
take n lst =
    case lst of
        Nil ->
            Nil

        Cons v rest ->
            if n >= 0 then
                Cons v (take (n - 1) rest)

            else
                Nil

        Thunk thnk ->
            Thunk (thnk >> take n)


toList : LazyList a -> List a
toList lst =
    case lst of
        Nil ->
            []

        Cons v rest ->
            v :: toList rest

        Thunk thnk ->
            toList (thnk ())
