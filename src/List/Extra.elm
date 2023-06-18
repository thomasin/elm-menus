module List.Extra exposing (last, splitWhen)

import List


{-| Extract the last element of a list.

    last [ 1, 2, 3 ]
    --> Just 3

    last []
    --> Nothing

-}
last : List a -> Maybe a
last items =
    case items of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: rest ->
            last rest


{-| Take a number and a list, return a tuple of lists, where first part is prefix of the list of length equal the number, and second part is the remainder of the list. `splitAt n xs` is equivalent to `(take n xs, drop n xs)`.

    splitAt 3 [ 1, 2, 3, 4, 5 ]
    --> ( [ 1, 2, 3 ], [ 4, 5 ] )

    splitAt 1 [ 1, 2, 3 ]
    --> ( [ 1 ], [ 2, 3 ] )

    splitAt 3 [ 1, 2, 3 ]
    --> ( [ 1, 2, 3 ], [] )

    splitAt 4 [ 1, 2, 3 ]
    --> ( [ 1, 2, 3 ], [] )

    splitAt 0 [ 1, 2, 3 ]
    --> ( [], [ 1, 2, 3 ] )

    splitAt -1 [ 1, 2, 3 ]
    --> ( [], [ 1, 2, 3 ] )

-}
splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


{-| Attempts to split the list at the first element where the given predicate is true. If the predicate is not true for any elements in the list, return nothing. Otherwise, return the split list.

    splitWhen (\n -> n == 3) [ 1, 2, 3, 4, 5 ]
    --> Just ( [ 1, 2 ], [ 3, 4, 5 ] )

    splitWhen (\n -> n == 6) [ 1, 2, 3, 4, 5 ]
    --> Nothing

-}
splitWhen : (a -> Bool) -> List a -> Maybe ( List a, List a )
splitWhen predicate list =
    findIndex predicate list
        |> Maybe.map (\i -> splitAt i list)


{-| Take a predicate and a list, return the index of the first element that satisfies the predicate. Otherwise, return `Nothing`. Indexing starts from 0.

    isEven : Int -> Bool
    isEven i =
        modBy 2 i == 0

    findIndex isEven [ 1, 2, 3 ]
    --> Just 1

    findIndex isEven [ 1, 3, 5 ]
    --> Nothing

    findIndex isEven [ 1, 2, 4 ]
    --> Just 1

-}
findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs
