module Util exposing (..)

import Array exposing (Array)


maybeToList : Maybe a -> List a
maybeToList m =
    case m of
        Nothing ->
            []

        Just a ->
            [ a ]


mapMaybeToList : (a -> b) -> Maybe a -> List b
mapMaybeToList f x =
    maybeToList (Maybe.map f x)


getFromIndex list index =
    List.take 1 (List.drop index list)


asplitByIndex : Int -> Array a -> ( Array a, Array a, Array a )
asplitByIndex index array =
    let
        before =
            Array.slice 0 index array

        element =
            Array.slice index (1 + index) array

        after =
            arrayDrop (index + 1) array
    in
    ( before, element, after )


splitByIndex : Int -> List a -> ( List a, List a, List a )
splitByIndex index list =
    let
        before =
            List.take index list

        element =
            getFromIndex list index

        after =
            List.drop (index + 1) list
    in
    ( before, element, after )


findFirst : (( Int, a ) -> Bool) -> List a -> Maybe ( Int, a )
findFirst test list =
    let
        indexed =
            List.indexedMap (\a b -> ( a, b )) list

        found =
            List.filter test indexed
    in
    List.head found



-- Get elements other than the element that is found by given test function.
-- The result is in a pair of two lists.
-- First list has elements before the element specified by the test.
-- Second list has elements after the element specified by the test.


otherElements : List a -> (( Int, a ) -> Bool) -> ( List a, List a )
otherElements list test =
    case findFirst test list of
        Nothing ->
            ( list, [] )

        -- element was not found!
        Just ( elementIndex, _ ) ->
            let
                ( before, _, after ) =
                    splitByIndex elementIndex list
            in
            ( before, after )


updateElement : Array a -> a -> (( Int, a ) -> Bool) -> Array a
updateElement array newElement test =
    let
        list =
            Array.toList array

        result =
            listUpdateElement list newElement test
    in
    Array.fromList result



-- Replace element in a list with a new element.


listUpdateElement : List a -> a -> (( Int, a ) -> Bool) -> List a
listUpdateElement list newElement test =
    let
        ( before, after ) =
            otherElements list test

        -- the old element gets discarded
    in
    List.concat [ before, [ newElement ], after ]


arrayReverse : Array a -> Array a
arrayReverse array =
    let
        list =
            Array.toList array
    in
    Array.fromList (List.reverse list)


arrayDrop : Int -> Array a -> Array a
arrayDrop amount array =
    Array.slice amount (Array.length array) array
