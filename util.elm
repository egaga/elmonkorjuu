module Util where

maybeToList : Maybe a -> List a
maybeToList m =
  case m of
    Nothing -> []
    Just a -> [ a ]

mapMaybeToList : (a -> b) -> Maybe a -> List b
mapMaybeToList f x = maybeToList (Maybe.map f x)

getFromIndex list index =
  List.take 1 (List.drop index list)

splitByIndex : Int -> List a -> (List a, List a, List a)
splitByIndex index list =
  let
    before = List.take index list
    element = getFromIndex list index
    after = List.drop (index+1) list
 in
    (before, element, after)

findFirst : ((Int, a) -> Bool) -> List a -> Maybe (Int, a)
findFirst test list =
  let
    indexed = List.indexedMap (,) list
    found = List.filter test indexed
  in
    List.head found

-- Get elements other than the element that is found by given test function.
-- The result is in a pair of two lists.
-- First list has elements before the element specified by the test.
-- Second list has elements after the element specified by the test.
otherElements : List a -> ((Int, a) -> Bool) -> (List a, List a)
otherElements list test =
    case findFirst test list of
      Nothing -> (list, []) -- element was not found!
      Just (elementIndex, _) ->
        let (before, _, after) = splitByIndex elementIndex list
        in (before, after)

-- Replace element in a list with a new element.
updateElement : List a -> a -> ((Int, a) -> Bool) -> List a
updateElement list newElement test =
  let
    (before, after) = otherElements list test -- the old element gets discarded
  in
    List.concat [ before, [ newElement ], after ]
