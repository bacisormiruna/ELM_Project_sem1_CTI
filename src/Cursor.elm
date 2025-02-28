module Cursor exposing (Cursor, back, current, forward, fromList, length, nonEmpty, toList, withSelectedElement)

{-| Data structure to efficiently navigate a list forward or backward.

It stores a non-empty list as two lists and one element that is currently "selected".

For example, the list `[1, 2, 3]`, when focused on the first element, would be stored as `Cursor [] 1 [2, 3]`.
To focus on the second element, the representation becomes `Cursor [1] 2 [3]`.
Finally, focusing on the third element is: `Cursor [2, 1] 3 []`.

**Note that the left part of the list is stored in reverse order!**

-}


type Cursor a
    = Cursor (List a) a (List a)


withSelectedElement : List a -> a -> List a -> Cursor a
withSelectedElement left mid right =
    Cursor (List.reverse left) mid right


nonEmpty : a -> List a -> Cursor a
nonEmpty x xs =
    Cursor [] x xs


{-| Creates a `Cursor` from a `List`, if the list is not empty

    fromList [ 1, 2, 3 ] --> Just (withSelectedElement [] 1 [2, 3])

    fromList [] --> Nothing

-}
fromList : List a -> Maybe (Cursor a) --Transformă o listă obișnuită (List) într-un Cursor
fromList l =
    case l of
        [] -> Nothing
        xs :: x -> Just (Cursor [] xs x)


{-| Convert the `Cursor` to a `List`

    toList (nonEmpty 1 [ 2, 3 ]) --> [1, 2, 3]

-}
toList : Cursor a -> List a --Transformă un Cursor înapoi într-o listă obișnuită (List).
toList (Cursor left select right) =
    List.reverse left ++ (select :: right)


{-| Get the current element from the cursor

    current (nonEmpty 1 [ 2, 3 ]) {- ignore -} --> 1

    current (withSelectedElement [ 1, 2 ] 3 [ 4, 5 ]) {- ignore -} --> 3

-}
current : Cursor a -> a
current (Cursor _ a _) =
    a

{-| Move the cursor forward.

If the cursor would go past the last element, the function should return `Nothing`.

    forward (nonEmpty 1 [ 2, 3 ]) --> Just (withSelectedElement [1] 2 [3])

    forward (nonEmpty 1 []) --> Nothing

    nonEmpty 1 [ 2, 3 ] |> forward |> Maybe.andThen forward --> Just (withSelectedElement [1, 2] 3 [])

    nonEmpty 1 [ 2, 3 ] |> forward |> Maybe.andThen forward |> Maybe.andThen forward {- hidden -} --> Nothing

-}

forward : Cursor a -> Maybe (Cursor a)
forward (Cursor left select right) =
    case right of
        [] -> Nothing
        xs :: x -> Just (Cursor (select :: left) xs x)


{-| Move the cursor backward.

If the cursor would go before the first element, the function should return `Nothing`.

    back (nonEmpty 1 [ 2, 3 ]) --> Nothing

    back (nonEmpty 1 []) --> Nothing

    nonEmpty 1 [ 2, 3 ] |> forward |> Maybe.andThen back --> Just (withSelectedElement [] 1 [2, 3])

-}

back : Cursor a -> Maybe (Cursor a) --se muta cursorul inapoi
back (Cursor left select right) =
    case left of
        [] -> Nothing
        xs :: x -> Just (Cursor x xs (select :: right))

{-| Get the number of elements

    length (nonEmpty 1 []) --> 1

    length (nonEmpty 1 [ 2, 3 ]) --> 3

-}

length : Cursor a -> Int --numarul elementelor din lista
length (Cursor left _ right) =
    List.length left + List.length right + 1
