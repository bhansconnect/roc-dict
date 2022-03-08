interface U64FlatDict
    exposes [ U64FlatDict, empty, insert, contains, get ]
    imports [ ]

# TODO: move to dict folder once supported.

# A Flat Dict is just a list of key value pairs.
U64FlatDictElem a : [ T U64 a ]

U64FlatDict a := List (U64FlatDictElem a)

empty : {} -> U64FlatDict a
empty = \{} -> $U64FlatDict []

contains : U64FlatDict a, U64 -> Bool
contains = \$U64FlatDict list, key ->
    List.walkUntil list False (\state, T elemKey _ ->
        if elemKey == key then
            Stop True
        else
            Continue state
    )

Option a : [ Some a, None ]

get : U64FlatDict a, U64 -> Option a
get = \$U64FlatDict list, key ->
    List.walkUntil list None (\state, T elemKey val ->
            if elemKey == key then
                Stop (Some val)
            else
                Continue state
    )

insert : U64FlatDict a, U64, a -> U64FlatDict a
insert = \$U64FlatDict list, key, value ->
    index = List.walkUntil list 0 (\state, T elemKey _ ->
            if elemKey == key then
                Stop state
            else
                Continue (state + 1)
        )
    if index == List.len list then
        # Insert new element
        $U64FlatDict (List.append list (T key value))
    else
        # Update existing element
        $U64FlatDict (List.set list index (T key value))
