interface U64FlatDict
    exposes [ U64FlatDict, empty, insert, contains, get, remove ]
    imports []

# TODO: move to dict folder once supported.
# A Flat Dict is just a list of key value pairs.
Option a : [ Some a, None ]

Elem a : [ T U64 a ]

U64FlatDict a := List (Elem a)

empty : {} -> U64FlatDict a
empty = \{  } -> $U64FlatDict []

contains : U64FlatDict a, U64 -> Bool
contains = \$U64FlatDict list, key ->
    List.walkUntil
        list
        False
        (\state, T elemKey _ ->
                if elemKey == key then
                    Stop True
                else
                    Continue state
        )

get : U64FlatDict a, U64 -> Option a
get = \$U64FlatDict list, key ->
    List.walkUntil
        list
        None
        (\state, T elemKey val ->
                if elemKey == key then
                    Stop (Some val)
                else
                    Continue state
        )

insert : U64FlatDict a, U64, a -> U64FlatDict a
insert = \$U64FlatDict list, key, value ->
    index = List.walkUntil
        list
        0
        (\state, T elemKey _ ->
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

remove : U64FlatDict a, U64 -> U64FlatDict a
remove = \$U64FlatDict list, key ->
    index = List.walkUntil
        list
        0
        (\state, T elemKey _ ->
                if elemKey == key then
                    Stop state
                else
                    Continue (state + 1)
        )

    if index == List.len list then
        # Item not found, nothing to do
        $U64FlatDict list
    else
        # Update existing element
        $U64FlatDict (List.dropAt list index)