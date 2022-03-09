interface U64FlatHashDict
    exposes [ U64FlatHashDict, empty, insert, contains, get, remove ]
    imports [ Wyhash ]

# This is based off of absl::flat_hash_map.
# It is simplified to make it nicer to write in roc.
defaultSlotCount : Nat
defaultSlotCount = 64

maxLoadFactor : F64
maxLoadFactor = 0.875

emptySlot : I8
emptySlot = -128
deletedSlot : I8
deletedSlot = -2
# sentinel : I8
# sentinel = -1
Option a : [ Some a, None ]

Elem a : [ T U64 a ]

U64FlatHashDict a := {
        data : List (Elem a),
        metadata : List I8,
        size : Nat,
        default : Elem a,
    }

# This requires an element because we don't know how to generate a default elem.
# For simplicity for now, I am just storing the default value.
empty : a -> U64FlatHashDict a
empty = \default ->
    defaultElem = T 0 default

    $U64FlatHashDict
        {
            data: [],
            metadata: [],
            size: 0,
            default: defaultElem,
        }

contains : U64FlatHashDict a, U64 -> Bool
contains = \dict, key ->
    # TODO
    False

get : U64FlatHashDict a, U64 -> Option a
get = \dict, key ->
    # TODO
    None

insert : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insert = \dict, key, value ->
    insertInternal (maybeRehash dict) key value

remove : U64FlatHashDict a, U64 -> U64FlatHashDict a
remove = \dict, key ->
    # TODO
    dict

# Does insertion without potentially rehashing.
insertInternal : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insertInternal = \$U64FlatHashDict { data, metadata, size, default }, key, value ->
    hashKey = hash key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    index =
        when h1Key % Num.toU64 (List.len data) is
            Ok i ->
                indexHelper metadata (Num.toNat i)

            Err DivByZero ->
                # This should never happen. Panic.
                0 - 1

    $U64FlatHashDict
        {
            data: List.set data index (T key value),
            metadata: List.set metadata index h2Key,
            size,
            default,
        }

indexHelper : List I8, Nat -> Nat
indexHelper = \metadata, index ->
    when List.get metadata index is
        Ok md ->
            if md < 0 then
                # Deleted or empty slot
                index
            else
                # Used slot, check next slot
                indexHelper metadata (index + 1)

        Err OutOfBounds ->
            # loop back to begining of list
            indexHelper metadata 0

# This is how we grow the container.
# If we aren't to the load factor yet, just ignore this.
maybeRehash : U64FlatHashDict a -> U64FlatHashDict a
maybeRehash = \$U64FlatHashDict { data, metadata, size, default } ->
    when Num.toFloat size / Num.toFloat (List.len data) is
        Ok loadFactor ->
            if loadFactor >= maxLoadFactor then
                rehash ($U64FlatHashDict { data, metadata, size, default })
            else
                $U64FlatHashDict { data, metadata, size, default }

        Err DivByZero ->
            rehash ($U64FlatHashDict { data, metadata, size, default })

rehash : U64FlatHashDict a -> U64FlatHashDict a
rehash = \$U64FlatHashDict { data, metadata, size, default } ->
    newLen =
        if List.isEmpty data then
            defaultSlotCount
        else
            2 * List.len data

    newDict = $U64FlatHashDict
        {
            data: List.repeat default newLen,
            metadata: List.repeat emptySlot newLen,
            size,
            default,
        }

    rehashHelper newDict metadata data 0

rehashHelper : U64FlatHashDict a, List I8, List (Elem a), Nat -> U64FlatHashDict a
rehashHelper = \dict, metadata, data, index ->
    when List.get metadata index is
        Ok md ->
            nextDict =
                if md > 0 then
                    # We have an actual element here
                    when List.get data index is
                        Ok (T k v) ->
                            insertInternal dict k v

                        Err OutOfBounds ->
                            # This should be an impossible state since data and metadata are the same size
                            dict
                else
                    # Empty or deleted data
                    dict

            rehashHelper nextDict metadata data (index + 1)

        Err OutOfBounds ->
            dict

h1 : U64 -> U64
h1 = \hashKey ->
    Num.shiftRightZfBy 7 hashKey

h2 : U64 -> I8
h2 = \hashKey ->
    Num.toI8 (Num.bitwiseAnd hashKey 127)

# This is just Wyhash.
hash : U64 -> U64
hash = \key ->
    # TODO
    key