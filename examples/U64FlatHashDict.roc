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

# Metadata should be grouped. This helps make loading faster.
# One memory load would then load 8 possible indexes.
# These could then be compared with vector instructions (if added to roc).
# To start just making it non-grouped for simplicity.
U64FlatHashDict a := {
        data : List (Elem a),
        metadata : List I8,
        size : Nat,
        default : Elem a,
        seed : Wyhash.Seed,
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
            seed: Wyhash.createSeed 0x0123_4567_89AB_CDEF,
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
insertInternal = \$U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    index =
        indexHelper metadata data h2Key key (Num.toNat h1Key)

    $U64FlatHashDict
        {
            data: List.set data index (T key value),
            metadata: List.set metadata index h2Key,
            size: size + 1,
            default,
            seed,
        }

indexHelper : List I8, List (Elem a), I8, U64, Nat -> Nat
indexHelper = \metadata, data, h2Key, key, oversizedIndex ->
    # we know that the length data is always a power of 2.
    # as such, we can just and with the length - 1.
    index = Num.bitwiseAnd oversizedIndex (List.len metadata - 1)
    when List.get metadata index is
        Ok md ->
            if md < 0 then
                # Deleted or empty slot
                index
            else if md == h2Key then
                # This is potentially a match.
                # Check data for if it is a match.
                when List.get data index is
                    Ok (T k v) ->
                        if k == key then
                            # we have a match, return it's index
                            index
                        else
                            # no match, keep checking.
                            indexHelper metadata data h2Key key (index + 1)

                    Err OutOfBounds ->
                        # not possible. just panic
                        0 - 1
            else
                # Used slot, check next slot
                indexHelper metadata data h2Key key (index + 1)

        Err OutOfBounds ->
            # not possible. just panic
            0 - 1

# This is how we grow the container.
# If we aren't to the load factor yet, just ignore this.
maybeRehash : U64FlatHashDict a -> U64FlatHashDict a
maybeRehash = \$U64FlatHashDict { data, metadata, size, default, seed } ->
    when (Num.toFloat size) / (Num.toFloat (List.len data)) is
        Ok loadFactor ->
            if loadFactor >= maxLoadFactor then
                rehash ($U64FlatHashDict { data, metadata, size, default, seed })
            else
                $U64FlatHashDict { data, metadata, size, default, seed }

        Err DivByZero ->
            rehash ($U64FlatHashDict { data, metadata, size, default, seed })

rehash : U64FlatHashDict a -> U64FlatHashDict a
rehash = \$U64FlatHashDict { data, metadata, size, default, seed } ->
    if List.isEmpty data then
        newLen = defaultSlotCount
        $U64FlatHashDict
            {
                data: List.repeat default newLen,
                metadata: List.repeat emptySlot newLen,
                size,
                default,
                seed,
            }
    else
        newLen = 2 * (List.len data)
        newDict =
            $U64FlatHashDict
                {
                    data: List.repeat default newLen,
                    metadata: List.repeat emptySlot newLen,
                    size,
                    default,
                    seed,
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
    Num.toI8 (Num.bitwiseAnd hashKey 0b0111_1111)