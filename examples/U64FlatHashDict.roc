interface U64FlatHashDict
    exposes [ U64FlatHashDict, empty, insert, contains, get, remove, clear, capacity, len ]
    imports [ Wyhash ]

# This is based off of absl::flat_hash_map.
# It is simplified to make it nicer to write in roc.
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
        # TODO: switch to power of 2 minus 1 sizes of slots and u64 for metadata.
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

    @U64FlatHashDict
        {
            data: [],
            metadata: [],
            size: 0,
            default: defaultElem,
            seed: Wyhash.createSeed 0x0123_4567_89AB_CDEF,
        }

contains : U64FlatHashDict a, U64 -> Bool
contains = \@U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey

    when indexFindHelper metadata data h2Key key (Num.toNat h1Key) 0 8 is
        T (Found _) _ ->
            True

        _ ->
            False

get : U64FlatHashDict a, U64 -> Option a
get = \@U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey

    when indexFindHelper metadata data h2Key key (Num.toNat h1Key) 0 8 is
        T (Found v) _ ->
            Some v

        _ ->
            None

insert : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insert = \dict, key, value ->
    insertInternal (maybeRehash dict) key value

len : U64FlatHashDict a -> Nat
len = \@U64FlatHashDict { size } ->
    size

capacity : U64FlatHashDict a -> Nat
capacity = \@U64FlatHashDict { data } ->
    List.len data

remove : U64FlatHashDict a, U64 -> [ T (U64FlatHashDict a) Bool ]
remove = \@U64FlatHashDict { data, metadata, size, default, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey

    when indexFindHelper metadata data h2Key key (Num.toNat h1Key) 0 8 is
        T (Found _) index ->
            T (@U64FlatHashDict { data, metadata: List.set metadata index deletedSlot, size: (size - 1), default, seed }) True

        _ ->
            T (@U64FlatHashDict { data, metadata, size, default, seed }) False

clear : U64FlatHashDict a -> U64FlatHashDict a
clear = \@U64FlatHashDict { data, metadata, default, seed } ->
    cap = List.len data
    # Only clear large allocations.
    if cap > 128 * 8 then
        when default is
            T _ v ->
                empty v
    else
        @U64FlatHashDict {
            data: List.map data (\_ -> default),
            metadata: List.map metadata (\_ -> emptySlot),
            size: 0,
            default,
            seed
        }

# Does insertion without potentially rehashing.
insertInternal : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insertInternal = \@U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey

    when indexInsertHelper metadata data h2Key key (Num.toNat h1Key) 0 8 is
        Found index ->
            @U64FlatHashDict
                {
                    data: List.set data index (T key value),
                    metadata: List.set metadata index h2Key,
                    size: size + 1,
                    default,
                    seed,
                }
        NotFound _ ->
            # Need to rescan searching for the sirt empty or deleted cell.
            index = fillEmptyOrDeletedHelper metadata data h2Key key (Num.toNat h1Key) 0 8
            @U64FlatHashDict
                {
                    data: List.set data index (T key value),
                    metadata: List.set metadata index h2Key,
                    size: size + 1,
                    default,
                    seed,
                }

insertInEmptyOrDeleted : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insertInEmptyOrDeleted = \@U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey

    index = fillEmptyOrDeletedHelper metadata data h2Key key (Num.toNat h1Key) 0 8
    @U64FlatHashDict
        {
            data: List.set data index (T key value),
            metadata: List.set metadata index h2Key,
            size,
            default,
            seed,
        }

fillEmptyOrDeletedHelper : List I8, List (Elem a), I8, U64, Nat, Nat, Nat -> Nat
fillEmptyOrDeletedHelper = \metadata, data, h2Key, key, oversizedIndex, offset, probeI ->
    # For inserting, we can use deleted indices.
    # we know that the length data is always a power of 2.
    # as such, we can just and with the length - 1.
    index = Num.bitwiseAnd (oversizedIndex + offset) (List.len metadata - 1)

    when List.get metadata index is
        Ok md ->
            if md < 0 then
                # Empty or deleted slot no possibility of the element
                index
            else
                # Used slot, check next slot
                if offset == 7 then
                    fillEmptyOrDeletedHelper metadata data h2Key key (index + probeI) 0 (probeI + 8)
                else
                    fillEmptyOrDeletedHelper metadata data h2Key key index (offset + 1) probeI

        Err OutOfBounds ->
            # not possible. just panic
            (0 - 1)

indexInsertHelper : List I8, List (Elem a), I8, U64, Nat, Nat, Nat -> [ Found Nat, NotFound Nat ]
indexInsertHelper = \metadata, data, h2Key, key, oversizedIndex, offset, probeI ->
    # For inserting, we can use deleted indices.
    # we know that the length data is always a power of 2.
    # as such, we can just and with the length - 1.
    index = Num.bitwiseAnd (oversizedIndex + offset) (List.len metadata - 1)

    when List.get metadata index is
        Ok md ->
            if md == emptySlot then
                # Empty slot, no possibility of the element
                NotFound index
            else if md == h2Key then
                # This is potentially a match.
                # Check data for if it is a match.
                when List.get data index is
                    Ok (T k _) ->
                        if k == key then
                            # we have a match, return it's index
                            Found index
                        else
                            # no match, keep checking.
                            if offset == 7 then
                                indexInsertHelper metadata data h2Key key (index + probeI) 0 (probeI + 8)
                            else
                                indexInsertHelper metadata data h2Key key index (offset + 1) probeI

                    Err OutOfBounds ->
                        # not possible. just panic
                        NotFound (0 - 1)
            else
                # Used slot, check next slot
                if offset == 7 then
                    indexInsertHelper metadata data h2Key key (index + probeI) 0 (probeI + 8)
                else
                    indexInsertHelper metadata data h2Key key index (offset + 1) probeI

        Err OutOfBounds ->
            # not possible. just panic
            NotFound (0 - 1)

indexFindHelper : List I8, List (Elem a), I8, U64, Nat, Nat, Nat -> [ T [ Found a, NotFound ] Nat ]
indexFindHelper = \metadata, data, h2Key, key, oversizedIndex, offset, probeI ->
    # For finding we have to scan past deleted items.
    # we know that the length data is always a power of 2.
    # as such, we can just and with the length - 1.
    index = Num.bitwiseAnd (oversizedIndex + offset) (List.len metadata - 1)

    when List.get metadata index is
        Ok md ->
            if md == emptySlot then
                # Empty slot no possibility of the element
                T NotFound index
            else if md == h2Key then
                # This is potentially a match.
                # Check data for if it is a match.
                when List.get data index is
                    Ok (T k v) ->
                        if k == key then
                            # we have a match, return it's index
                            T (Found v) index
                        else
                            # no match, keep checking.
                            if offset == 7 then
                                indexFindHelper metadata data h2Key key (index + probeI) 0 (probeI + 8)
                            else
                                indexFindHelper metadata data h2Key key index (offset + 1) probeI


                    Err OutOfBounds ->
                        # not possible. just panic
                        T NotFound (0 - 1)
            else
                # Used or deleted slot, check next slot
                if offset == 7 then
                    indexFindHelper metadata data h2Key key (index + probeI) 0 (probeI + 8)
                else
                    indexFindHelper metadata data h2Key key index (offset + 1) probeI

        Err OutOfBounds ->
            # not possible. just panic
            T NotFound (0 - 1)

# This is how we grow the container.
# If we aren't to the load factor yet, just ignore this.
maybeRehash : U64FlatHashDict a -> U64FlatHashDict a
maybeRehash = \@U64FlatHashDict { data, metadata, size, default, seed } ->
    cap = List.len data
    maxLoadCap =
            # This is 7/8 * capacity, which is the max load factor.
            cap - (Num.shiftRightZfBy 3 cap)
    if size >= maxLoadCap then
        rehash (@U64FlatHashDict { data, metadata, size, default, seed })
    else
        @U64FlatHashDict { data, metadata, size, default, seed }

rehash : U64FlatHashDict a -> U64FlatHashDict a
rehash = \@U64FlatHashDict { data, metadata, size, default, seed } ->
    if List.isEmpty data then
        @U64FlatHashDict
            {
                data: List.repeat default 8,
                metadata: List.repeat emptySlot 8,
                size,
                default,
                seed,
            }
    else
        newLen = 2 * List.len data
        newDict =
            @U64FlatHashDict
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
    if index < List.len metadata then
        when List.get metadata index is
            Ok md ->
                nextDict =
                    if md >= 0 then
                        # We have an actual element here
                        when List.get data index is
                            Ok (T k v) ->
                                insertInEmptyOrDeleted dict k v

                            Err OutOfBounds ->
                                # This should be an impossible state since data and metadata are the same size
                                dict
                    else
                        # Empty or deleted data
                        dict

                rehashHelper nextDict metadata data (index + 1)

            Err OutOfBounds ->
                dict
    else
        dict

h1 : U64 -> U64
h1 = \hashKey ->
    Num.shiftRightZfBy 7 hashKey

h2 : U64 -> I8
h2 = \hashKey ->
    Num.toI8 (Num.bitwiseAnd hashKey 0b0111_1111)
