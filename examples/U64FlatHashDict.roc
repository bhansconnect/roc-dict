interface U64FlatHashDict
    exposes [ U64FlatHashDict, empty, insert, contains, get, remove, clear, capacity, len ]
    imports [ Wyhash ]

# This is based off of absl::flat_hash_map.
# It is simplified to make it nicer to write in roc.
allEmpty : I64
allEmpty = Num.toI64 0x8080_8080_8080_8080
emptySlot : I8
emptySlot = -128
deletedSlot : I8
deletedSlot = -2
# sentinel : I8
# sentinel = -1

# Must be a multiple of 2 and equivalent to the number of bytes in allEmpty.
groupSize : Nat
groupSize = 8

Option a : [ Some a, None ]

Elem a : [ T U64 a ]

U64FlatHashDict a := {
        data : List (Elem a),
        metadata : List I64,
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
contains = \$U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    when probeMD metadata data key h1Key h2Key is 
        Found _ _ ->
            True
        NotFound _ ->
            False

get : U64FlatHashDict a, U64 -> Option a
get = \$U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    when probeMD metadata data key h1Key h2Key is 
        Found _ v ->
            Some v
        NotFound _ ->
            None

insert : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insert = \dict, key, value ->
    insertInternal (maybeRehash dict) key value

len : U64FlatHashDict a -> Nat
len = \$U64FlatHashDict { size } ->
    size

capacity : U64FlatHashDict a -> Nat
capacity = \$U64FlatHashDict { data } ->
    List.len data

remove : U64FlatHashDict a, U64 -> [ T (U64FlatHashDict a) Bool ]
remove = \$U64FlatHashDict { data, metadata, size, default, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    when probeMD metadata data key h1Key h2Key is
        Found { slotIndex, offset, loaded } _ ->
            T ($U64FlatHashDict
                {
                    data: data,
                    metadata: List.set metadata slotIndex (updateAtOffset loaded offset deletedSlot),
                    size: size - 1,
                    default,
                    seed,
                }) True
        NotFound _ ->
            T ($U64FlatHashDict { data, metadata, size, default, seed }) False

clear : U64FlatHashDict a -> U64FlatHashDict a
clear = \$U64FlatHashDict { data, metadata, default, seed } ->
    slots = List.len metadata
    # Only clear large allocations.
    if slots > 128 then
        when default is
            T _ v ->
                empty v
    else
        $U64FlatHashDict {
            data: List.map data (\_ -> default),
            metadata: List.map metadata (\_ -> allEmpty),
            size: 0,
            default,
            seed
        }

# Does insertion without potentially rehashing.
insertInternal : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insertInternal = \$U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = h1 hashKey
    h2Key = h2 hashKey
    when probeMD metadata data key h1Key h2Key is
        Found { slotIndex, offset } _ ->
            dataIndex = slotIndex * groupSize + offset
            $U64FlatHashDict
                {
                    data: List.set data dataIndex (T key value),
                    metadata, # metadata will already be correct if we found the key
                    size,
                    default,
                    seed,
                }
        NotFound { slotIndex, offset, loaded } ->
            dataIndex = slotIndex * groupSize + offset
            $U64FlatHashDict
                {
                    data: List.set data dataIndex (T key value),
                    metadata: List.set metadata slotIndex (updateAtOffset loaded offset h2Key),
                    size: size + 1,
                    default,
                    seed,
                }


Position : { slotIndex: Nat, offset: Nat, loaded: I64 }
ProbeResult a: [ Found Position a, NotFound Position ]

loadAtOffset : I64, Nat -> I8
loadAtOffset = \val, offset ->
    Num.toI8 (shiftRightZfByHack (offset * 8) (Num.toNat val))

updateAtOffset : I64, Nat, I8 -> I64
updateAtOffset = \val, offset, updateVal ->
    bitOffset = offset * 8
    # No bitwiseNot update when added.
    mask = Num.bitwiseXor 0xFFFF_FFFF_FFFF_FFFF (Num.shiftLeftBy bitOffset 0xFF)
    update = Num.shiftLeftBy bitOffset (Num.toNat updateVal)
    Num.bitwiseOr (Num.bitwiseAnd val (Num.toI64 mask)) (Num.toI64 update)

probeMD: List I64, List (Elem a), U64, U64, I8 -> ProbeResult a
probeMD = \md, data, key, h1Key, h2Key ->
    slotIndex = Num.bitwiseAnd (Num.toNat h1Key) (List.len md - 1)
    when List.get md slotIndex is
        Ok loaded ->
            probeMDHelper md data key h2Key 0 {loaded, slotIndex, offset: 0} None
        Err OutOfBounds ->
            # not possible. just panic
            NotFound { slotIndex: 0 - 1, offset: 0, loaded: 0 }


# I really hope this giant function inlines.
# I think it will be needed for performance. Otherwise I will have to make like 4 slightly different copies.
# In the NotFound case this will return the first empty or deleted index.
probeMDHelper: List I64, List (Elem a), U64, I8, Nat, Position, Option Position -> ProbeResult a
probeMDHelper = \md, data, key, h2Key, probeI, { slotIndex, offset, loaded }, deletedIndex ->
    if offset < groupSize then
        # Hopefully the toNat doesn't break anything here. Types are angry.
        byte = loadAtOffset loaded offset
        if byte == emptySlot then
            # No more possible data.
            # return the first tombstone index.
            when deletedIndex is
                Some pos ->
                    NotFound pos
                None ->
                    NotFound { slotIndex, offset, loaded }
        else if byte == h2Key then
            # We potentially found the element.
            # Just need to check the key.
            dataIndex = slotIndex * groupSize + offset
            when List.get data dataIndex is
                Ok (T k v) ->
                    if k == key then
                        # we have a match, return it's index
                        Found { slotIndex, offset, loaded } v
                    else
                        # No match, keep checking.
                        # indexInsertHelper metadata data h2Key key (index + 1)
                        probeMDHelper md data key h2Key probeI { slotIndex, offset: (offset + 1), loaded } deletedIndex

                Err OutOfBounds ->
                    # not possible. just panic
                    NotFound { slotIndex: 0 - 1, offset: 0, loaded: 0 }
        else if byte == deletedSlot then
            # Found a deleted item. Store the index because we might insert here
            # Otherwise, just move on.
            deletedPos =
                when deletedIndex is
                    Some pos ->
                        pos
                    None ->
                        { slotIndex, offset, loaded }
            probeMDHelper md data key h2Key probeI { slotIndex, offset: (offset + 1), loaded } (Some deletedPos)
        else
            # Just continue, this is a used slot
            probeMDHelper md data key h2Key probeI { slotIndex, offset: (offset + 1), loaded } deletedIndex
    else
        # The offset is too large, which means we need to load the next chunk of metadata
        newProbeI = probeI + 1
        newSlotIndex = Num.bitwiseAnd (slotIndex + newProbeI) (List.len md - 1)
        newOffset = 0
        when List.get md newSlotIndex is
            Ok newLoaded ->
                probeMDHelper md data key h2Key newProbeI { slotIndex: newSlotIndex, offset: newOffset, loaded: newLoaded } deletedIndex
            Err OutOfBounds ->
                # not possible. just panic
                NotFound { slotIndex: 0 - 1, offset: 0, loaded: 0 }


shiftRightZfByHack = \by, val ->
    Num.shiftRightBy by val

# This is how we grow the container.
# If we aren't to the load factor yet, just ignore this.
maybeRehash : U64FlatHashDict a -> U64FlatHashDict a
maybeRehash = \$U64FlatHashDict { data, metadata, size, default, seed } ->
    cap = List.len data
    maxLoadCap =
            # This is 7/8 * capacity, which is the max load factor.
            cap - (shiftRightZfByHack 3 cap)
    if size >= maxLoadCap then
        rehash ($U64FlatHashDict { data, metadata, size, default, seed })
    else
        $U64FlatHashDict { data, metadata, size, default, seed }

rehash : U64FlatHashDict a -> U64FlatHashDict a
rehash = \$U64FlatHashDict { data, metadata, size, default, seed } ->
    if List.isEmpty data then
        $U64FlatHashDict
            {
                data: List.repeat default groupSize,
                metadata: [allEmpty],
                size,
                default,
                seed,
            }
    else
        newDict =
            $U64FlatHashDict
                {
                    data: List.repeat default (2 * List.len data),
                    metadata: List.repeat allEmpty (2 * List.len metadata),
                    size: 0,
                    default,
                    seed,
                }
        rehashHelper newDict metadata data 0

maybeInsertForRehash : U64FlatHashDict a, I8, List (Elem a), Nat -> U64FlatHashDict a
maybeInsertForRehash = \$U64FlatHashDict { data, metadata, size, default, seed }, oldH2Key, oldData, oldDataIndex ->
    if oldH2Key >= 0 then
        when List.get oldData oldDataIndex is
            Ok (T key value) ->
                hashKey = Wyhash.hashU64 seed key
                h1Key = h1 hashKey
                h2Key = h2 hashKey
                when probeMD metadata data key h1Key h2Key is
                    NotFound { slotIndex, offset, loaded } ->
                        dataIndex = slotIndex * groupSize + offset
                        $U64FlatHashDict
                            {
                                data: List.set data dataIndex (T key value),
                                metadata: List.set metadata slotIndex (updateAtOffset loaded offset h2Key),
                                size,
                                default,
                                seed,
                            }
                    Found _ _ ->
                        # Panic this should never happen.
                        $U64FlatHashDict
                            {
                                data,
                                metadata,
                                size: 0 - 1,
                                default,
                                seed,
                            }

            Err OutOfBounds ->
                # This should be an impossible state since data and metadata are the same size
                $U64FlatHashDict
                    {
                        data,
                        metadata,
                        size: 0 - 1,
                        default,
                        seed,
                    }
    else
        $U64FlatHashDict
            {
                data,
                metadata,
                size,
                default,
                seed,
            }


rehashHelper : U64FlatHashDict a, List I64, List (Elem a), Nat -> U64FlatHashDict a
rehashHelper = \dict, metadata, data, slotIndex ->
    when List.get metadata slotIndex is
        Ok md ->
            s0 = loadAtOffset md 0
            s1 = loadAtOffset md 1
            s2 = loadAtOffset md 2
            s3 = loadAtOffset md 3
            s4 = loadAtOffset md 4
            s5 = loadAtOffset md 5
            s6 = loadAtOffset md 6
            s7 = loadAtOffset md 7
            dataIndex = slotIndex * groupSize
            nextDict0 = maybeInsertForRehash dict s0 data dataIndex
            nextDict1 = maybeInsertForRehash nextDict0 s1 data (dataIndex + 1)
            nextDict2 = maybeInsertForRehash nextDict1 s2 data (dataIndex + 2)
            nextDict3 = maybeInsertForRehash nextDict2 s3 data (dataIndex + 3)
            nextDict4 = maybeInsertForRehash nextDict3 s4 data (dataIndex + 4)
            nextDict5 = maybeInsertForRehash nextDict4 s5 data (dataIndex + 5)
            nextDict6 = maybeInsertForRehash nextDict5 s6 data (dataIndex + 6)
            nextDict7 = maybeInsertForRehash nextDict6 s7 data (dataIndex + 7)

            rehashHelper nextDict7 metadata data (slotIndex + 1)

        Err OutOfBounds ->
            dict

h1 : U64 -> U64
h1 = \hashKey ->
    shiftRightZfByHack 7 hashKey

h2 : U64 -> I8
h2 = \hashKey ->
    Num.toI8 (Num.bitwiseAnd hashKey 0b0111_1111)
