interface U64FlatHashDict
    exposes [ U64FlatHashDict, empty, insert, contains, get, remove, clear, capacity, len ]
    imports [ Wyhash, BitMask.{ BitMask }, Group.{ Group } ]

Elem a : [ T U64 a ]

U64FlatHashDict a := {
        data : List (Elem a),
        metadata : List Group,
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

len : U64FlatHashDict a -> Nat
len = \$U64FlatHashDict { size } ->
    size

capacity : U64FlatHashDict a -> Nat
capacity = \$U64FlatHashDict { data } ->
    List.len data

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
            metadata: List.map metadata (\_ -> Group.allEmpty),
            size: 0,
            default,
            seed
        }

# Capacity must be a power of 2.
Probe : { slotIndex: Nat, probeI: Nat, mask: Nat }

newProbe : U64, Nat -> Probe
newProbe = \h1Key, slots ->
    mask = Num.subSaturated slots 1
    slotIndex = Num.bitwiseAnd (Num.toNat h1Key) mask
    { slotIndex, probeI: 1, mask }

nextProbe : Probe -> Probe
nextProbe = \{ slotIndex, probeI, mask } ->
    nextSlotIndex = Num.bitwiseAnd (Num.addWrap slotIndex probeI) mask
    { slotIndex: nextSlotIndex, probeI: Num.addWrap probeI 1, mask }

contains : U64FlatHashDict a, U64 -> Bool
contains = \$U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = Group.h1 hashKey
    h2Key = Group.h2 hashKey
    probe = newProbe h1Key (List.len metadata)
    # TODO: verify this optimizes correctly and isn't slow due to copying around the value.
    when getHelper data metadata probe h2Key key is
        Ok _ -> True
        Err NotFound -> False

get : U64FlatHashDict a, U64 -> Result a [ NotFound ]
get = \$U64FlatHashDict { data, metadata, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = Group.h1 hashKey
    h2Key = Group.h2 hashKey
    probe = newProbe h1Key (List.len metadata)
    getHelper data metadata probe h2Key key

getHelper : List (Elem a), List Group, Probe, Group.H2, U64 -> Result a [ NotFound ]
getHelper = \data, metadata, { slotIndex, probeI, mask }, h2Key, key ->
    when List.get metadata slotIndex is
        Ok group ->
            h2Match = Group.match group h2Key
            found =
                BitMask.walkUntil h2Match (Err NotFound) (\_, offset ->
                    dataIndex = Num.addWrap (Group.mulSize slotIndex) offset
                    when List.get data dataIndex is
                        Ok (T k v) ->
                            if k == key then
                                # we have a match, return its value
                                Stop (Ok v)
                            else
                                Continue (Err NotFound)
                        Err OutOfBounds ->
                            # This should not be possible, maybe panic
                            Stop (Err NotFound)
                )
            when found is
                Ok v ->
                    Ok v
                Err NotFound ->
                    if BitMask.any (Group.matchEmpty group) then
                        # Group has empty, definitely not found
                        Err NotFound
                    else
                        # Group is full, check next group.
                        np = nextProbe { slotIndex, probeI, mask }
                        getHelper data metadata np h2Key key
        Err OutOfBounds ->
            # This should not be possible, maybe panic
            Err NotFound

remove : U64FlatHashDict a, U64 -> [ T (U64FlatHashDict a) Bool ]
remove = \$U64FlatHashDict { data, metadata, size, default, seed }, key ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = Group.h1 hashKey
    h2Key = Group.h2 hashKey
    probe = newProbe h1Key (List.len metadata)
    removeHelper ($U64FlatHashDict { data, metadata, size, default, seed }) probe h2Key key

removeHelper : U64FlatHashDict a, Probe, Group.H2, U64 -> [ T (U64FlatHashDict a) Bool ]
removeHelper = \$U64FlatHashDict { data, metadata, size, default, seed }, { slotIndex, probeI, mask }, h2Key, key ->
    when List.get metadata slotIndex is
        Ok group ->
            h2Match = Group.match group h2Key
            found =
                BitMask.walkUntil h2Match (Err NotFound) (\_, offset ->
                    dataIndex = Num.addWrap (Group.mulSize slotIndex) offset
                    when List.get data dataIndex is
                        Ok (T k _) ->
                            if k == key then
                                # we have a match, return its offset
                                Stop (Ok offset)
                            else
                                Continue (Err NotFound)
                        Err OutOfBounds ->
                            # This should not be possible, maybe panic
                            Stop (Err NotFound)
                )
            when found is
                Ok offset ->
                    newGroup = Group.setDeletedAtOffset group offset
                    newDict =
                        $U64FlatHashDict
                            {
                                data,
                                metadata: List.set metadata slotIndex newGroup,
                                size: Num.subWrap size 1,
                                default,
                                seed,
                            }
                    T newDict True
                Err NotFound ->
                    emptyMask = Group.matchEmpty group
                    if BitMask.any emptyMask then
                        T ($U64FlatHashDict { data, metadata, size, default, seed }) False
                    else
                        # Group is full, check next group.
                        np = nextProbe { slotIndex, probeI, mask }
                        removeHelper ($U64FlatHashDict { data, metadata, size, default, seed }) np h2Key key
        Err OutOfBounds ->
            # This should not be possible, maybe panic
            T ($U64FlatHashDict { data, metadata, size, default, seed }) False

insert : U64FlatHashDict a, U64, a -> [ T (U64FlatHashDict a) Nat ]
insert = \$U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = Group.h1 hashKey
    h2Key = Group.h2 hashKey
    probe = newProbe h1Key (List.len metadata)
    when insertHelper ($U64FlatHashDict { data, metadata, size, default, seed }) probe h2Key key value 0 is
        Inserted dict loads ->
            T dict loads
        NeedsInsert dict loads ->
            # probe must be recalculated because there may have been a rehash.
            ($U64FlatHashDict dictInternal) = dict
            probe2 = newProbe h1Key (List.len dictInternal.metadata)
            insertInFirstEmptyOrDeleted dict probe2 h2Key key value loads

insertHelper : U64FlatHashDict a, Probe, Group.H2, U64, a, Nat -> [ Inserted (U64FlatHashDict a) Nat, NeedsInsert (U64FlatHashDict a) Nat ]
insertHelper = \$U64FlatHashDict { data, metadata, size, default, seed }, { slotIndex, probeI, mask }, h2Key, key, value, loads ->
    when List.get metadata slotIndex is
        Ok group ->
            nextLoads = loads + 1
            h2Match = Group.match group h2Key
            found =
                BitMask.walkUntil h2Match (Err NotFound) (\_, offset ->
                    dataIndex = Num.addWrap (Group.mulSize slotIndex) offset
                    when List.get data dataIndex is
                        Ok (T k _) ->
                            if k == key then
                                # we have a match, return its offset
                                Stop (Ok offset)
                            else
                                Continue (Err NotFound)
                        Err OutOfBounds ->
                            # This should not be possible, maybe panic
                            Stop (Err NotFound)
                )
            when found is
                Ok offset ->
                    dataIndex = Num.addWrap (Group.mulSize slotIndex) offset
                    Inserted ($U64FlatHashDict
                        {
                            data: List.set data dataIndex (T key value),
                            metadata, # metadata will already be correct if we found the key
                            size,
                            default,
                            seed,
                        }) nextLoads
                Err NotFound ->
                    emptyMask = Group.matchEmpty group
                    if BitMask.any emptyMask then
                        # Group has empty, insert in the first empty or deleted slot.
                        # We are adding a new element, this may require a rehash.
                        (T rehashedDict rehashLoads) = maybeRehash ($U64FlatHashDict
                            {
                                data,
                                metadata,
                                size: Num.addWrap size 1,
                                default,
                                seed,
                            })
                        NeedsInsert rehashedDict (loads + rehashLoads)
                    else
                        # Group is full, check next group.
                        np = nextProbe { slotIndex, probeI, mask }
                        insertHelper ($U64FlatHashDict { data, metadata, size, default, seed }) np h2Key key value nextLoads
        Err OutOfBounds ->
            # This will only happen if the dictionary is completely empty.
            # Rehash and then insert.
            (T rehashedDict rehashLoads) = maybeRehash ($U64FlatHashDict
                {
                    data,
                    metadata,
                    size: Num.addWrap size 1,
                    default,
                    seed,
                })
            NeedsInsert rehashedDict (loads + rehashLoads)

# This will not check for key matches.
# It should only be used when we know the key won't match.
insertInFirstEmptyOrDeleted : U64FlatHashDict a, Probe, Group.H2, U64, a, Nat -> [ T (U64FlatHashDict a) Nat ]
insertInFirstEmptyOrDeleted = \$U64FlatHashDict { data, metadata, size, default, seed }, { slotIndex, probeI, mask }, h2Key, key, value, loads ->
    when List.get metadata slotIndex is
        Ok group ->
            nextLoads = loads + 1
            emptyOrDeletedMask = Group.matchEmpty group
            if BitMask.any emptyOrDeletedMask then
                # We found a spot to insert in.
                offset = BitMask.lowestSet emptyOrDeletedMask
                dataIndex = Num.addWrap (Group.mulSize slotIndex) offset
                newGroup = Group.updateKeyAtOffset group offset h2Key
                T ($U64FlatHashDict
                    {
                        data: List.set data dataIndex (T key value),
                        metadata: List.set metadata slotIndex newGroup,
                        size,
                        default,
                        seed,
                    }) nextLoads
            else
                # Group is full, check next group.
                np = nextProbe { slotIndex, probeI, mask }
                insertInFirstEmptyOrDeleted ($U64FlatHashDict { data, metadata, size, default, seed }) np h2Key key value nextLoads
        Err OutOfBounds ->
            # This should not be possible, maybe panic
            T ($U64FlatHashDict { data, metadata, size: 0 - 1, default, seed }) loads

# This is how we grow the container.
# If adding an element would cause us to reach load factor, we must rehash.
maybeRehash : U64FlatHashDict a -> [ T (U64FlatHashDict a) Nat ]
maybeRehash = \$U64FlatHashDict { data, metadata, size, default, seed } ->
    cap = List.len data
    maxLoadCap =
            # This is 7/8 * capacity, which is the max load factor.
            Num.subWrap cap (shiftRightZfByHack 3 cap)
    if size >= maxLoadCap then
        rehash ($U64FlatHashDict { data, metadata, size, default, seed })
    else
        T ($U64FlatHashDict { data, metadata, size, default, seed }) 0

rehash : U64FlatHashDict a -> [T (U64FlatHashDict a) Nat ]
rehash = \$U64FlatHashDict { data, metadata, size, default, seed } ->
    if List.isEmpty data then
        T ($U64FlatHashDict
            {
                data: List.repeat default Group.size,
                metadata: [Group.allEmpty],
                size,
                default,
                seed,
            }) 0
    else
        newDict =
            $U64FlatHashDict
                {
                    data: List.repeat default (2 * List.len data),
                    metadata: List.repeat Group.allEmpty (2 * List.len metadata),
                    size,
                    default,
                    seed,
                }
        rehashHelper newDict metadata data 0 0

rehashHelper : U64FlatHashDict a, List Group, List (Elem a), Nat, Nat -> [ T (U64FlatHashDict a) Nat ]
rehashHelper = \dict, oldMetadata, oldData, slotIndex, loads ->
    ($U64FlatHashDict {seed, metadata}) = dict
    slots = List.len metadata
    when List.get oldMetadata slotIndex is
        Ok group ->
            nextLoads = loads + 1
            matchFull = Group.matchFull group
            (T nextDict finalLoads) =
                BitMask.walk matchFull (T dict nextLoads) (\(T currentDict currentLoads), offset ->
                    dataIndex = Num.addWrap (Group.mulSize slotIndex) offset
                    when List.get oldData dataIndex is
                        Ok (T k v) ->
                            hashKey = Wyhash.hashU64 seed k
                            h1Key = Group.h1 hashKey
                            h2Key = Group.h2 hashKey
                            probe = newProbe h1Key slots
                            insertInFirstEmptyOrDeleted currentDict probe h2Key k v currentLoads
                        Err OutOfBounds ->
                            # This should not be possible, maybe panic
                            T currentDict nextLoads
                )
            rehashHelper nextDict oldMetadata oldData (Num.addWrap slotIndex 1) finalLoads

        Err OutOfBounds ->
            # We have walked the entire list.
            # The new dict is loaded.
            T dict loads

# This is broken. Zf and normal are filled.
shiftRightZfByHack = \by, val ->
    Num.shiftRightBy by val
