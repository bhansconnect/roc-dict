interface U64FlatHashDict
    exposes [ U64FlatHashDict, empty, insert, contains, get, remove, clear, capacity, len ]
    imports [ Wyhash, BitMask, Group.{ Group } ]

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

contains : U64FlatHashDict a, U64 -> Bool
contains = \$U64FlatHashDict { data, metadata, seed }, key ->
    False
    # hashKey = Wyhash.hashU64 seed key
    # h1Key = h1 hashKey
    # h2Key = h2 hashKey
    # when probeMD metadata data key h1Key h2Key is 
    #     Found _ _ ->
    #         True
    #     NotFound _ ->
    #         False

len : U64FlatHashDict a -> Nat
len = \$U64FlatHashDict { size } ->
    size

capacity : U64FlatHashDict a -> Nat
capacity = \$U64FlatHashDict { data } ->
    List.len data

remove : U64FlatHashDict a, U64 -> [ T (U64FlatHashDict a) Bool ]
remove = \$U64FlatHashDict { data, metadata, size, default, seed }, key ->
    T ($U64FlatHashDict { data, metadata, size, default, seed }) False
    # hashKey = Wyhash.hashU64 seed key
    # h1Key = h1 hashKey
    # h2Key = h2 hashKey
    # when probeMD metadata data key h1Key h2Key is
    #     Found { slotIndex, offset, loaded } _ ->
    #         T ($U64FlatHashDict
    #             {
    #                 data: data,
    #                 metadata: List.set metadata slotIndex (updateAtOffset loaded offset deletedSlot),
    #                 size: size - 1,
    #                 default,
    #                 seed,
    #             }) True
    #     NotFound _ ->
    #         T ($U64FlatHashDict { data, metadata, size, default, seed }) False

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
    nextSlotIndex = Num.bitwiseAnd (slotIndex + probeI) mask 
    { slotIndex: nextSlotIndex, probeI: probeI + 1, mask }

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
                BitMask.walkUntil h2Match (Err NotFound) (\state, offset ->
                    dataIndex = (Group.mulSize slotIndex) + offset
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
            # not possible. just return not found
            Err NotFound

insert : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insert = \dict, key, value ->
    insertInternal (maybeRehash dict) key value

# Does insertion without potentially rehashing.
insertInternal : U64FlatHashDict a, U64, a -> U64FlatHashDict a
insertInternal = \$U64FlatHashDict { data, metadata, size, default, seed }, key, value ->
    hashKey = Wyhash.hashU64 seed key
    h1Key = Group.h1 hashKey
    h2Key = Group.h2 hashKey
    probe = newProbe h1Key (List.len metadata)
    insertInternalHelper ($U64FlatHashDict { data, metadata, size, default, seed }) probe h2Key key value

insertInternalHelper : U64FlatHashDict a, Probe, Group.H2, U64, a -> U64FlatHashDict a
insertInternalHelper = \$U64FlatHashDict { data, metadata, size, default, seed }, { slotIndex, probeI, mask }, h2Key, key, value ->
    when List.get metadata slotIndex is
        Ok group ->
            h2Match = Group.match group h2Key
            found =
                BitMask.walkUntil h2Match (Err NotFound) (\state, offset ->
                    dataIndex = (Group.mulSize slotIndex) + offset
                    when List.get data dataIndex is
                        Ok (T k v) ->
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
                    dataIndex = (Group.mulSize slotIndex) + offset
                    $U64FlatHashDict
                        {
                            data: List.set data dataIndex (T key value),
                            metadata, # metadata will already be correct if we found the key
                            size: 0 - 1,
                            default,
                            seed,
                        }
                Err NotFound ->
                    emptyMask = Group.matchEmpty group
                    if BitMask.any emptyMask then
                        # Group has empty, insert in the first empty offset.
                        # TODO: this should actually rescan the metadata for the first empty or delete and insert.
                        offset = BitMask.lowestSet emptyMask
                        dataIndex = (Group.mulSize slotIndex) + offset
                        newGroup = Group.updateKeyAtOffset group offset h2Key
                        $U64FlatHashDict
                            {
                                data: List.set data dataIndex (T key value),
                                metadata: List.set metadata slotIndex newGroup,
                                size,
                                default,
                                seed,
                            }
                    else
                        # Group is full, check next group.
                        np = nextProbe { slotIndex, probeI, mask }
                        insertInternalHelper ($U64FlatHashDict { data, metadata, size, default, seed }) np h2Key key value
        Err OutOfBounds ->
            # This should not be possible, maybe panic
            $U64FlatHashDict { data, metadata, size: 0 - 1, default, seed }
            
    # hashKey = Wyhash.hashU64 seed key
    # h1Key = h1 hashKey
    # h2Key = h2 hashKey
    # when probeMD metadata data key h1Key h2Key is
    #     Found { slotIndex, offset } _ ->
    #         dataIndex = (mulGroupSize slotIndex) + offset
    #         $U64FlatHashDict
    #             {
    #                 data: List.set data dataIndex (T key value),
    #                 metadata, # metadata will already be correct if we found the key
    #                 size,
    #                 default,
    #                 seed,
    #             }
    #     NotFound { slotIndex, offset, loaded } ->
    #         dataIndex = (mulGroupSize slotIndex) + offset
    #         $U64FlatHashDict
    #             {
    #                 data: List.set data dataIndex (T key value),
    #                 metadata: List.set metadata slotIndex (updateAtOffset loaded offset h2Key),
    #                 size: size + 1,
    #                 default,
    #                 seed,
    #             }

# ProbeResult a: [ Found Position a, NotFound Position ]

# mul8 = \val -> Num.shiftLeftBy 3 val

# probeMD: List I64, List (Elem a), U64, U64, I8 -> ProbeResult a
# probeMD = \md, data, key, h1Key, h2Key ->
#     slotIndex = Num.bitwiseAnd (Num.toNat h1Key) (List.len md - 1)
#     when List.get md slotIndex is
#         Ok loaded ->
#             probeMDHelper md data key h2Key 0 {loaded, slotIndex, offset: 0} None
#         Err OutOfBounds ->
#             # not possible. just panic
#             NotFound { slotIndex: 0 - 1, offset: 0, loaded: 0 }


# I really hope this giant function inlines.
# I think it will be needed for performance. Otherwise I will have to make like 4 slightly different copies.
# In the NotFound case this will return the first empty or deleted index.
# probeMDHelper: List I64, List (Elem a), U64, I8, Nat, Position, Option Position -> ProbeResult a
# probeMDHelper = \md, data, key, h2Key, probeI, { slotIndex, offset, loaded }, deletedIndex ->
#     if offset < groupSize then
#         # Hopefully the toNat doesn't break anything here. Types are angry.
#         byte = loadAtOffset loaded offset
#         if byte == emptySlot then
#             # No more possible data.
#             # return the first tombstone index.
#             when deletedIndex is
#                 Some pos ->
#                     NotFound pos
#                 None ->
#                     NotFound { slotIndex, offset, loaded }
#         else if byte == h2Key then
#             # We potentially found the element.
#             # Just need to check the key.
#             dataIndex = (mulGroupSize slotIndex) + offset
#             when List.get data dataIndex is
#                 Ok (T k v) ->
#                     if k == key then
#                         # we have a match, return it's index
#                         Found { slotIndex, offset, loaded } v
#                     else
#                         # No match, keep checking.
#                         # indexInsertHelper metadata data h2Key key (index + 1)
#                         probeMDHelper md data key h2Key probeI { slotIndex, offset: (offset + 1), loaded } deletedIndex

#                 Err OutOfBounds ->
#                     # not possible. just panic
#                     NotFound { slotIndex: 0 - 1, offset: 0, loaded: 0 }
#         else if byte == deletedSlot then
#             # Found a deleted item. Store the index because we might insert here
#             # Otherwise, just move on.
#             deletedPos =
#                 when deletedIndex is
#                     Some pos ->
#                         pos
#                     None ->
#                         { slotIndex, offset, loaded }
#             probeMDHelper md data key h2Key probeI { slotIndex, offset: (offset + 1), loaded } (Some deletedPos)
#         else
#             # Just continue, this is a used slot
#             probeMDHelper md data key h2Key probeI { slotIndex, offset: (offset + 1), loaded } deletedIndex
#     else
#         # The offset is too large, which means we need to load the next chunk of metadata
#         newProbeI = probeI + 1
#         newSlotIndex = Num.bitwiseAnd (slotIndex + newProbeI) (List.len md - 1)
#         newOffset = 0
#         when List.get md newSlotIndex is
#             Ok newLoaded ->
#                 probeMDHelper md data key h2Key newProbeI { slotIndex: newSlotIndex, offset: newOffset, loaded: newLoaded } deletedIndex
#             Err OutOfBounds ->
#                 # not possible. just panic
#                 NotFound { slotIndex: 0 - 1, offset: 0, loaded: 0 }


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
                data: List.repeat default Group.size,
                metadata: [Group.allEmpty],
                size,
                default,
                seed,
            }
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
        # rehashHelper newDict metadata data 0
        newDict

# maybeInsertForRehash : U64FlatHashDict a, I8, List (Elem a), Nat -> U64FlatHashDict a
# maybeInsertForRehash = \$U64FlatHashDict { data, metadata, size, default, seed }, oldH2Key, oldData, oldDataIndex ->
#     if oldH2Key >= 0 then
#         when List.get oldData oldDataIndex is
#             Ok (T key value) ->
#                 hashKey = Wyhash.hashU64 seed key
#                 h1Key = h1 hashKey
#                 h2Key = h2 hashKey
#                 when probeMD metadata data key h1Key h2Key is
#                     NotFound { slotIndex, offset, loaded } ->
#                         dataIndex = (mulGroupSize slotIndex) + offset
#                         $U64FlatHashDict
#                             {
#                                 data: List.set data dataIndex (T key value),
#                                 metadata: List.set metadata slotIndex (updateAtOffset loaded offset h2Key),
#                                 size,
#                                 default,
#                                 seed,
#                             }
#                     Found _ _ ->
#                         # Panic this should never happen.
#                         $U64FlatHashDict
#                             {
#                                 data,
#                                 metadata,
#                                 size: 0 - 1,
#                                 default,
#                                 seed,
#                             }

#             Err OutOfBounds ->
#                 # This should be an impossible state since data and metadata are the same size
#                 $U64FlatHashDict
#                     {
#                         data,
#                         metadata,
#                         size: 0 - 1,
#                         default,
#                         seed,
#                     }
#     else
#         $U64FlatHashDict
#             {
#                 data,
#                 metadata,
#                 size,
#                 default,
#                 seed,
#             }


# rehashHelper : U64FlatHashDict a, List I64, List (Elem a), Nat -> U64FlatHashDict a
# rehashHelper = \dict, metadata, data, slotIndex ->
#     when List.get metadata slotIndex is
#         Ok md ->
#             s0 = loadAtOffset md 0
#             s1 = loadAtOffset md 1
#             s2 = loadAtOffset md 2
#             s3 = loadAtOffset md 3
#             s4 = loadAtOffset md 4
#             s5 = loadAtOffset md 5
#             s6 = loadAtOffset md 6
#             s7 = loadAtOffset md 7
#             dataIndex = mulGroupSize slotIndex
#             nextDict0 = maybeInsertForRehash dict s0 data dataIndex
#             nextDict1 = maybeInsertForRehash nextDict0 s1 data (dataIndex + 1)
#             nextDict2 = maybeInsertForRehash nextDict1 s2 data (dataIndex + 2)
#             nextDict3 = maybeInsertForRehash nextDict2 s3 data (dataIndex + 3)
#             nextDict4 = maybeInsertForRehash nextDict3 s4 data (dataIndex + 4)
#             nextDict5 = maybeInsertForRehash nextDict4 s5 data (dataIndex + 5)
#             nextDict6 = maybeInsertForRehash nextDict5 s6 data (dataIndex + 6)
#             nextDict7 = maybeInsertForRehash nextDict6 s7 data (dataIndex + 7)

#             rehashHelper nextDict7 metadata data (slotIndex + 1)

#         Err OutOfBounds ->
#             dict



bitwiseNot : Nat -> Nat
bitwiseNot = \x ->
    Num.bitwiseXor 0xFFFF_FFFF_FFFF_FFFF x

# This is broken. Zf and normal are filled.
shiftRightZfByHack = \by, val ->
    Num.shiftRightBy by val
