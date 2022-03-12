interface AbslHash
    exposes [ hashU64, hashBytes, createSeed, Seed, rand, combine ]
    imports []

# AbslHash is a modified form of wyhash that is theoretically faster.
# It cuts some corners and uses a larger group size
# https://raw.githubusercontent.com/abseil/abseil-cpp/master/absl/hash/internal/low_level_hash.cc
salt0 : U64
salt0 = 0x243F6A8885A308D3
salt1 : U64
salt1 = 0x13198A2E03707344
salt2 : U64
salt2 = 0xA4093822299F31D0
salt3 : U64
salt3 = 0x082EFA98EC4E6C89
salt4 : U64
salt4 = 0x452821E638D01377

shiftRightZfByHack = \by, val ->
    Num.shiftRightBy by val

mix : U64, U64 -> U64
mix = \a, b ->
    r = Num.toU128 a * Num.toU128 b
    lowerR = Num.bitwiseAnd r 0xFFFF_FFFF_FFFF_FFFF
    upperR = shiftRightZfByHack 64 r

    Num.bitwiseXor (Num.toU64 lowerR) (Num.toU64 upperR)

combine : U64, U64 -> U64
combine = \a, b ->
    mix (Num.bitwiseXor a salt0) (Num.bitwiseXor b salt1)

Seed := U64

createSeed : U64 -> Seed
createSeed = \seed -> $Seed seed

rand : Seed -> [ T Seed U64 ]
rand = \$Seed seed ->
    nextSeed = Num.addWrap seed salt0

    T ($Seed nextSeed) (mix seed (Num.bitwiseXor seed salt1))

# We are always accessing within length, so we use a special version of List.Get
getByte : List U8, Nat -> U8
getByte = \list, index ->
    when List.get list index is
        Ok b ->
            b

        # Panic on out of range access.
        _ ->
            0 - 1

# Get the next 8 bytes as a U64
loadU64At : List U8, Nat -> U64
loadU64At = \list, index ->
    # These are dummies until roc can load a U64 from a list of bytes properly.
    # This should take about the same amount of time to run.
    Num.toU64 (getByte list index)

# Get the next 4 bytes as a U64
loadU32At : List U8, Nat -> U64
loadU32At = \list, index ->
    # These are dummies until roc can load a U64 from a list of bytes properly.
    # This should take about the same amount of time to run.
    Num.toU64 (getByte list index)

# Direct implementation of what for happen with hashBytes but for U64
hashU64 : Seed, U64 -> U64
hashU64 = \$Seed seed, key ->
    state0 = Num.bitwiseXor seed salt0
    a = Num.bitwiseAnd key 0xFFFF_FFFF
    b = shiftRightZfByHack 32 key
    w = mix (Num.bitwiseXor a salt1) (Num.bitwiseXor b state0)
    z = Num.bitwiseXor salt1 8

    mix w z

hashBytes : Seed, List U8 -> U64
hashBytes = \$Seed seed, list ->
    startingLen = List.len list
    remaining0 = startingLen
    index0 = 0
    state0 = Num.bitwiseXor seed salt0

    s1 =
        if remaining0 > 64 then
            hashBytesHelper64 state0 state0 list index0 remaining0
        else
            { state: state0, index: index0, remaining: remaining0 }

    s2 =
        hashBytesHelper16 s1.state list s1.index s1.remaining
    state2 = s2.state
    index2 = s2.index
    remaining2 = s2.remaining

    ab =
        if remaining2 > 8 then
            { a: loadU64At list index2, b: loadU64At list (index2 + remaining2 - 8) }
        else if remaining2 > 4 then
            { a: loadU32At list index2, b: loadU32At list (index2 + remaining2 - 4) }
        else if remaining2 > 0 then
            p1 = Num.toU64 (getByte list index2)
            p2 = Num.toU64 (getByte list (index2 + shiftRightZfByHack 1 remaining2))
            p3 = Num.toU64 (getByte list (index2 + remaining2 - 1))
            a = Num.bitwiseOr p3 (Num.bitwiseOr (Num.shiftLeftBy 16 p1) (Num.shiftLeftBy 8 p2))

            { a, b: 0 }
        else
            { a: 0, b: 0 }
    w = mix (Num.bitwiseXor ab.a salt1) (Num.bitwiseXor ab.b state2)
    z = Num.bitwiseXor salt1 (Num.toU64 startingLen)

    mix w z

hashBytesHelper64 : U64, U64, List U8, Nat, Nat -> { state : U64, index : Nat, remaining : Nat }
hashBytesHelper64 = \ds, cs, list, index, remaining ->
    a = loadU64At list index
    b = loadU64At list (index + 8)
    c = loadU64At list (index + 16)
    d = loadU64At list (index + 24)
    e = loadU64At list (index + 32)
    f = loadU64At list (index + 40)
    g = loadU64At list (index + 48)
    h = loadU64At list (index + 56)

    cs0 = mix (Num.bitwiseXor a salt1) (Num.bitwiseXor b cs)
    cs1 = mix (Num.bitwiseXor c salt2) (Num.bitwiseXor d cs)
    nextCS = Num.bitwiseXor cs0 cs1

    ds0 = mix (Num.bitwiseXor e salt3) (Num.bitwiseXor f ds)
    ds1 = mix (Num.bitwiseXor g salt4) (Num.bitwiseXor h ds)
    nextDS = Num.bitwiseXor ds0 ds1

    nextIndex = index + 64
    nextRemaining = remaining - 64

    if remaining > 64 then
        hashBytesHelper64 nextDS nextCS list nextIndex nextRemaining
    else
        { state: Num.bitwiseXor nextCS nextDS, index: nextIndex, remaining: nextRemaining }

hashBytesHelper16 : U64, List U8, Nat, Nat -> { state : U64, index : Nat, remaining : Nat }
hashBytesHelper16 = \state, list, index, remaining ->
    if remaining > 16 then
        a = loadU64At list index
        b = loadU64At list (index + 8)
        nextState = mix (Num.bitwiseXor a salt1) (Num.bitwiseXor b state)

        hashBytesHelper16 nextState list (index + 16) (remaining - 16)
    else
        { state, index, remaining }
