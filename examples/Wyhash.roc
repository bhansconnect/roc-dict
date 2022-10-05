interface Wyhash
    exposes [ hashU64, hashBytes, hashBytesStateful, hashBytesStatefulList, createSeed, Seed, rand, combine ]
    imports []

# Note: this is just the version of wyhash for 64 bit numbers.
# https://github.com/wangyi-fudan/wyhash/blob/master/wyhash.h
# Also, apparently on some arm systems 64x64 to 128 is quite slow and wouldn't be wanted.
wyp0 : U64
wyp0 = 0xa0761d6478bd642f
wyp1 : U64
wyp1 = 0xe7037ed1a0b428db
wyp2 : U64
wyp2 = 0x8ebc6af09c88c6e3
wyp3 : U64
wyp3 = 0x589965cc75374cc3

wymum : U64, U64 -> [ T U64 U64 ]
wymum = \a, b ->
    r = Num.toU128 a * Num.toU128 b
    lowerR = Num.bitwiseAnd r 0xFFFF_FFFF_FFFF_FFFF
    upperR = Num.shiftRightZfBy r 64

    # This is the more robust form.
    # T (Num.bitwiseXor a (Num.toU64 lowerR)) (Num.bitwiseXor b (Num.toU64 upperR))
    T (Num.toU64 lowerR) (Num.toU64 upperR)

wymix : U64, U64 -> U64
wymix = \a, b ->
    when wymum a b is
        T x y ->
            Num.bitwiseXor x y

combine : U64, U64 -> U64
combine = \a, b ->
    when wymum (Num.bitwiseXor a wyp0) (Num.bitwiseXor b wyp1) is
        T x y ->
            wymix (Num.bitwiseXor x wyp0) (Num.bitwiseXor y wyp1)

Seed := U64

createSeed : U64 -> Seed
createSeed = \seed -> @Seed seed

rand : Seed -> [ T Seed U64 ]
rand = \@Seed seed ->
    nextSeed = Num.addWrap seed wyp0

    T (@Seed nextSeed) (wymix seed (Num.bitwiseXor seed wyp1))

# The actual wyhash function is made to operate on raw bytes.
# Instead I am directly implementing it for specific types.
hashU64 : Seed, U64 -> U64
hashU64 = \@Seed seed, key ->
    # seed^=*secret;
    # a=(_wyr4(p)<<32)|_wyr4(p+4); b=(_wyr4(p+4)<<32)|_wyr4(p); }
    # return _wymix(secret[1]^len,_wymix(a^secret[1],b^seed));
    p1 = Num.bitwiseAnd 0xFFFF_FFFF key
    p2 = Num.shiftRightZfBy key 32
    a = Num.bitwiseOr (Num.shiftLeftBy p1 32) p2
    b = Num.bitwiseOr (Num.shiftLeftBy p2 32) p1

    wymix (Num.bitwiseXor wyp1 8) (wymix (Num.bitwiseXor wyp1 a) (Num.bitwiseXor (Num.bitwiseXor seed wyp0) b))
    # 0

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
wyr8 : List U8, Nat -> U64
wyr8 = \list, index ->
    p1 = Num.toU64 (getByte list index)
    p2 = Num.toU64 (getByte list (Num.addWrap index 1))
    p3 = Num.toU64 (getByte list (Num.addWrap index 2))
    p4 = Num.toU64 (getByte list (Num.addWrap index 3))
    p5 = Num.toU64 (getByte list (Num.addWrap index 4))
    p6 = Num.toU64 (getByte list (Num.addWrap index 5))
    p7 = Num.toU64 (getByte list (Num.addWrap index 6))
    p8 = Num.toU64 (getByte list (Num.addWrap index 7))
    a = Num.bitwiseOr p1 (Num.shiftLeftBy p2 8)
    b = Num.bitwiseOr (Num.shiftLeftBy p3 16) (Num.shiftLeftBy p4 24)
    c = Num.bitwiseOr (Num.shiftLeftBy p5 32) (Num.shiftLeftBy p6 40)
    d = Num.bitwiseOr (Num.shiftLeftBy p7 48) (Num.shiftLeftBy p8 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

# Get the next 4 bytes as a U64
wyr4 : List U8, Nat -> U64
wyr4 = \list, index ->
    p1 = Num.toU64 (getByte list index)
    p2 = Num.toU64 (getByte list (Num.addWrap index 1))
    p3 = Num.toU64 (getByte list (Num.addWrap index 2))
    p4 = Num.toU64 (getByte list (Num.addWrap index 3))
    a = Num.bitwiseOr p1 (Num.shiftLeftBy p2 8)
    b = Num.bitwiseOr (Num.shiftLeftBy p3 16) (Num.shiftLeftBy p4 24)

    Num.bitwiseOr a b

# Get the next K bytes with some shifting.
# K must be 3 or less.
wyr3 : List U8, Nat, Nat -> U64
wyr3 = \list, index, k ->
    # ((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1]
    p1 = Num.toU64 (getByte list index)
    p2 = Num.toU64 (getByte list (Num.shiftRightZfBy k 1 |> Num.addWrap index)) 
    p3 = Num.toU64 (getByte list (Num.subWrap k 1 |> Num.addWrap index))
    a = Num.bitwiseOr (Num.shiftLeftBy p1 16) (Num.shiftLeftBy p2 8)

    Num.bitwiseOr a p3

hashBytes : Seed, List U8 -> U64
hashBytes = \@Seed oldSeed, list ->
    len = List.len list
    seed = Num.bitwiseXor oldSeed wyp0
    abs =
        if len <= 16 then
            if len >= 4 then
                x = Num.shiftLeftBy (Num.shiftRightZfBy len 3) 2
                a = Num.bitwiseOr (Num.shiftLeftBy (wyr4 list 0) 32) (wyr4 list x)
                b = Num.bitwiseOr (Num.shiftLeftBy (wyr4 list (Num.subWrap len 4)) 32) (wyr4 list (Num.subWrap len 4 |> Num.subWrap x))

                { a, b, seed }
            else if len > 0 then
                { a: wyr3 list 0 len, b: 0, seed }
            else
                { a: 0, b: 0, seed }
        else if len <= 48 then
            hashBytesHelper16 seed list 0 len
        else
            hashBytesHelper48 seed seed seed list 0 len

    wymix (Num.bitwiseXor wyp1 (Num.toU64 len)) (wymix (Num.bitwiseXor wyp1 abs.a) (Num.bitwiseXor abs.seed abs.b))

hashBytesHelper48 : U64, U64, U64, List U8, Nat, Nat -> { a : U64, b : U64, seed : U64 }
hashBytesHelper48 = \seed, see1, see2, list, index, remaining ->
    newSeed = wymix (Num.bitwiseXor (wyr8 list index) wyp1) (Num.bitwiseXor (wyr8 list (Num.addWrap index 8)) seed)
    newSee1 = wymix (Num.bitwiseXor (wyr8 list (Num.addWrap index 16)) wyp2) (Num.bitwiseXor (wyr8 list (Num.addWrap index 24)) see1)
    newSee2 = wymix (Num.bitwiseXor (wyr8 list (Num.addWrap index 32)) wyp3) (Num.bitwiseXor (wyr8 list (Num.addWrap index 40)) see2)
    newRemaining = Num.subWrap remaining 48
    newIndex = Num.addWrap index 48

    if newRemaining > 48 then
        hashBytesHelper48 newSeed newSee1 newSee2 list newIndex newRemaining
    else if newRemaining > 16 then
        finalSeed = Num.bitwiseXor newSee2 (Num.bitwiseXor newSee1 newSeed)

        hashBytesHelper16 finalSeed list newIndex newRemaining
    else
        finalSeed = Num.bitwiseXor newSee2 (Num.bitwiseXor newSee1 newSeed)

        { a: wyr8 list (Num.subWrap newRemaining 16 |> Num.addWrap newIndex), b: wyr8 list (Num.subWrap newRemaining 8|> Num.addWrap newIndex), seed: finalSeed }

hashBytesHelper16 : U64, List U8, Nat, Nat -> { a : U64, b : U64, seed : U64 }
hashBytesHelper16 = \seed, list, index, remaining ->
    newSeed = wymix (Num.bitwiseXor (wyr8 list index) wyp1) (Num.bitwiseXor (wyr8 list (Num.addWrap index 8)) seed)
    newRemaining = Num.subWrap remaining 16
    newIndex = Num.addWrap index 16

    if newRemaining <= 16 then
        { a: wyr8 list (Num.subWrap newRemaining 16 |> Num.addWrap newIndex), b: wyr8 list (Num.subWrap newRemaining 8 |> Num.addWrap newIndex), seed: newSeed }
    else
        hashBytesHelper16 newSeed list newIndex newRemaining

#   seed^=*secret;
#   if(_likely_(len<=16)){
#     if(_likely_(len>=4)){ a=(_wyr4(p)<<32)|_wyr4(p+((len>>3)<<2)); b=(_wyr4(p+len-4)<<32)|_wyr4(p+len-4-((len>>3)<<2)); }
#     else if(_likely_(len>0)){ a=_wyr3(p,len); b=0;}
#     else a=b=0;
#   }
#   else{
#     size_t i=len;
#     if(_unlikely_(i>48)){
#       uint64_t see1=seed, see2=seed;
#       do{
#         seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);
#         see1=_wymix(_wyr8(p+16)^secret[2],_wyr8(p+24)^see1);
#         see2=_wymix(_wyr8(p+32)^secret[3],_wyr8(p+40)^see2);
#         p+=48; i-=48;
#       }while(_likely_(i>48));
#       seed^=see1^see2;
#     }
#     while(_unlikely_(i>16)){  seed=_wymix(_wyr8(p)^secret[1],_wyr8(p+8)^seed);  i-=16; p+=16;  }
#     a=_wyr8(p+i-16);  b=_wyr8(p+i-8);
#   }
#   return _wymix(secret[1]^len,_wymix(a^secret[1],b^seed));

# It would be nice to use a list of U8 for the state, but an allocation and pointer chasing is too expensive.
# Would really love an array type.
# Instead...just gonna do it adhoc I guess....

HashBuffer : {
    # Basically an array of 48 bytes.
    a00 : U8,
    a01 : U8,
    a02 : U8,
    a03 : U8,
    a04 : U8,
    a05 : U8,
    a06 : U8,
    a07 : U8,
    a08 : U8,
    a09 : U8,
    a10 : U8,
    a11 : U8,
    a12 : U8,
    a13 : U8,
    a14 : U8,
    a15 : U8,
    a16 : U8,
    a17 : U8,
    a18 : U8,
    a19 : U8,
    a20 : U8,
    a21 : U8,
    a22 : U8,
    a23 : U8,
    a24 : U8,
    a25 : U8,
    a26 : U8,
    a27 : U8,
    a28 : U8,
    a29 : U8,
    a30 : U8,
    a31 : U8,
    a32 : U8,
    a33 : U8,
    a34 : U8,
    a35 : U8,
    a36 : U8,
    a37 : U8,
    a38 : U8,
    a39 : U8,
    a40 : U8,
    a41 : U8,
    a42 : U8,
    a43 : U8,
    a44 : U8,
    a45 : U8,
    a46 : U8,
    a47 : U8,
    # We need to keep the last 16 bytes to complete correctly.
    b00 : U8,
    b01 : U8,
    b02 : U8,
    b03 : U8,
    b04 : U8,
    b05 : U8,
    b06 : U8,
    b07 : U8,
    b08 : U8,
    b09 : U8,
    b10 : U8,
    b11 : U8,
    b12 : U8,
    b13 : U8,
    b14 : U8,
    b15 : U8,
    len : U8,
}

setHashBuffer : HashBuffer, U8, U8 -> HashBuffer
setHashBuffer = \buf, index, val ->
    when index is
        0 -> {buf & a00: val}
        1 -> {buf & a01: val}
        2 -> {buf & a02: val}
        3 -> {buf & a03: val}
        4 -> {buf & a04: val}
        5 -> {buf & a05: val}
        6 -> {buf & a06: val}
        7 -> {buf & a07: val}
        8 -> {buf & a08: val}
        9 -> {buf & a09: val}
        10 -> {buf & a10: val}
        11 -> {buf & a11: val}
        12 -> {buf & a12: val}
        13 -> {buf & a13: val}
        14 -> {buf & a14: val}
        15 -> {buf & a15: val}
        16 -> {buf & a16: val}
        17 -> {buf & a17: val}
        18 -> {buf & a18: val}
        19 -> {buf & a19: val}
        20 -> {buf & a20: val}
        21 -> {buf & a21: val}
        22 -> {buf & a22: val}
        23 -> {buf & a23: val}
        24 -> {buf & a24: val}
        25 -> {buf & a25: val}
        26 -> {buf & a26: val}
        27 -> {buf & a27: val}
        28 -> {buf & a28: val}
        29 -> {buf & a29: val}
        30 -> {buf & a30: val}
        31 -> {buf & a31: val}
        32 -> {buf & a32: val}
        33 -> {buf & a33: val}
        34 -> {buf & a34: val}
        35 -> {buf & a35: val}
        36 -> {buf & a36: val}
        37 -> {buf & a37: val}
        38 -> {buf & a38: val}
        39 -> {buf & a39: val}
        40 -> {buf & a40: val}
        41 -> {buf & a41: val}
        42 -> {buf & a42: val}
        43 -> {buf & a43: val}
        44 -> {buf & a44: val}
        45 -> {buf & a45: val}
        46 -> {buf & a46: val}
        47 -> {buf & a47: val}
        _ -> {buf & a47: 0 - 1}

emptyHashBuffer : HashBuffer
emptyHashBuffer = {
    a00: 0,
    a01: 0,
    a02: 0,
    a03: 0,
    a04: 0,
    a05: 0,
    a06: 0,
    a07: 0,
    a08: 0,
    a09: 0,
    a10: 0,
    a11: 0,
    a12: 0,
    a13: 0,
    a14: 0,
    a15: 0,
    a16: 0,
    a17: 0,
    a18: 0,
    a19: 0,
    a20: 0,
    a21: 0,
    a22: 0,
    a23: 0,
    a24: 0,
    a25: 0,
    a26: 0,
    a27: 0,
    a28: 0,
    a29: 0,
    a30: 0,
    a31: 0,
    a32: 0,
    a33: 0,
    a34: 0,
    a35: 0,
    a36: 0,
    a37: 0,
    a38: 0,
    a39: 0,
    a40: 0,
    a41: 0,
    a42: 0,
    a43: 0,
    a44: 0,
    a45: 0,
    a46: 0,
    a47: 0,
    b00: 0,
    b01: 0,
    b02: 0,
    b03: 0,
    b04: 0,
    b05: 0,
    b06: 0,
    b07: 0,
    b08: 0,
    b09: 0,
    b10: 0,
    b11: 0,
    b12: 0,
    b13: 0,
    b14: 0,
    b15: 0,
    len: 0,
}

clearHashBuffer: HashBuffer -> HashBuffer
clearHashBuffer = \buf ->
    {buf &
        len: 0,
        b00: buf.a32,
        b01: buf.a33,
        b02: buf.a34,
        b03: buf.a35,
        b04: buf.a36,
        b05: buf.a37,
        b06: buf.a38,
        b07: buf.a39,
        b08: buf.a40,
        b09: buf.a41,
        b10: buf.a42,
        b11: buf.a43,
        b12: buf.a44,
        b13: buf.a45,
        b14: buf.a46,
        b15: buf.a47,
    }

# firstBackupU64HashBuffer : HashBuffer -> U64 
# firstBackupU64HashBuffer = \buf ->
#     b00 = Num.toU64 buf.b00
#     b01 = Num.toU64 buf.b01
#     b02 = Num.toU64 buf.b02
#     b03 = Num.toU64 buf.b03
#     b04 = Num.toU64 buf.b04
#     b05 = Num.toU64 buf.b05
#     b06 = Num.toU64 buf.b06
#     b07 = Num.toU64 buf.b07
#     a = Num.bitwiseOr b00 (Num.shiftLeftBy b01 8)
#     b = Num.bitwiseOr (Num.shiftLeftBy b02 16) (Num.shiftLeftBy b03 24)
#     c = Num.bitwiseOr (Num.shiftLeftBy b04 32) (Num.shiftLeftBy b05 40)
#     d = Num.bitwiseOr (Num.shiftLeftBy b06 48) (Num.shiftLeftBy b07 56)

#     Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

# secondBackupU64HashBuffer : HashBuffer -> U64 
# secondBackupU64HashBuffer = \buf ->
#     b08 = Num.toU64 buf.b08
#     b09 = Num.toU64 buf.b09
#     b10 = Num.toU64 buf.b10
#     b11 = Num.toU64 buf.b11
#     b12 = Num.toU64 buf.b12
#     b13 = Num.toU64 buf.b13
#     b14 = Num.toU64 buf.b14
#     b15 = Num.toU64 buf.b15
#     a = Num.bitwiseOr b08 (Num.shiftLeftBy b09 8)
#     b = Num.bitwiseOr (Num.shiftLeftBy b10 16) (Num.shiftLeftBy b11 24)
#     c = Num.bitwiseOr (Num.shiftLeftBy b12 32) (Num.shiftLeftBy b13 40)
#     d = Num.bitwiseOr (Num.shiftLeftBy b14 48) (Num.shiftLeftBy b15 56)

#     Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

firstU64HashBuffer : HashBuffer -> U64 
firstU64HashBuffer = \buf ->
    a00 = Num.toU64 buf.a00
    a01 = Num.toU64 buf.a01
    a02 = Num.toU64 buf.a02
    a03 = Num.toU64 buf.a03
    a04 = Num.toU64 buf.a04
    a05 = Num.toU64 buf.a05
    a06 = Num.toU64 buf.a06
    a07 = Num.toU64 buf.a07
    a = Num.bitwiseOr a00 (Num.shiftLeftBy a01 8)
    b = Num.bitwiseOr (Num.shiftLeftBy a02 16) (Num.shiftLeftBy a03 24)
    c = Num.bitwiseOr (Num.shiftLeftBy a04 32) (Num.shiftLeftBy a05 40)
    d = Num.bitwiseOr (Num.shiftLeftBy a06 48) (Num.shiftLeftBy a07 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

secondU64HashBuffer : HashBuffer -> U64 
secondU64HashBuffer = \buf ->
    a08 = Num.toU64 buf.a08
    a09 = Num.toU64 buf.a09
    a10 = Num.toU64 buf.a10
    a11 = Num.toU64 buf.a11
    a12 = Num.toU64 buf.a12
    a13 = Num.toU64 buf.a13
    a14 = Num.toU64 buf.a14
    a15 = Num.toU64 buf.a15
    a = Num.bitwiseOr a08 (Num.shiftLeftBy a09 8)
    b = Num.bitwiseOr (Num.shiftLeftBy a10 16) (Num.shiftLeftBy a11 24)
    c = Num.bitwiseOr (Num.shiftLeftBy a12 32) (Num.shiftLeftBy a13 40)
    d = Num.bitwiseOr (Num.shiftLeftBy a14 48) (Num.shiftLeftBy a15 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

thirdU64HashBuffer : HashBuffer -> U64 
thirdU64HashBuffer = \buf ->
    a16 = Num.toU64 buf.a16
    a17 = Num.toU64 buf.a17
    a18 = Num.toU64 buf.a18
    a19 = Num.toU64 buf.a19
    a20 = Num.toU64 buf.a20
    a21 = Num.toU64 buf.a21
    a22 = Num.toU64 buf.a22
    a23 = Num.toU64 buf.a23
    a = Num.bitwiseOr a16 (Num.shiftLeftBy a17 8)
    b = Num.bitwiseOr (Num.shiftLeftBy a18 16) (Num.shiftLeftBy a19 24)
    c = Num.bitwiseOr (Num.shiftLeftBy a20 32) (Num.shiftLeftBy a21 40)
    d = Num.bitwiseOr (Num.shiftLeftBy a22 48) (Num.shiftLeftBy a23 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

fourthU64HashBuffer : HashBuffer -> U64 
fourthU64HashBuffer = \buf ->
    a24 = Num.toU64 buf.a24
    a25 = Num.toU64 buf.a25
    a26 = Num.toU64 buf.a26
    a27 = Num.toU64 buf.a27
    a28 = Num.toU64 buf.a28
    a29 = Num.toU64 buf.a29
    a30 = Num.toU64 buf.a30
    a31 = Num.toU64 buf.a31
    a = Num.bitwiseOr a24 (Num.shiftLeftBy a25 8)
    b = Num.bitwiseOr (Num.shiftLeftBy a26 16) (Num.shiftLeftBy a27 24)
    c = Num.bitwiseOr (Num.shiftLeftBy a28 32) (Num.shiftLeftBy a29 40)
    d = Num.bitwiseOr (Num.shiftLeftBy a30 48) (Num.shiftLeftBy a31 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

fifthU64HashBuffer : HashBuffer -> U64 
fifthU64HashBuffer = \buf ->
    a32 = Num.toU64 buf.a32
    a33 = Num.toU64 buf.a33
    a34 = Num.toU64 buf.a34
    a35 = Num.toU64 buf.a35
    a36 = Num.toU64 buf.a36
    a37 = Num.toU64 buf.a37
    a38 = Num.toU64 buf.a38
    a39 = Num.toU64 buf.a39
    a = Num.bitwiseOr a32 (Num.shiftLeftBy a33 8)
    b = Num.bitwiseOr (Num.shiftLeftBy a34 16) (Num.shiftLeftBy a35 24)
    c = Num.bitwiseOr (Num.shiftLeftBy a36 32) (Num.shiftLeftBy a37 40)
    d = Num.bitwiseOr (Num.shiftLeftBy a38 48) (Num.shiftLeftBy a39 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

sixthU64HashBuffer : HashBuffer -> U64 
sixthU64HashBuffer = \buf ->
    a40 = Num.toU64 buf.a40
    a41 = Num.toU64 buf.a41
    a42 = Num.toU64 buf.a42
    a43 = Num.toU64 buf.a43
    a44 = Num.toU64 buf.a44
    a45 = Num.toU64 buf.a45
    a46 = Num.toU64 buf.a46
    a47 = Num.toU64 buf.a47
    a = Num.bitwiseOr a40 (Num.shiftLeftBy a41 8)
    b = Num.bitwiseOr (Num.shiftLeftBy a42 16) (Num.shiftLeftBy a43 24)
    c = Num.bitwiseOr (Num.shiftLeftBy a44 32) (Num.shiftLeftBy a45 40)
    d = Num.bitwiseOr (Num.shiftLeftBy a46 48) (Num.shiftLeftBy a47 56)

    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

# If we really wanted to give the best try of making this performant,
# we should use a ring buffer with 64 elements.
# Everytime we hit 49 elements, we hash the first 48.
# Due to the ring, we can avoid copying bytes.
State : { seed: U64, see1: U64, see2: U64, buf: HashBuffer, totalLen: U64 }

hashBytesStateful : Seed, List U8 -> U64
hashBytesStateful = \@Seed oldSeed, list ->
    # we are gonna pretend we don't know how many bytes we have and instead walk these bytes, build up state, and hash that way.
    seed = Num.bitwiseXor oldSeed wyp0
    # even though we hash in chunks of 48, we need to potentially keep around an extra 16 bytes for finalizing the algorithm.
    state = {seed, see1: seed, see2: seed, buf: emptyHashBuffer, totalLen: 0}
    List.walk list state hashByte
    |> complete

hashByte : State, U8 -> State
hashByte = \{seed, see1, see2, buf, totalLen}, byte ->
    # # by default, just collect until we have 48 bytes to hash.
    nextLen = Num.addWrap buf.len 1
    nextBuf =
        tmp = setHashBuffer buf buf.len byte
        {tmp & len: nextLen}
    if nextLen != 48 then
       {seed, see1, see2, buf: nextBuf, totalLen: Num.addWrap totalLen 1} 
    else
        # we are at max size, hash 48 bytes.
        # Note: the first 16 are old bytes for final state cleanup, so skip them.
        newSeed = wymix (Num.bitwiseXor (firstU64HashBuffer nextBuf) wyp1) (Num.bitwiseXor (secondU64HashBuffer nextBuf) seed)
        newSee1 = wymix (Num.bitwiseXor (thirdU64HashBuffer nextBuf) wyp2) (Num.bitwiseXor (fourthU64HashBuffer nextBuf) see1)
        newSee2 = wymix (Num.bitwiseXor (fifthU64HashBuffer nextBuf) wyp3) (Num.bitwiseXor (sixthU64HashBuffer nextBuf) see2)
        {seed: newSeed, see1: newSee1, see2: newSee2, buf: clearHashBuffer nextBuf, totalLen: Num.addWrap totalLen 1}


complete : State -> U64
complete = \{seed, see1, see2, buf, totalLen} ->
    len = buf.len
    abs : { a: U64, b: U64, seed: U64 }
    abs =
        if totalLen < 48 then
            # ignoring all of these cases for now, just panic.
            { a: 0, b: 0 - 1, seed }
        else if len != 16 then
            # Also, ignoring this case to make base test simple.
            { a: 0, b: 0 - 1, seed }
        else
            # This should be correct for the 0 case but not others.
            finalSeed =
                seed
                |> Num.bitwiseXor see1
                |> Num.bitwiseXor see2

            { a: firstU64HashBuffer buf, b: secondU64HashBuffer buf, seed: finalSeed }
    
    wymix (Num.bitwiseXor wyp1 totalLen) (wymix (Num.bitwiseXor wyp1 abs.a) (Num.bitwiseXor abs.seed abs.b))

# wyr3AtZeroHashBuffer : HashBuffer -> U64
# wyr3AtZeroHashBuffer = \{a00, a01, a02, len} ->
#     # ((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1]
#     p0 = a00
#     {p1, p2} =
#         when len is
#             1 -> {p1: a00, p2: a00}
#             2 -> {p1: a01, p2: a01}
#             3 -> {p1: a01, p2: a02}
#             _ -> {p1: 0 - 1, p2: 0 - 1}
#     p0U64 = Num.toU64 p0
#     p1U64 = Num.toU64 p1 
#     p2U64 = Num.toU64 p2
#     a = Num.bitwiseOr (Num.shiftLeftBy p0U64 16) (Num.shiftLeftBy p1U64 8)

#     Num.bitwiseOr a p2U64

StateList : { seed: U64, see1: U64, see2: U64, buf: List U8, totalLen: U64 }

hashBytesStatefulList : Seed, List U8 -> U64
hashBytesStatefulList = \@Seed oldSeed, list ->
    # we are gonna pretend we don't know how many bytes we have and instead walk these bytes, build up state, and hash that way.
    seed = Num.bitwiseXor oldSeed wyp0
    buf =
        List.withCapacity 64
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
        |> List.append 0
    # even though we hash in chunks of 48, we need to potentially keep around an extra 16 bytes for finalizing the algorithm.
    state = {seed, see1: seed, see2: seed, buf, totalLen: 0}
    List.walk list state hashByteList
    |> completeList

hashByteList : StateList, U8 -> StateList
hashByteList = \{seed, see1, see2, buf, totalLen}, byte ->
    # by default, just collect until we have 48 bytes to hash.
    # That is 64 elements because we have to keep an extra 16
    nextBuf = List.append buf byte
    if List.len nextBuf != 64 then
       {seed, see1, see2, buf: nextBuf, totalLen: Num.addWrap totalLen 1} 
    else
        # we are at max size, hash 48 bytes.
        # Note: the first 16 are old bytes for final state cleanup, so skip them.
        newSeed = wymix (Num.bitwiseXor (wyr8 nextBuf 16) wyp1) (Num.bitwiseXor (wyr8 nextBuf 24) seed)
        newSee1 = wymix (Num.bitwiseXor (wyr8 nextBuf 32) wyp2) (Num.bitwiseXor (wyr8 nextBuf 40) see1)
        newSee2 = wymix (Num.bitwiseXor (wyr8 nextBuf 48) wyp3) (Num.bitwiseXor (wyr8 nextBuf 56) see2)
        {seed: newSeed, see1: newSee1, see2: newSee2, buf: List.takeLast nextBuf 16, totalLen: Num.addWrap totalLen 1}


completeList : StateList -> U64
completeList = \{seed, see1, see2, buf, totalLen} ->
    len = List.len buf |> Num.subWrap 16
    abs : { a: U64, b: U64, seed: U64 }
    abs =
        if totalLen < 48 then
            # ignoring all of these cases for now, just panic.
            { a: 0, b: 0 - 1, seed }
        else if len != 16 then
            # Also, ignoring this case to make base test simple.
            { a: 0, b: 0 - 1, seed }
        else
            # This should be correct for the 0 case but not others.
            finalSeed =
                seed
                |> Num.bitwiseXor see1
                |> Num.bitwiseXor see2

            { a: wyr8 buf 16, b: wyr8 buf 24, seed: finalSeed }
    
    wymix (Num.bitwiseXor wyp1 totalLen) (wymix (Num.bitwiseXor wyp1 abs.a) (Num.bitwiseXor abs.seed abs.b))
