interface Wyhash
    exposes [ hashU64, hashBytes, createSeed, Seed, rand, combine ]
    imports []

# Note: this is just the version of wyhash for 64 bit numbers.
# https://github.com/wangyi-fudan/wyhash/blob/master/wyhash.h
# Also, apparently on some arm systems 64x64 to 128 is quite slow and wouldn't be wanted.
wyp0 : U64
wyp0 = 0xa0761d6478bd642f
wyp1 : U64
wyp1 = 0xe7037ed1a0b428db
# wyp2 : U64
# wyp2 = 0x8ebc6af09c88c6e3
# wyp3 : U64
# wyp3 = 0x589965cc75374cc3

# Remove these once related roc bugs are fixed.
toU128Hack = \val ->
    Num.bitwiseAnd 0xFFFF_FFFF_FFFF_FFFF (Num.toU128 val)

shiftRightZfByHack = \by, val ->
    Num.shiftRightBy by val

wymum : U64, U64 -> [ T U64 U64 ]
wymum = \a, b ->
    r = (toU128Hack a) * (toU128Hack b)
    lowerR = Num.bitwiseAnd r 0xFFFF_FFFF_FFFF_FFFF
    upperR = shiftRightZfByHack 64 r

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
createSeed = \seed -> $Seed seed

rand : Seed -> [ T Seed U64 ]
rand = \$Seed seed ->
    nextSeed = Num.addWrap seed wyp0

    T ($Seed nextSeed) (wymix seed (Num.bitwiseXor seed wyp1))

# The actual wyhash function is made to operate on raw bytes.
# Instead I am directly implementing it for specific types.
hashU64 : Seed, U64 -> U64
hashU64 = \$Seed seed, key ->
    # seed^=*secret;
    # a=(_wyr4(p)<<32)|_wyr4(p+4); b=(_wyr4(p+4)<<32)|_wyr4(p); }
    # return _wymix(secret[1]^len,_wymix(a^secret[1],b^seed));
    p1 = Num.bitwiseAnd 0xFFFF_FFFF key
    p2 = shiftRightZfByHack 32 key
    a = Num.bitwiseOr (Num.shiftLeftBy 32 p1) p2
    b = Num.bitwiseOr (Num.shiftLeftBy 32 p2) p1

    wymix (Num.bitwiseXor wyp1 8) (wymix (Num.bitwiseXor wyp1 a) (Num.bitwiseXor (Num.bitwiseXor seed wyp0) b))

# We are always accessing within length, so we use a special version of List.Get
getByte : List U8, Nat -> U8
getByte = \list, index ->
    when List.get list index is
        Ok b -> b
        # Panic on out of range access.
        _ -> 0 - 1

# Get the next 8 bytes as a U64
wyr8 : List U8, Nat -> U64
wyr8 = \list, index ->
    p1 = Num.toU64 (getByte list index)
    p2 = Num.toU64 (getByte list (index + 1))
    p3 = Num.toU64 (getByte list (index + 2))
    p4 = Num.toU64 (getByte list (index + 3))
    p5 = Num.toU64 (getByte list (index + 4))
    p6 = Num.toU64 (getByte list (index + 5))
    p7 = Num.toU64 (getByte list (index + 6))
    p8 = Num.toU64 (getByte list (index + 7))
    a = Num.bitwiseOr p1 (Num.shiftLeftBy 8 p2)
    b = Num.bitwiseOr (Num.shiftLeftBy 16 p3) (Num.shiftLeftBy 24 p4)
    c = Num.bitwiseOr (Num.shiftLeftBy 32 p5) (Num.shiftLeftBy 40 p6)
    d = Num.bitwiseOr (Num.shiftLeftBy 48 p7) (Num.shiftLeftBy 56 p8)
    Num.bitwiseOr (Num.bitwiseOr a b) (Num.bitwiseOr c d)

# Get the next 4 bytes as a U64
wyr4 : List U8, Nat -> U64
wyr4 = \list, index ->
    # TODO: Remove the and in the future, it shouldn't be needed.
    p1 = Num.bitwiseAnd 0xFF (Num.toU64 (getByte list index))
    p2 = Num.bitwiseAnd 0xFF (Num.toU64 (getByte list (index + 1)))
    p3 = Num.bitwiseAnd 0xFF (Num.toU64 (getByte list (index + 2)))
    p4 = Num.bitwiseAnd 0xFF (Num.toU64 (getByte list (index + 3)))
    a = Num.bitwiseOr p1 (Num.shiftLeftBy 8 p2)
    b = Num.bitwiseOr (Num.shiftLeftBy 16 p3) (Num.shiftLeftBy 24 p4)
    Num.bitwiseOr a b

# Get the next K bytes with some shifting.
# K must be 3 or less.
wyr3 : List U8, Nat, Nat -> U64
wyr3 = \list, index, k ->
    # ((uint64_t)p[0])<<16)|(((uint64_t)p[k>>1])<<8)|p[k-1]
    p1 = Num.toU64 (getByte list index)
    p2 = Num.toU64 (getByte list (index + (shiftRightZfByHack 1 k)))
    p3 = Num.toU64 (getByte list (index + k - 1))
    a = Num.bitwiseOr (Num.shiftLeftBy 16 p1) (Num.shiftLeftBy 8 p2)
    Num.bitwiseOr a p3


hashBytes : Seed, List U8 -> U64
hashBytes = \$Seed oldSeed, list ->
    len = List.len list
    seed = Num.bitwiseXor oldSeed wyp0
    ab =
        if len <= 16 then
            if len >= 4 then
                x = Num.shiftLeftBy 2 (shiftRightZfByHack 3 len)
                a = Num.bitwiseOr (Num.shiftLeftBy 32 (wyr4 list 0)) (wyr4 list x)
                b = Num.bitwiseOr (Num.shiftLeftBy 32 (wyr4 list (len-4))) (wyr4 list (len-4-x))
                {a, b}
            else if len > 0 then
                {a: wyr3 list 0 len, b: 0}
            else
                {a: 0, b: 0}
        else
            {a : 0, b: 0 }
    wymix (Num.bitwiseXor wyp1 (Num.toU64 len)) (wymix (Num.bitwiseXor wyp1 ab.a) (Num.bitwiseXor seed ab.b))


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