interface Wyhash
    exposes [ hashU64, createSeed, Seed, rand ]
    imports []

# Note: this is just the version of wyhash for 64 bit numbers.
# https://github.com/wangyi-fudan/wyhash/blob/master/wyhash.h
# Also, apparently on some arm systems 64x64 to 128 is quite slow and wouldn't be wanted.
wyp0 : U64
wyp0 = 0xa0761d6478bd642f
wyp1 : U64
wyp1 = 0xe7037ed1a0b428db
#wyp2 : U64
#wyp2 = 0x8ebc6af09c88c6e3
#wyp3 : U64
#wyp3 = 0x589965cc75374cc3

wymum : U64, U64 -> [ T U64 U64 ]
wymum = \a, b ->
    r = Num.toU128 a * Num.toU128 b
    lowerR = Num.bitwiseAnd r (Num.toU128 Num.maxU64)
    upperR = Num.shiftRightZfBy 64 r

    # This is the more robust form.
    # T (Num.bitwiseXor a (Num.toU64 lowerR)) (Num.bitwiseXor b (Num.toU64 upperR))
    T (Num.toU64 lowerR) (Num.toU64 upperR)

wymix : U64, U64 -> U64
wymix = \a, b ->
    when wymum a b is
        T x y ->
            Num.bitwiseXor x y

wyhash64 : U64, U64 -> U64
wyhash64 = \a, b ->
    when wymum (Num.bitwiseXor a wyp0) (Num.bitwiseXor b wyp1) is
        T x y ->
            wymix (Num.bitwiseXor x wyp0) (Num.bitwiseXor y wyp1)

Seed := U64

createSeed : U64 -> Seed
createSeed = \seed -> $Seed seed

rand : Seed -> [ T Seed U64 ]
rand = \$Seed seed ->
    nextSeed = seed + wyp0

    T ($Seed nextSeed) (wymix seed (Num.bitwiseXor seed wyp1))

# The actual wyhash function is made to operate on raw bytes.
# Instead I am directly implementing it for specific types.
hashU64 : Seed, U64 -> U64
hashU64 = \$Seed seed, key ->
    upper = Num.shiftRightZfBy 32 key
    lower = Num.bitwiseAnd (Num.toU64 Num.maxU32) key
    a = Num.bitwiseAnd (Num.shiftLeftBy 32 upper) lower
    b = Num.bitwiseAnd (Num.shiftLeftBy 32 lower) upper

    wymix (Num.bitwiseXor wyp1 8) (wymix (Num.bitwiseXor wyp1 a) (Num.bitwiseXor (Num.bitwiseXor seed wyp0) b))

