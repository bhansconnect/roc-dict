interface Group
    exposes [ Group, create, allEmpty, emptySlot, deletedSlot, h1, h2, H2, size, mulSize, match, matchEmpty, matchEmptyOrDeleted, matchFull, updateKeyAtOffset ]
    imports [ BitMask.{ BitMask } ]

Group := U64

# This is based off of absl::flat_hash_map.
# It is simplified to make it nicer to write in roc.
emptySlot : I8
emptySlot = -128
deletedSlot : I8
deletedSlot = -2
# sentinel : I8
# sentinel = -1

allEmpty : Group
allEmpty = $Group 0x8080_8080_8080_8080

create : U64 -> Group
create = \g ->
    $Group g

# These don't exactly go here, but I think they fit best here.
h1 : U64 -> U64
h1 = \hashKey ->
    shiftRightZfByHack 7 hashKey

H2 := U8

h2 : U64 -> H2
h2 = \hashKey ->
    $H2 (Num.toU8 (Num.bitwiseAnd hashKey 0b0111_1111))

# Must be a multiple of 2 and equivalent to the number of bytes in Group.
size : Nat
size = 8

mul8 = \val -> Num.shiftLeftBy 3 val
mulSize = mul8

# All of these group matching functions could use SSE and would merit adding builtins.
match : Group, H2 -> BitMask
match = \$Group g, $H2 h ->
    msbs = 0x8080808080808080
    lsbs = 0x0101010101010101
    x = Num.bitwiseXor g (lsbs * (Num.toU64 h))
    y = Num.bitwiseAnd msbs (bitwiseNot x)
    BitMask.create (Num.bitwiseAnd (x - lsbs) y)

matchFull : Group -> BitMask
matchFull = \$Group g ->
    msbs = 0x8080808080808080
    ng = bitwiseNot g
    BitMask.create (Num.bitwiseAnd ng msbs)

matchEmpty : Group -> BitMask
matchEmpty = \$Group g ->
    msbs = 0x8080808080808080
    ng = bitwiseNot g
    x = (Num.bitwiseAnd g (Num.shiftLeftBy 6 ng))
    BitMask.create (Num.bitwiseAnd x msbs)

matchEmptyOrDeleted : Group -> BitMask
matchEmptyOrDeleted = \$Group g ->
    msbs = 0x8080808080808080
    ng = bitwiseNot g
    x = (Num.bitwiseAnd g (Num.shiftLeftBy 7 ng))
    BitMask.create (Num.bitwiseAnd x msbs)
    
bitwiseNot : U64 -> U64
bitwiseNot = \x ->
    Num.bitwiseXor 0xFFFF_FFFF_FFFF_FFFF x

# This is broken. Zf and normal are filled.
shiftRightZfByHack = \by, val ->
    Num.shiftRightBy by val

updateKeyAtOffset : Group, Nat, H2 -> Group
updateKeyAtOffset = \$Group g, offset, $H2 updateVal ->
    bitOffset = mul8 offset
    # No bitwiseNot update when added.
    mask = bitwiseNot (Num.toU64 (Num.shiftLeftBy bitOffset 0xFF))
    update = Num.shiftLeftBy bitOffset (Num.toNat updateVal)
    $Group (Num.bitwiseOr (Num.bitwiseAnd g (Num.toU64 mask)) (Num.toU64 update))
