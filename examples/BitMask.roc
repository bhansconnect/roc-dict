interface BitMask
    exposes [ BitMask, create, lowestSet, next, hasMore, any, walk, walkUntil ]
    imports [ ]


BitMask := U64

create : U64 -> BitMask
create = \mask ->
    @BitMask mask

# This function is only valid if mask != 0.
# So call hasMore or Any before using this.
lowestSet : BitMask -> Nat
lowestSet = \@BitMask mask ->
    trailingZeroCount mask

next : BitMask -> BitMask
next = \@BitMask mask ->
    nextMask = Num.bitwiseAnd mask (Num.subWrap mask 1)
    @BitMask nextMask

any : BitMask -> Bool
any = hasMore

hasMore : BitMask -> Bool
hasMore = \@BitMask mask ->
    mask != 0

walk : BitMask, a, (a, Nat -> a) -> a
walk = \mask, state, callback ->
    if hasMore mask then
        offset = lowestSet mask
        nextState = callback state offset
        nextMask = next mask
        walk nextMask nextState callback
    else
        state

walkUntil : BitMask, a, (a, Nat -> [ Stop a, Continue a ]) -> a
walkUntil = \mask, state, callback ->
    if hasMore mask then
        offset = lowestSet mask
        when callback state offset is
            Continue nextState ->
                nextMask = next mask
                walkUntil nextMask nextState callback
            Stop nextState ->
                nextState
    else
        state

bitwiseNot : U64 -> U64
bitwiseNot = \x ->
    Num.bitwiseXor 0xFFFF_FFFF_FFFF_FFFF x

# This could be a builtin a take advantage of SSE instructions on some platforms.
# Returns the number of trailing zero bytes in a number.
# This function is only valid if num != 0
trailingZeroCount : U64 -> Nat
trailingZeroCount = \num ->
    x = Num.bitwiseAnd num (Num.addWrap (bitwiseNot num) 1)
    c0 = 63
    c1 = Num.subWrap c0 (if Num.bitwiseAnd x 0x00000000FFFFFFFF != 0 then 32 else 0)
    c2 = Num.subWrap c1 (if Num.bitwiseAnd x 0x0000FFFF0000FFFF != 0 then 16 else 0)
    c3 = Num.subWrap c2 (if Num.bitwiseAnd x 0x00FF00FF00FF00FF != 0 then 8 else 0)
    c4 = Num.subWrap c3 (if Num.bitwiseAnd x 0x0F0F0F0F0F0F0F0F != 0 then 4 else 0)
    c5 = Num.subWrap c4 (if Num.bitwiseAnd x 0x3333333333333333 != 0 then 2 else 0)
    c6 = Num.subWrap c5 (if Num.bitwiseAnd x 0x5555555555555555 != 0 then 1 else 0)
    Num.shiftRightZfBy c6 3
