app "wyhash-u64-bytes-bench"
    packages { pf: "platform/main.roc" }
    imports [ pf.Task, Wyhash ]
    provides [ main ] to pf

seed : Wyhash.Seed
seed = Wyhash.createSeed 0x1234_5678_90AB_CDEF

base : U64
base = 0xFEDC_BA09_8765_4321

main : Task.Task {} []
main =
    iters <- Task.after Task.getInt
    hashHelper base iters (List.repeat 0 8)
        |> Num.toI64
        |> Task.putInt

setBytes : List U8, U64 -> List U8
setBytes = \oldList, val ->
    # TODO: remove Num.toU8 once bitwiseAnd works correctly.
    p0 = Num.toU8 (Num.bitwiseAnd 0xFF val)
    p1 = Num.toU8 (Num.bitwiseAnd 0xFF (Num.shiftRightBy val 8))
    p2 = Num.toU8 (Num.bitwiseAnd 0xFF (Num.shiftRightBy val 16))
    p3 = Num.toU8 (Num.bitwiseAnd 0xFF (Num.shiftRightBy val 24))
    p4 = Num.toU8 (Num.bitwiseAnd 0xFF (Num.shiftRightBy val 32))
    p5 = Num.toU8 (Num.bitwiseAnd 0xFF (Num.shiftRightBy val 40))
    p6 = Num.toU8 (Num.bitwiseAnd 0xFF (Num.shiftRightBy val 48))
    p7 = Num.toU8 (Num.bitwiseAnd 0xFF (Num.shiftRightBy val 56))

    oldList
        |> List.set 0 p0
        |> List.set 1 p1
        |> List.set 2 p2
        |> List.set 3 p3
        |> List.set 4 p4
        |> List.set 5 p5
        |> List.set 6 p6
        |> List.set 7 p7

hashHelper : U64, I64, List U8 -> U64
hashHelper = \val, iters, list ->
    if iters <= 0 then
        val
    else
        newList = setBytes list val
        next = Wyhash.hashBytes seed newList

        hashHelper next (iters - 1) newList
