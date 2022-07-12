app "wyhash-1k-list-bench"
    packages { pf: "platform/main.roc" }
    imports [ pf.Task, Wyhash ]
    provides [ main ] to pf

seed : Wyhash.Seed
seed = Wyhash.createSeed 0x1234_5678_90AB_CDEF

main : Task.Task {} []
main =
    iters <- Task.after Task.getInt
    hashHelper 0 iters (List.repeat 0x77 1024)
        |> Num.toI64
        |> Task.putInt

hashHelper : U64, I64, List U8 -> U64
hashHelper = \hash, remaining, val ->
    if remaining <= 0 then
        hash
    else
        nextVal = List.set val 0 (Num.toU8 (Num.bitwiseAnd 0xFF remaining))
        next = Wyhash.combine hash (Wyhash.hashBytes seed nextVal)

        hashHelper next (remaining - 1) nextVal
