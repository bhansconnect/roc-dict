app "wyhash-1k-list-bench"
    packages { pf: "../roc/examples/benchmarks/platform" }
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
hashHelper = \hash, iters, val ->
    if iters <= 0 then
        hash
    else
        next = Wyhash.combine hash (Wyhash.hashBytes seed val)
        hashHelper next (iters - 1) val
