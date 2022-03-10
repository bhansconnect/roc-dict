app "wyhash-bench"
    packages { pf: "../roc/examples/benchmarks/platform" }
    imports [ pf.Task, Wyhash ]
    provides [ main ] to pf

seed : Wyhash.Seed
seed = Wyhash.createSeed 0x1234_5678_90AB_CDEF

base : U64
base = 0xFEDC_BA09_5678_4321

main : Task.Task {} []
main =
    iters <- Task.after Task.getInt
    hashHelper base iters
        |> Num.toI64
        |> Task.putInt
    

hashHelper : U64, I64 -> U64
hashHelper = \val, iters ->
    if iters <= 0 then
        val
    else
        next = Wyhash.hashU64 seed val
        hashHelper next (iters - 1)
