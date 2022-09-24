app "hashdict-insert-bench"
    packages { pf: "platform/main.roc" }
    imports [ pf.Task, U64FlatHashDict ]
    provides [ main ] to pf

# Time to insert and then remove.

main : Task.Task {} []
main =
    epochs <- Task.after Task.getInt

    # iters set to a reasonably large value that is too small to cause the dict to need to grow.
    iters = 1000000
    Task.putInt (epochHelper epochs iters 0)

epochHelper: I64, I64, I64 -> I64
epochHelper = \remaining, iters, val ->
    if remaining <= 0 then
        val
    else
        x = U64FlatHashDict.empty 0
            |> insertHelper iters
            |> Num.toI64
        epochHelper (remaining - 1) iters (val + x)


insertHelper : U64FlatHashDict.U64FlatHashDict I64, I64 -> I64
insertHelper = \dict, remaining ->
    if remaining <= 0 then
        # Just return a dummy value and how this function doesn't get optimized away.
        remaining
    else
        nextDict = U64FlatHashDict.insert dict (Num.toU64 remaining) remaining
        insertHelper nextDict (remaining - 1)