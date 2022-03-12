app "hashdict-insert-remove-bench"
    packages { pf: "../roc/examples/benchmarks/platform" }
    imports [ pf.Task, U64FlatHashDict ]
    provides [ main ] to pf

# Time to insert and then remove.

main : Task.Task {} []
main =
    iters <- Task.after Task.getInt

    U64FlatHashDict.empty 0
        |> insertHelper iters
        |> removeHelper iters 0
        |> Num.toI64
        |> Task.putInt

insertHelper : U64FlatHashDict.U64FlatHashDict I64, I64 -> U64FlatHashDict.U64FlatHashDict I64
insertHelper = \dict, remaining ->
    if remaining <= 0 then
        dict
    else
        nextDict = U64FlatHashDict.insert dict (Num.toU64 remaining) remaining
        insertHelper nextDict (remaining - 1)

removeHelper : U64FlatHashDict.U64FlatHashDict I64, I64, I64 -> I64
removeHelper = \dict, remaining, accum ->
    if remaining <= 0 then
        accum
    else
        when U64FlatHashDict.remove dict (Num.toU64 remaining) is
            T nextDict True ->
                removeHelper nextDict (remaining - 1) (accum + 1)
            T nextDict False ->
                removeHelper nextDict (remaining - 1) accum
