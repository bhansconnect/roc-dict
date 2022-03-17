app "hashdict-insert-remove-bench"
    packages { pf: "../roc/examples/benchmarks/platform" }
    imports [ pf.Task, U64FlatHashDict ]
    provides [ main ] to pf

# Time to insert and then remove.

main : Task.Task {} []
main =
    iters <- Task.after Task.getInt

    U64FlatHashDict.empty 0
        |> insertHelper iters 0
        |> (\T _ loads -> loads)
        |> Num.toI64
        |> Task.putInt

insertHelper : U64FlatHashDict.U64FlatHashDict I64, I64, Nat -> [ T (U64FlatHashDict.U64FlatHashDict I64) Nat ]
insertHelper = \dict, remaining, loads ->
    if remaining <= 0 then
        T dict loads
    else
        T nextDict nextLoads = U64FlatHashDict.insert dict (Num.toU64 remaining) remaining
        insertHelper nextDict (remaining - 1) (nextLoads + loads)

# removeHelper : U64FlatHashDict.U64FlatHashDict I64, I64, I64 -> I64
# removeHelper = \dict, remaining, accum ->
#     if remaining <= 0 then
#         accum
#     else
#         when U64FlatHashDict.remove dict (Num.toU64 remaining) is
#             T nextDict True ->
#                 removeHelper nextDict (remaining - 1) (accum + 1)
#             T nextDict False ->
#                 removeHelper nextDict (remaining - 1) accum
