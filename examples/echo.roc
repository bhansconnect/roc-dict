app "echo"
    packages { pf: "../roc/examples/interactive/cli-platform" }
    imports [ pf.Stdin, pf.Stdout, pf.Task, U64FlatDict ]
    provides [ main ] to pf

# TODO change examples to some form of benchmark.
main : Task.Task {} []
main =
    _ <- Task.await (Stdout.line "🗣  Shout into this cave and hear the echo! 👂👂👂")
    Task.loop {} (\_ -> Task.map tick Step)

tick : Task.Task {} []
tick =
    shout <- Task.await Stdin.line
    Stdout.line (echo shout)

echo : Str -> Str
echo = \shout ->
    dict = U64FlatDict.empty {}
        |> U64FlatDict.insert 10 "          "
        |> U64FlatDict.insert 9 "         "
        |> U64FlatDict.insert 8 "        "
        |> U64FlatDict.insert 7 "       "
        |> U64FlatDict.insert 6 "      "
        |> U64FlatDict.insert 5 "     "
        |> U64FlatDict.insert 4 "    "
        |> U64FlatDict.insert 3 "   "
        |> U64FlatDict.insert 2 "  "
        |> U64FlatDict.insert 1 " "
        |> U64FlatDict.insert 0 ""

    silence = \cache, length ->
        spaceInUtf8 = 32

        when U64FlatDict.get cache (Num.toU64 length) is
            Some val ->
                Str.toUtf8 val

            None ->
                List.repeat spaceInUtf8 length

    shout
        |> Str.toUtf8
        |> List.mapWithIndex
        (\_, i ->
                length = (List.len (Str.toUtf8 shout) - i)
                phrase = (List.split (Str.toUtf8 shout) length).before

                List.concat (silence dict (if i == 0 then 2 * length else length)) phrase)
        |> List.join
        |> Str.fromUtf8
        |> Result.withDefault ""
