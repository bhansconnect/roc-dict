app "echo"
    packages { pf: "platform/main.roc" }
    imports [ pf.Stdin, pf.Stdout, pf.Task, U64FlatHashDict ]
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
    dict = U64FlatHashDict.empty ""
        |> U64FlatHashDict.insert 10 "~~~~~~~~~~"
        |> U64FlatHashDict.insert 9 "---------"
        |> U64FlatHashDict.insert 8 "~~~~~~~~"
        |> U64FlatHashDict.insert 7 "-------"
        |> U64FlatHashDict.insert 6 "~~~~~~"
        |> U64FlatHashDict.insert 5 "-----"
        |> U64FlatHashDict.insert 12 "garbage"
        |> U64FlatHashDict.insert 4 "wrong"
        |> U64FlatHashDict.insert 3 "---"
        |> U64FlatHashDict.insert 2 "~~"
        |> U64FlatHashDict.insert 1 "-"
        |> U64FlatHashDict.insert 0 ""
        |> U64FlatHashDict.remove 12
        |> (\T rdict _ -> rdict)
        |> U64FlatHashDict.insert 4 "~~~~"

    silence = \cache, length ->
        spaceInUtf8 = 32

        when U64FlatHashDict.get cache (Num.toU64 length) is
            Ok val ->
                Str.toUtf8 val

            Err NotFound ->
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
