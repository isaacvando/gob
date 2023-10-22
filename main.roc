app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.1.0/vPU-UZbWGIXsAfcJvAnmU3t3SWlHoG_GauZpqzJiBKA.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.File,
        pf.Arg,
        pf.Path,
        parser.Core,
        parser.String,
        Parser.{ Program, Stack, Term },
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await
    when List.get args 1 is
        Err _ -> Stdout.line "I couldn't find any command line arguments. Please try again with the path to a stack program."
        Ok arg ->
            read = Path.fromStr arg |> File.readUtf8
            Task.attempt read \result ->
                when result is
                    Err _ -> Stdout.line "I wasn't able to read from '\(arg)'"
                    Ok file -> run file

# Execution
run : Str -> Task {} I32
run = \input ->
    when Parser.parse input is
        Err msg -> Stdout.line msg
        Ok prog ->
            msg <- Task.loop ([], prog) interpret |> Task.await
            when msg is
                Ok {} -> Task.ok {}
                Err m -> Stdout.line m

interpret = \(stack, program) ->
    _ <- showExecution stack program.body |> Stdout.line |> Task.await
    when step stack program is
        Err EndOfProgram -> Done (Ok {}) |> Task.ok
        Err Exception -> Done (Err "Uh oh, something went wrong!") |> Task.ok
        Ok state -> Step state |> Task.ok

step : Stack, Program -> Result (Stack, Program) [EndOfProgram, Exception]
step = \stack, program ->
    p = {program & body: List.dropFirst program.body }
    when List.first program.body is
        Err _ -> Err EndOfProgram
        Ok term ->
            when term is
                Number x -> Ok (List.append stack (Number x), p)
                String x -> Ok (List.append stack (String x), p)
                Quotation _ -> Ok (stack |> List.append term, p)
                Builtin s -> stepBuiltin stack p s
                Def s -> when Dict.get program.defs s is
                    Err _ -> Err Exception
                    Ok ts -> Ok (stack, {p & body: List.concat ts p.body})
                _ -> Err Exception

stepBuiltin : Stack, Program, Str -> Result (Stack, Program) [EndOfProgram, Exception]
stepBuiltin = \stack, p, name ->
    when name is
        "+" ->
            when stack is
                [.., Number x, Number y] ->
                    s = stack |> dropLast2 |> List.append (Number (x + y))
                    Ok (s, p)

                _ -> Err Exception

        "-" ->
            when stack is
                [.., Number x, Number y] ->
                    s = stack |> dropLast2 |> List.append (Number (x - y))
                    Ok (s, p)

                _ -> Err Exception

        "*" ->
            when stack is
                [.., Number x, Number y] ->
                    s = stack |> dropLast2 |> List.append (Number (x * y))
                    Ok (s, p)

                _ -> Err Exception

        "dup" ->
            when stack is
                [.., x] -> Ok (List.append stack x, p)
                _ -> Err Exception

        "swap" ->
            when stack is
                [.., x, y] -> Ok (stack |> dropLast2 |> List.concat [y, x], p)
                _ -> Err Exception

        "dig" ->
            when stack is
                [.., x, y, z] -> Ok (stack |> dropLast2 |> List.dropLast |> List.concat [y, z, x], p)
                _ -> Err Exception

        "apply" ->
            when stack is
                [.., Quotation ts] -> Ok (stack |> List.dropLast, {p & body: List.concat ts p.body })
                _ -> Err Exception

        "repeat" ->
            when stack is
                [.., t, Number x] -> Ok (stack |> List.dropLast |> List.dropLast |> List.concat (List.repeat t x), p)
                _ -> Err Exception

        "compose" ->
            when stack is
                [.., Quotation x, Quotation y] -> Ok (stack |> List.dropLast |> List.dropLast |> List.append (Quotation (List.concat x y)), p)
                _ -> Err Exception

        "branch" ->
            when stack is
                [.., Builtin "true", branch, _] -> Ok (stack |> List.dropLast |> List.dropLast |> List.dropLast |> List.append branch, p)
                [.., Builtin "false", _, branch] -> Ok (stack |> List.dropLast |> List.dropLast |> List.dropLast |> List.append branch, p)
                _ -> Err Exception

        "=" ->
            when stack is
                [.., x, y] ->
                    toBool = \b -> if b then Builtin "true" else Builtin "false"
                    Ok (stack |> List.dropLast |> List.dropLast |> List.append (toBool (x == y)), p)

                _ ->
                    dbg
                        stack

                    Err Exception

        "quote" ->
            when stack is
                [.., x] -> Ok (stack |> List.dropLast |> List.append (Quotation [x]), p)
                _ -> Err Exception

        "drop" ->
            when stack is
                [.., x] -> Ok (stack |> List.dropLast, p)
                _ -> Err Exception

        "true" -> Ok (List.append stack (Builtin "true"), p)
        "false" -> Ok (List.append stack (Builtin "false"), p)
        _ -> Err Exception

dropLast2 : List a -> List a
dropLast2 = \list ->
    list |> List.dropLast |> List.dropLast

showExecution : Stack, List Term -> Str
showExecution = \stack, program ->
    showTerms stack
    |> Str.concat " | "
    |> Str.concat (showTerms program)

showTerms : List Term -> Str
showTerms = \terms ->
    terms
    |> List.map showTerm
    |> Str.joinWith " "

# showTerm : Term -> Str
showTerm = \term ->
    when term is
        Number x -> Num.toStr x
        String s -> "\"\(s)\""
        Quotation prog -> "[\(showTerms prog)]"
        Builtin s -> s
        Def s -> s
        _ -> "catchall"

