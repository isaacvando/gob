app "stack"
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
        parser.Core, # must be imported here to be used by Parser.roc
        parser.String, # must be imported here to be used by Parser.roc
        Parser.{ Program, Stack, Term },
    ]
    provides [main] to pf

main : Task {} I32
main =
    args <- Arg.list |> Task.await
    when List.get args 1 is
        Err _ -> Stdout.line "I couldn't find any command line arguments. Please try again with the path to a stack program."
        Ok path ->
            result <- Path.fromStr path |> File.readUtf8 |> Task.attempt
            when result is
                Err _ -> Stdout.line "I wasn't able to read from '\(path)'"
                Ok file ->
                    config = List.contains args "--debug"
                    run file config

Config : Bool

# Execution
run : Str, Config -> Task {} I32
run = \input, config ->
    when Parser.parse input is
        Err msg -> Stdout.line msg
        Ok prog ->
            msg <- Task.loop ([], prog) (\x -> interpret x config) |> Task.await
            Stdout.line msg

interpret = \(stack, program), config ->
    task = if config then showExecution stack program.body |> Stdout.line else Task.ok {}
    _ <- Task.await task
    result =
        when step stack program is
            Ok state -> Step state
            Err err -> Done (handleStepError err)

    result |> Task.ok

handleStepError : StepError -> Str
handleStepError = \err ->
    when err is
        EndOfProgram stack -> showTerms stack
        Arity name n ->
            when n is
                1 -> "Uh oh, \(name) expects there to be at least 1 element on the stack but there weren't any."
                _ -> "Uh oh, \(name) expects there to be at least \(Num.toStr n) elements on the stack but there weren't enough."

        TypeMismatch name -> "Uh oh, \(name) can't operate on that kind of arguments."
        UnknownName name -> "Uh oh, I don't know anything named '\(name)'."

StepError : [
    EndOfProgram Stack,
    UnknownName Str,
    Arity Str Nat,
    TypeMismatch Str,
]

step : Stack, Program -> Result (Stack, Program) StepError
step = \stack, program ->
    p = { program & body: List.dropFirst program.body }
    when List.first program.body is
        Err _ -> Err (EndOfProgram stack)
        Ok term ->
            when term is
                Number _ -> Ok (stack |> List.append term, p)
                String _ -> Ok (stack |> List.append term, p)
                Quotation _ -> Ok (stack |> List.append term, p)
                Builtin s -> stepBuiltin stack p s
                Def name ->
                    when Dict.get program.defs name is
                        Err _ -> Err (UnknownName name)
                        Ok ts -> Ok (stack, { p & body: List.concat ts p.body })

stepBuiltin : Stack, Program, Str -> Result (Stack, Program) StepError
stepBuiltin = \stack, p, name ->
    when name is
        "+" ->
            when stack is
                [.., Number x, Number y] ->
                    Ok (stack |> dropLast 2 |> List.append (Number (x + y)), p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "-" ->
            when stack is
                [.., Number x, Number y] ->
                    Ok (stack |> dropLast 2 |> List.append (Number (x - y)), p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "*" ->
            when stack is
                [.., Number x, Number y] ->
                    s = stack |> dropLast 2 |> List.append (Number (x * y))
                    Ok (s, p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "dup" ->
            when stack is
                [.., x] -> Ok (List.append stack x, p)
                _ -> Err (Arity name 1)

        "swap" ->
            when stack is
                [.., x, y] -> Ok (stack |> dropLast 2 |> List.concat [y, x], p)
                _ -> Err (Arity name 2)

        "dig" ->
            when stack is
                [.., x, y, z] -> Ok (stack |> dropLast 3 |> List.concat [y, z, x], p)
                _ -> Err (Arity name 3)

        "apply" ->
            when stack is
                [.., Quotation ts] -> Ok (stack |> List.dropLast, { p & body: List.concat ts p.body })
                [.., _] -> Err (TypeMismatch name)
                [] -> Err (Arity name 1)

        "repeat" ->
            when stack is
                [.., t, Number x] -> Ok (stack |> dropLast 2 |> List.concat (List.repeat t x), p)
                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "compose" ->
            when stack is
                [.., Quotation x, Quotation y] -> Ok (stack |> dropLast 2 |> List.append (Quotation (List.concat x y)), p)
                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "branch" ->
            when stack is
                [.., Builtin "true", branch, _] -> Ok (stack |> dropLast 3 |> List.append branch, p)
                [.., Builtin "false", _, branch] -> Ok (stack |> dropLast 3 |> List.append branch, p)
                [.., _, _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 3)

        "=" ->
            when stack is
                [.., x, y] ->
                    toBool = \b -> if b then Builtin "true" else Builtin "false"
                    Ok (stack |> dropLast 2 |> List.append (toBool (x == y)), p)

                _ -> Err (Arity name 2)

        "quote" ->
            when stack is
                [.., x] -> Ok (stack |> List.dropLast |> List.append (Quotation [x]), p)
                _ -> Err (Arity name 1)

        "drop" ->
            when stack is
                [.., _] -> Ok (stack |> List.dropLast, p)
                _ -> Err (Arity name 1)

        "true" -> Ok (List.append stack (Builtin "true"), p)
        "false" -> Ok (List.append stack (Builtin "false"), p)
        _ -> crash "***crash*** There was either an error during parsing or \(name) hasn't been implemented yet."

dropLast : List elem, Nat -> List elem
dropLast = \list, n ->
    remaining = Num.subSaturated (List.len list) n

    List.takeFirst list remaining

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

