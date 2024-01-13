app "gob"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.4/yrk4tKd0w9oaxt0s66zrejc6L67Y7B-86BQrL9yjZMY.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
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
    when args is
        [] | [_] | [_, "--help"] | [_, "-h"] -> Stdout.line helpMessage
        [_, path, ..] ->    
            result <- Path.fromStr path |> File.readUtf8 |> Task.attempt
            when result is
                Err _ -> Stdout.line "I wasn't able to read from '\(path)'"
                Ok file ->
                    # TODO: use a real command line arg parser to make the cli more robust once one is available
                    pipeInputTask = if contains args "--pipe" "-p" then Task.loop "" readStdin else Task.ok ""
                    stdin <- Task.await pipeInputTask
                    run file stdin (toConfig args)


contains = \list, x, y -> 
    List.contains list x || List.contains list y

helpMessage = 
    """
    The gob-lang cli

    usage: gob <filepath>.gob [option]

    options:
        -h, --help      print this message
        -d, --debug     print all intermediate states
        -s, --step      print the current state and wait for a key press to print the next state

    examples:
        gob examples/factorial.gob
        gob advent_of_code.gob --debug
        gob program.gob -s
    """

Config : [Step, Debug, None]

toConfig = \args ->
    if contains args "--step" "-s"
    then Step
    else if contains args "--debug" "-d"
    then Debug
    else None

readStdin = \lines ->
    result <- Stdin.line |> Task.await
    state =
        when result is
            Input line -> Step (Str.joinWith [lines, line] "\n")
            End -> Done lines
    Task.ok state

run : Str, Str, Config -> Task {} I32
run = \file, stdin, config ->
    when Parser.parse file is
        Err msg -> Stdout.line msg
        Ok fileProg ->
            when Parser.parse stdin is
                Err msg -> Stdout.line msg
                Ok stdinProg ->
                    when compose stdinProg fileProg is
                        Err msg -> Stdout.line msg
                        Ok prog ->
                            msg <- Task.loop ([], prog) (\x -> interpret x config) |> Task.await
                            Stdout.line msg


# Merge the program read from the file and the one read from stdin into a single one
compose : Program, Program -> Result Program Str
compose = \x, y ->
    if
        Dict.keys x.defs |> List.any \k -> Dict.contains y.defs k
    then
        Err "I found a duplicate key in the piped input"
    else
        Ok { defs: Dict.insertAll x.defs y.defs, body: List.concat x.body y.body }

interpret = \(stack, program), config ->
    task =
        when config is
            Debug -> showExecution stack program.body |> Stdout.line
            Step ->
                _ <- showExecution stack program.body |> Stdout.write |> Task.await
                Stdin.line |> Task.map \_ -> {}

            _ -> Task.ok {}
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
        ArgMustBePositive name num -> "Whoops, \(name) can't operate on a negative value like \(Num.toStr num)!"

StepError : [
    EndOfProgram Stack,
    UnknownName Str,
    Arity Str Nat,
    TypeMismatch Str,
    ArgMustBePositive Str I64,
]

step : Stack, Program -> Result (Stack, Program) StepError
step = \stack, program ->
    p = { program & body: List.dropFirst program.body 1 }
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
                [.. as rest, Number x, Number y] ->
                    Ok (rest |> List.append (Number (x + y)), p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "-" ->
            when stack is
                [.. as rest, Number x, Number y] ->
                    Ok (rest |> List.append (Number (x - y)), p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "*" ->
            when stack is
                # There is some bug preventing me from pattern matching on rest here. When I add dbg stack, the bug disappears
                [.., Number x, Number y] -> 
                    Ok (stack |> List.dropLast 2 |> List.append (Number (x * y)), p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "dup" ->
            when stack is
                [.., x] -> Ok (List.append stack x, p)
                _ -> Err (Arity name 1)

        "swap" ->
            when stack is
                [.. as rest, x, y] -> Ok (rest |> List.concat [y, x], p)
                _ -> Err (Arity name 2)

        "dig" ->
            when stack is
                [.. as rest, x, y, z] -> Ok (rest |> List.concat [y, z, x], p)
                _ -> Err (Arity name 3)

        "apply" ->
            when stack is
                [.. as rest, Quotation ts] -> Ok (rest, { p & body: List.concat ts p.body })
                [.., _] -> Err (TypeMismatch name)
                [] -> Err (Arity name 1)

        "repeat" ->
            when stack is
                [.. as rest, t, Number x] if x > 0 -> Ok (rest |> List.concat (List.repeat t (Num.toNat x)), p)
                [.. as rest, t, Number x] -> Err (ArgMustBePositive name x)
                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "compose" ->
            when stack is
                [.. as rest, Quotation x, Quotation y] -> Ok (rest |> List.append (Quotation (List.concat x y)), p)
                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "branch" ->
            when stack is
                [.. as rest, Builtin "true", branch, _] -> Ok (rest |> List.append branch, p)
                [.. as rest, Builtin "false", _, branch] -> Ok (rest |> List.append branch, p)
                [.., _, _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 3)

        "=" ->
            when stack is
                [.. as rest, x, y] ->
                    toBool = \b -> if b then Builtin "true" else Builtin "false"
                    Ok (rest |> List.append (toBool (x == y)), p)

                _ -> Err (Arity name 2)

        "quote" ->
            when stack is
                [.. as rest, x] -> Ok (rest |> List.append (Quotation [x]), p)
                _ -> Err (Arity name 1)

        "drop" ->
            when stack is
                [.. as rest, _] -> Ok (rest, p)
                _ -> Err (Arity name 1)

        "split" ->
            when stack is
                [.. as rest, String x, String y] ->
                    chunks = Str.split x y |> List.map String
                    Ok (rest |> List.concat chunks, p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "true" -> Ok (List.append stack (Builtin "true"), p)
        "false" -> Ok (List.append stack (Builtin "false"), p)
        # TODO: refactor the builtins to be tags instead of strings which would avoid the need for this.
        _ -> crash "***crash*** There was either an error during parsing or \(name) hasn't been implemented yet."


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

showTerm = \term ->
    when term is
        Number x -> Num.toStr x
        String s -> "\"\(s)\""
        Quotation prog -> "[\(showTerms prog)]"
        Builtin s -> s
        Def s -> s
