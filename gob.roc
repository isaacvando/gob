app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.12.0/Lb8EgiejTUzbggO2HVVuPJFkwvvsfW6LojkLR20kTVE.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.7.1/MvLlME9RxOBjl0QCxyn3LIaoG9pSlaNxCa-t3BfbPNc.tar.br",
}

import cli.Stdout
import cli.Stdin
import cli.Task exposing [Task]
import cli.File
import cli.Arg
import cli.Arg
import cli.Arg.Opt as Opt
import cli.Arg.Cli as Cli
import cli.Arg.Param as Param
import Parser exposing [Program, Stack, Term]

main =
    cliParser =
        Cli.build {
            pipe: <- Opt.flag { short: "p", long: "pipe", help: "Should Gob read a program from stdin." },
            debug: <- Opt.flag { short: "d", long: "debug", help: "Should Gob print the state of the stack and program during execution. Conflicts with --step." },
            step: <- Opt.flag { short: "s", long: "step", help: "Should Gob step through the execution of the program by pressing any key. Conflicts with --debug." },
            path: <- Param.str { name: "path", help: "The path to the Gob program to run" },
        }
        |> Cli.finish {
            name: "gob",
            authors: ["Isaac Van Doren <https://github.com/isaacvando>"],
            description: "The gob-lang cli",
        }
        |> Cli.assertValid

    args =
        Cli.parseOrDisplayMessage cliParser (Arg.list! {})
            |> Task.fromResult
            |> Task.mapErr! \err -> Exit 1 err

    if args.debug && args.step then
        # TODO: restructure the CLI to remove this case
        error "Only one of --debug and --step can be specified at once."
        |> Task.err
    else
        file = File.readUtf8! args.path
        run file "" Debug

# main =
#   result <- path |> File.readUtf8 |> Task.attempt
#   when result is
#       Err _ -> Stdout.line "I wasn't able to read from '$(path)'"
#       Ok file ->
#           # TODO: use a real command line arg parser to make the cli more robust once one is available
#           pipeInputTask = if contains args "--pipe" "-p" then Task.loop "" readStdin else Task.ok ""
#           stdin <- Task.await pipeInputTask
#           run file stdin (toConfig args)

contains = \list, x, y ->
    List.contains list x || List.contains list y

Config : [Step, Debug, None]

# readStdin : List Str -> Task _ _
# readStdin = \lines ->
#   result = Stdin.line!
#   when result is
#       Input line -> Step (Str.joinWith [lines, line] "\n")
#       End -> Done lines

run = \file, stdin, config ->
    fileProgram =
        Parser.parse file
            |> Task.fromResult
            |> Task.mapErr! error

    stdinProgram =
        Parser.parse stdin
            |> Task.fromResult
            |> Task.mapErr! error

    finalProgram =
        compose stdinProgram fileProgram
            |> Task.fromResult
            |> Task.mapErr! error

    msg = Task.loop! ([], finalProgram) \x ->
        interpret x config
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
                1 -> "Uh oh, $(name) expects there to be at least 1 element on the stack but there weren't any."
                _ -> "Uh oh, $(name) expects there to be at least $(Num.toStr n) elements on the stack but there weren't enough."

        TypeMismatch name -> "Uh oh, $(name) can't operate on that kind of arguments."
        UnknownName name -> "Uh oh, I don't know anything named '$(name)'."
        ArgMustBePositive name num -> "Whoops, $(name) can't operate on a negative value like $(Num.toStr num)!"

StepError : [
    EndOfProgram Stack,
    UnknownName Str,
    Arity Str U64,
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
                [.., Number x, Number y] ->
                    Ok (stack |> List.dropLast 2 |> List.append (Number (x + y)), p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "-" ->
            when stack is
                [.., Number x, Number y] ->
                    Ok (stack |> List.dropLast 2 |> List.append (Number (x - y)), p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "*" ->
            when stack is
                [.., Number x, Number y] ->
                    s = stack |> List.dropLast 2 |> List.append (Number (x * y))
                    Ok (s, p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "dup" ->
            when stack is
                [.., x] -> Ok (List.append stack x, p)
                _ -> Err (Arity name 1)

        "swap" ->
            when stack is
                [.., x, y] -> Ok (stack |> List.dropLast 2 |> List.concat [y, x], p)
                _ -> Err (Arity name 2)

        "dig" ->
            when stack is
                [.., x, y, z] -> Ok (stack |> List.dropLast 3 |> List.concat [y, z, x], p)
                _ -> Err (Arity name 3)

        "apply" ->
            when stack is
                [.., Quotation ts] -> Ok (stack |> List.dropLast 1, { p & body: List.concat ts p.body })
                [.., _] -> Err (TypeMismatch name)
                [] -> Err (Arity name 1)

        "repeat" ->
            when stack is
                [.., t, Number x] if x > 0 -> Ok (stack |> List.dropLast 2 |> List.concat (List.repeat t (Num.toU64 x)), p)
                [.., _, Number x] -> Err (ArgMustBePositive name x)
                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "compose" ->
            when stack is
                [.., Quotation x, Quotation y] -> Ok (stack |> List.dropLast 2 |> List.append (Quotation (List.concat x y)), p)
                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "branch" ->
            when stack is
                [.., Builtin "true", branch, _] -> Ok (stack |> List.dropLast 3 |> List.append branch, p)
                [.., Builtin "false", _, branch] -> Ok (stack |> List.dropLast 3 |> List.append branch, p)
                [.., _, _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 3)

        "=" ->
            when stack is
                [.., x, y] ->
                    toBool = \b -> if b then Builtin "true" else Builtin "false"
                    Ok (stack |> List.dropLast 2 |> List.append (toBool (x == y)), p)

                _ -> Err (Arity name 2)

        "quote" ->
            when stack is
                [.., x] -> Ok (stack |> List.dropLast 1 |> List.append (Quotation [x]), p)
                _ -> Err (Arity name 1)

        "drop" ->
            when stack is
                [.., _] -> Ok (stack |> List.dropLast 1, p)
                _ -> Err (Arity name 1)

        "split" ->
            when stack is
                [.., String x, String y] ->
                    chunks = Str.split x y |> List.map String
                    Ok (stack |> List.dropLast 2 |> List.concat chunks, p)

                [.., _, _] -> Err (TypeMismatch name)
                _ -> Err (Arity name 2)

        "true" -> Ok (List.append stack (Builtin "true"), p)
        "false" -> Ok (List.append stack (Builtin "false"), p)
        # TODO: refactor the builtins to be tags instead of strings which would avoid the need for this.
        _ -> crash "***crash*** There was either an error during parsing or $(name) hasn't been implemented yet."

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
        String s -> "\"$(s)\""
        Quotation prog -> "[$(showTerms prog)]"
        Builtin s -> s
        Def s -> s

error : Str -> [Exit (Num *) Str]
error = \msg ->
    Exit 1 "\u(001b)[31mERROR:\u(001b)[0m $(msg)"
