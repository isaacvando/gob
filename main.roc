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
            Stdout.line msg

interpret = \(stack, program) ->
    _ <- showExecution stack program |> Stdout.line |> Task.await
    when step stack program is
        Err EndOfProgram -> Done "Done" |> Task.ok
        Err Exception -> Done "Uh oh, something went wrong!" |> Task.ok
        Ok state -> Step state |> Task.ok

step : Stack, Program -> Result (Stack, Program) [EndOfProgram, Exception]
step = \stack, program ->
    p = List.dropFirst program
    when List.first program is
        Err _ -> Err EndOfProgram
        Ok term ->
            when term is
                Number x -> Ok (List.append stack (Number x), p)
                Add ->
                    when stack is
                        [.., Number x, Number y] ->
                            s = stack |> dropLast2 |> List.append (Number (x + y))
                            Ok (s, p)

                        _ -> Err Exception

                Multiply ->
                    when stack is
                        [.., Number x, Number y] ->
                            s = stack |> dropLast2 |> List.append (Number (x * y))
                            Ok (s, p)

                        _ -> Err Exception

                Dup ->
                    when stack is
                        [.., x] -> Ok (List.append stack x, p)
                        _ -> Err Exception

                Swap ->
                    when stack is
                        [.., x, y] -> Ok (stack |> dropLast2 |> List.concat [y, x], p)
                        _ -> Err Exception

                Dig ->
                    when stack is
                        [.., x, y, z] -> Ok (stack |> dropLast2 |> List.dropLast |> List.concat [y, z, x], p)
                        _ -> Err Exception

                Quote _ -> 
                    Ok (stack |> List.append term, p)

                Apply -> 
                    when stack is
                        [.., Quote ts] -> Ok (stack |> List.dropLast, List.concat ts p)
                        _ -> Err Exception

                _ -> Err Exception

dropLast2 : List a -> List a
dropLast2 = \list ->
    list |> List.dropLast |> List.dropLast

showExecution : Stack, Program -> Str
showExecution = \stack, program ->
    showProgram stack
    |> Str.concat " | "
    |> Str.concat (showProgram program)

showProgram : Program -> Str
showProgram = \program ->
    program
    |> List.map showTerm
    |> Str.joinWith " "

# showTerm : Term -> Str
showTerm = \term ->
    when term is
        Number x -> Num.toStr x
        String s -> "'\(s)'"
        Add -> "+"
        Multiply -> "*"
        Dup -> "dup"
        Swap -> "swap"
        Dig -> "dig"
        Quote prog -> "[\(showProgram prog)]"
        Apply -> "apply"
        _ -> "catchall"

