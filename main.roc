app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.File,
        pf.Arg,
        pf.Path,
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
        Err msg -> Str.concat "I wasn't able to parse the input file: " msg |> Stdout.line
        Ok prog -> interpret [] prog

interpret : Stack, Program -> Task {} I32
interpret = \stack, program ->
    _ <- showExecution stack program |> Stdout.line |> Task.await
    when step stack program is
        Err EndOfProgram -> Stdout.line "done"
        Err Exception -> Stdout.line "Uh oh, something went wrong!"
        Ok (s,p) -> showExecution s p |> Stdout.line


step : Stack, Program -> Result (Stack, Program) [EndOfProgram, Exception]
step = \stack, program -> 
    when List.first program is
        Err _ -> Err EndOfProgram
        Ok term -> when term is
            Number x -> Ok (List.append stack (Number x), List.dropFirst program)
            _ -> Err Exception


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

showTerm : Term -> Str
showTerm = \term ->
    when term is
        Number x -> Num.toStr x
        Add -> "+"
        Multiply -> "*"
        Dup -> "dup"
        Swap -> "swap"
        Dig -> "dig"
