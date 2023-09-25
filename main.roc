app "main"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.File,
        pf.Arg,
        pf.Path
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

Program : [Program]

ParseError : [ParseError]

parse : Str -> Result Program ParseError
parse = \file -> Ok Program

run : Str -> Task {} I32
run = \program -> when parse program is
    Err e -> parseErrorToString e |> Stdout.line
    Ok prog -> interpret prog

interpret : Program -> Task {} I32
interpret = \program -> Stdout.line "done interpreting"

parseErrorToString : ParseError -> Str
parseErrorToString = \error ->
    when error is
        ParseError -> "I was unable to parse the input file"

