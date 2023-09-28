app "main"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br",
        parser: "Parser/main.roc",
    }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.File,
        pf.Arg,
        pf.Path,
        parser.Core.{ Parser },
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

# Types
Program : List Term
Term : [Copy, Add, Multiply, Number I64, String Str]

# Parsing
parse : Str -> Result Program [ParsingFailure Str, ParsingIncomplete Str]
parse = \file ->
    cleaned =
        file
        |> Str.replaceEach " " ""
        |> Str.replaceEach "\t" " "
    Core.parse (Core.fail "fail") cleaned Str.isEmpty

# Execution
run : Str -> Task {} I32
run = \program ->
    when parse program is
        Err (ParsingFailure msg) -> Str.concat "I wasn't able to parse the input file: " msg |> Stdout.line
        Err (ParsingIncomplete _) -> Stdout.line "I couldn't parse the whole file" gs
        Ok prog -> interpret prog

interpret : Program -> Task {} I32
interpret = \program -> Stdout.line program

