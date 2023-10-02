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
    # when Parser.parse program is
    #     Err msg -> Str.concat "I wasn't able to parse the input file: " msg |> Stdout.line
    #     Ok prog -> interpret prog
    foo = parse input
    Stdout.line "foo"

interpret : Program -> Task {} I32
interpret = \p ->
    Stdout.line "finished running"



# Parsing

Program : List Term
Term : [
    Number I64,
    Add,
    Multiply,
    Dup,
    Swap,
    Dig,
    # Quote (List Term),
]

Parser : {
    tokens : List Str,
    index : Nat,
    result : Program,
}

parse : Str -> Result Program Str
parse = \input ->
    result = program {
        # tokens: lex input,
        tokens: ["dup"],
        index: 0,
        result: List.withCapacity 1000,
    }
    when result is
        Err x -> Err x
        Ok p -> Ok p.result


program : Parser -> Result Parser Str
program = \parser ->
    # dbg parser.result
    go = \term -> parser |> add term |> program
    when parser.tokens is
        [] -> Ok parser
        [term, ..] ->
            when term is
                _ -> Err "in program"
                # "]" -> Ok parser
                # "[" ->
                #     when program { parser & index: parser.index + 1 } is
                #         Err msg -> Err msg
                #         Ok p -> program { parser & index: p.index, result: List.append parser.result (Quote p.result) }
                # "dup" -> go Dup
                # "swap" -> go Swap
                # "dig" -> go Dig
                # "+" -> go Add
                # "*" -> go Multiply
                # x ->
                #     when Str.toI64 x is
                #         Ok num -> Number num |> go
                #         Err _ -> Err "I don't recognize '\(x)'"

add : Parser, Term -> Parser
add = \parser, term ->
    { parser & index: parser.index + 1, result: List.append parser.result term }

lex : Str -> List Str
lex = \input ->
    input
    |> Str.replaceEach "[" "[ "
    |> Str.replaceEach "]" " ]"
    |> Str.replaceEach "\n" " "
    |> Str.replaceEach "\t" " "
    |> Str.split " "

expect lex "345 copy swap [swap dup 4]\ncopy" == ["345", "copy", "swap", "[", "swap", "dup", "4", "]", "copy"]

expect 1 == 1


