interface Parser
    exposes [parse, Program]
    imports []

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
        tokens: lex input,
        index: 0,
        result: [],
    }
    when result is
        Err x -> Err x
        Ok p -> Ok p.result


program : Parser -> Result Parser Str
program = \parser ->
    go = \term -> parser |> add term |> program
    when List.get parser.tokens parser.index is
        Err _ -> Ok parser
        Ok term ->
            when term is
                "dup" -> go Dup
                "swap" -> go Swap
                "dig" -> go Dig
                "+" -> go Add
                "*" -> go Multiply
                x ->
                    when Str.toI64 x is
                        Ok num -> Number num |> go
                        Err _ -> Err "I don't recognize '\(x)'"

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

