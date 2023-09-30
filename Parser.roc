interface Parser
    exposes [parse, Program]
    imports []

Program : List Term
Term : [Number U32, Add, Multiply, Dup, Swap, Quote]

Parser : {
    tokens : List Str,
    index : Nat,
    result : Program,
}

parse : Str -> Result Program Str
parse = \file ->
    program {
        tokens: Str.graphemes file,
        index: 0,
        result: [],
    }

program : Parser -> Result Program Str
program = \parser ->
    dbg
        T parser.index parser.result

    if
        parser.index == List.len parser.tokens
    then
        Ok parser.result
    else if
        matches parser "dup"
    then
        advance parser Dup 3 |> program
    else if
        matches parser "swap"
    then
        advance parser Swap 4 |> program
    else
        Err "err"

matches : Parser, Str -> Bool
matches = \parser, token ->
    gs = Str.graphemes token
    List.sublist parser.tokens { start: parser.index, len: List.len gs } == gs

advance : Parser, Term, Nat -> Parser
advance = \parser, term, len ->
    { parser & index: parser.index + len, result: List.append parser.result term }

expect
    parse "dup" == Ok [Dup]

