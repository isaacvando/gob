interface Parser
    exposes [parse, Program]
    imports []

Program : List [Number U32, Add, Multiply, Copy, Swap, Quote]

Parser : {
    tokens : List Str,
    index: Nat,
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
    if
        parser.index + 1 == List.len parser.tokens
    then
        Ok parser.result
    else if matches parser "copy" 
        then Ok [Copy]
        else Err "err"




matches : Parser, Str -> Bool
matches = \parser, token ->
    gs = Str.graphemes token
    List.sublist parser.tokens {start: parser.index, len: List.len gs} == gs
