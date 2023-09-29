interface Parser
    exposes [parse, Quote]
    imports []

Quote : List [Number U32, Add, Multiply, Copy, Swap, Quote]

Parser : {
    tokens : List Str,
    index : U32,
    result : Quote,
    error : Str,
}

parse : Str -> Result Quote Str
parse = \file ->
    parser = {
        tokens: Str.graphemes file,
        index: 0,
        result: [],
        error: "",
    }
    parseResult = quote parser
    if
        parseResult.error != ""
    then
        Err parseResult.error
    else
        Ok parseResult.result

quote : Parser -> Parser
quote = \parser -> 
  when parser.tokens is
      [] -> parser

