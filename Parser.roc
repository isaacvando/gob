interface Parser
    exposes [parse, Program, Stack, Term]
    imports [
        parser.Core.{ Parser },
        parser.String.{ RawStr },
    ]

Program : List Term
Stack : Program
Term : [
    Number I64,
    String Str,
    Add,
    Multiply,
    Dup,
    Swap,
    Dig,
    Quote (List Term),
]

# parse : Str -> Result Program Str
parse = \input ->
    when String.parseStr program input is
        Err (ParsingFailure msg) -> Err msg
        Err (ParsingIncomplete remaining) -> Err "I wasn't able to parse all of the input. What I had left was: \(remaining)"
        Ok p -> Ok p

# program : Parser RawStr Program
program =
    block =
        Core.const (\x -> x)
        |> Core.skip whitespace
        |> Core.keep term

    Core.const (\x -> x)
        |> Core.keep (Core.many block)
        |> Core.skip whitespace

# term : Parser RawStr Term
term =
    keywords = [("dup", Dup), ("swap", Swap), ("dig", Dig)] |> List.map keyword
    List.append keywords quote |> Core.oneOf

# quote : Parser RawStr Term
quote =
    Core.buildPrimitiveParser (\input -> Core.parsePartial (Core.between program (String.scalar '[') (String.scalar ']')
        |> Core.map (\list -> Quote list)) input)

# keyword : (Str, Term) -> Parser RawStr Term
keyword = \(name, tag) ->
    String.string name
    |> Core.map \_ -> tag

# isWhitespace : U8 -> Bool
isWhitespace = \char ->
    when char is
        ' ' -> Bool.true
        '\n' -> Bool.true
        '\t' -> Bool.true
        '\r' -> Bool.true
        _ -> Bool.false

# whitespace : Parser (List U8) (List U8)
whitespace =
    Core.chompWhile isWhitespace
