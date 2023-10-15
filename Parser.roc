interface Parser
    exposes [parse, Program, Stack, Term]
    imports [
        parser.Core.{ Parser },
        parser.String.{ RawStr },
    ]

Program : List Term
Stack : Program
Term : [
    Number Nat,
    String Str,
    True,
    False,
    Add,
    Subtract,
    Multiply,
    Equals,
    Dup,
    Swap,
    Dig,
    Quote (List Term),
    Apply,
    Repeat,
    Compose,
    If,
]

# parse : Str -> Result Program Str
parse = \input ->
    when String.parseStr program (stripComments input) is
        Err (ParsingFailure msg) -> Err msg
        Err (ParsingIncomplete remaining) -> Err "I wasn't able to parse all of the input. What I had left was:\n \(remaining)"
        Ok p -> Ok p

stripComments : Str -> Str
stripComments = \input ->
    isComment = \line -> Str.trimStart line
        |> Str.startsWithScalar '#'

    Str.split input "\n"
    |> List.dropIf isComment
    |> Str.joinWith "\n"

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
    otherTerms = [
        number,
        quote,
        string,
    ]
    List.concat keywords otherTerms |> Core.oneOf

keywords =
    [
        ("dup", Dup),
        ("swap", Swap),
        ("dig", Dig),
        ("+", Add),
        ("-", Subtract),
        ("*", Multiply),
        ("=", Equals),
        ("apply", Apply),
        ("repeat", Repeat),
        ("compose", Compose),
        ("if", If),
        ("true", True),
        ("false", False),
    ]
    |> List.map keyword

number =
    String.digits |> Core.map Number

string =
    toStr = \list ->
        when Str.fromUtf8 list is
            Ok s -> String s
            Err _ -> crash "TODO: don't crash here"

    isNotQuote = \c -> c != '"'

    Core.const toStr
    |> Core.skip (String.string "\"")
    |> Core.keep (Core.chompWhile isNotQuote)
    |> Core.skip (String.string "\"")

# quote : Parser RawStr Term
quote =
    Core.buildPrimitiveParser
        (\input -> Core.parsePartial
                (
                    Core.between program (String.scalar '[') (String.scalar ']')
                    |> Core.map (\list -> Quote list)
                )
                input)

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
