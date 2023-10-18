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
    Quotation (List Term),
    Builtin Str
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
    List.concat builtins otherTerms |> Core.oneOf

builtins =
    toParser = \s -> String.string s |> Core.map Builtin
    [
        "dup", 
        "swap",
        "dig",
        "+", 
        "-", 
        "*", 
        "=", 
        "apply", 
        "repeat", 
        "compose", 
        "quote", 
        "drop", 
        "branch", 
        "true", 
        "false", 
    ]
    |> List.map toParser

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
                    |> Core.map (\list -> Quotation list)
                )
                input)

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
