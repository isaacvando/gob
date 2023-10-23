interface Parser
    exposes [parse, Program, Stack, Term]
    imports [
        parser.Core.{ Parser },
        parser.String.{ RawStr },
    ]

Program : {
    defs : Dict Str (List Term),
    body : List Term,
}
Stack : List Term
Term : [
    Number Nat,
    String Str,
    Quotation (List Term),
    Builtin Str,
    Def Str,
]

parse : Str -> Result Program Str
parse = \input ->
    when String.parseStr program (clean input) is
        Err (ParsingFailure msg) -> Err msg
        Err (ParsingIncomplete remaining) -> Err "I wasn't able to parse all of the input. What I had left was:\n \(remaining)"
        Ok p -> Ok p

clean : Str -> Str
clean = \input ->
    isComment = \line -> Str.trimStart line
        |> Str.startsWithScalar '#'

    Str.split input "\n"
    |> List.dropIf isComment
    |> List.dropIf isBlank
    |> Str.joinWith "\n"

isBlank : Str -> Bool
isBlank = \str ->
    Str.toUtf8 str
    |> List.all isWhitespace

# program : Parser RawStr Program
program =
    toProgram = \d -> \b ->
            { defs: Dict.fromList d, body: b }
    Core.const toProgram
    |> Core.keep (Core.many def)
    |> Core.keep terms

def =
    block =
        Core.const (\x -> x)
        |> Core.skip hWhitespace
        |> Core.keep term

    d =
        Core.const (\x -> x)
        |> Core.keep identifier
        |> Core.skip (String.scalar ':')

    Core.const (\name -> \ts -> (name, ts))
    |> Core.keep d
    |> Core.skip hWhitespace
    |> Core.keep (Core.many block)
    |> Core.skip hWhitespace
    |> Core.skip (String.scalar '\n')

hTerms =
    block =
        Core.const (\x -> x)
        |> Core.skip hWhitespace
        |> Core.keep term

    Core.const (\x -> x)
    |> Core.keep (Core.many block)
    |> Core.skip hWhitespace

expect
    String.parseStr hTerms "\t   false + -  []  "
    == Ok [Builtin "false", Builtin "+", Builtin "-", Quotation []]

identifier =
    check = \str ->
        converted =
            when Str.fromUtf8 str is
                Ok s -> s
                Err _ -> crash "utf8 conversion error"

        if
            Str.isEmpty converted
        then
            Err "identifiers must contain at least one character"
        else if
            List.contains reserved converted
        then
            Err "'\(converted)' is a reserved word"
        else
            Ok converted

    isAlphaNumeric = \c -> List.contains alphaNumeric c

    Core.chompWhile isAlphaNumeric
    |> Core.map check
    |> Core.flatten

expect String.parseStr identifier "foo78" == Ok "foo78"
expect String.parseStr identifier "foo_" |> Result.isErr
expect String.parseStr identifier "" |> Result.isErr

# alphaNumeric : List U8
alphaNumeric =
    digits = List.range { start: At 48, end: At 57 }
    caps = List.range { start: At 65, end: At 90 }
    lowers = List.range { start: At 97, end: At 122 }
    List.join [digits, caps, lowers]

terms =
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
        quotation,
        string,
    ]
    List.concat builtins otherTerms
    |> Core.oneOf
    |> Core.alt (identifier |> Core.map Def)

expect String.parseStr term "quote" == Ok (Builtin "quote")
expect String.parseStr term "true" == Ok (Builtin "true")
expect String.parseStr term "789" == Ok (Number 789)
expect String.parseStr term "[dup]" == Ok (Quotation [Builtin "dup"])
expect String.parseStr term "[dup" |> Result.isErr
expect String.parseStr term "unknown" == Ok (Def "unknown")

builtins =
    toParser = \s -> String.string s |> Core.map Builtin
    reserved |> List.map toParser

reserved : List Str
reserved = [
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

number =
    String.digits |> Core.map Number

expect String.parseStr number "12345" == Ok (Number 12345)
expect String.parseStr number "aldkfadlkfj" |> Result.isErr

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

expect
    String.parseStr string "\"a string\"" == Ok (String "a string")

# quote : Parser RawStr Term
quotation =
    Core.buildPrimitiveParser \input ->
        Core.parsePartial
            (
                Core.between hTerms (String.scalar '[') (String.scalar ']')
                |> Core.map (\list -> Quotation list)
            )
            input
expect
    String.parseStr quotation "[]"
    == Ok (Quotation [])
expect
    String.parseStr quotation "foo" |> Result.isErr
expect
    out = String.parseStr quotation "[true]"
    out == Ok (Quotation [Builtin "true"])
expect
    String.parseStr quotation "[ false 7 + - [] ]"
    == Ok (Quotation [Builtin "false", Number 7, Builtin "+", Builtin "-", Quotation []])

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

expect String.parseStr whitespace " \t\n" == Ok [' ', '\t', '\n']
expect String.parseStr whitespace "  \nfoo" |> Result.isErr

isHWhitespace = \char ->
    when char is
        ' ' -> Bool.true
        '\t' -> Bool.true
        _ -> Bool.false

hWhitespace =
    Core.chompWhile isHWhitespace

expect String.parseStr hWhitespace " \t" == Ok [' ', '\t']
expect String.parseStr hWhitespace "  \n" |> Result.isErr
