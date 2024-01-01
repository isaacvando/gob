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
    Number I64,
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

# Remove comments and blank space
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
    |> List.all \x -> 
        List.contains [' ', '\t', '\n'] x

program =
    toProgram = \d -> \b ->
            { defs: Dict.fromList d, body: b }
    Core.const toProgram
    |> Core.keep (Core.many def)
    |> Core.keep body

expect
    input = 
        """
        def1: "foo"
        def2: def1 10
        10 5 repeat
        """
    d = Dict.fromList [("def1", [String "foo"]), ("def2", [Def "def1", Number 10])]
    b = [Number 10, Number 5, Builtin "repeat"]

    result = String.parseStr program input
    result == Ok { defs: d, body: b }

body = terms (Core.oneOrMore space)

expect 
    result = String.parseStr body "-10 + 10"
    result == Ok [Number -10, Builtin "+", Number 10]

expect 
    result = String.parseStr body "- 10 + 10"
    result == Ok [Builtin "-", Number 10, Builtin "+", Number 10]

def =
    defName =
        Core.const (\x -> x)
        |> Core.keep identifier
        |> Core.skip (String.scalar ':')

    Core.const (\name -> \ts -> (name, ts))
    |> Core.keep defName
    |> Core.skip (Core.many hSpace)
    |> Core.keep (terms (Core.oneOrMore hSpace))
    |> Core.skip (Core.many hSpace)
    |> Core.skip (String.scalar '\n')

expect 
    result = String.parseStr def "name: \"foo\" + dup\n"
    result == Ok ("name", [String "foo", Builtin "+", Builtin "dup"])

expect
    result = String.parseStr (terms (Core.many hSpace)) "false    \t  + -  []  "
    result == Ok [Builtin "false", Builtin "+", Builtin "-", Quotation []]

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

alphaNumeric =
    digits = List.range { start: At 48, end: At 57 }
    caps = List.range { start: At 65, end: At 90 }
    lowers = List.range { start: At 97, end: At 122 }
    List.join [digits, caps, lowers]

terms = \spacer -> 
    Core.sepBy term spacer



term =
    [number, quotation, string] 
    |> List.concat builtins
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
    "split",
]

number =
    positive = String.digits |> Core.map \n -> Num.toI64 n |> Number
    negative = 
        convert = \n -> Num.toI64 n * -1 |> Number
        Core.const convert
            |> Core.skip (String.scalar '-')
            |> Core.keep String.digits

    Core.alt positive negative

expect String.parseStr number "12345" == Ok (Number 12345)
expect String.parseStr number "-567" == Ok (Number -567)
expect String.parseStr number "aldkfadlkfj" |> Result.isErr

string =
    toStr = \list ->
        when Str.fromUtf8 list is
            Ok s -> String s
            # TODO: make this use Core.fail instead of crashing
            Err _ -> crash "I was unable to convert the contents of a string as utf8"

    isNotQuote = \c -> c != '"'

    Core.const toStr
    |> Core.skip (String.string "\"")
    |> Core.keep (Core.chompWhile isNotQuote)
    |> Core.skip (String.string "\"")



expect
    String.parseStr string "\"a string\"" == Ok (String "a string")

# buildPrimitiveParser is used here as a work around for a bug that sometimes comes up with recursive parsers such as this
# https://roc.zulipchat.com/#narrow/stream/231634-beginners/topic/Compiler.20stack.20overflow.20on.20recursive.20parser/near/377685052
quotation =
    bracket = \b -> 
        Core.const (\_ -> {})
        |> Core.keep (String.scalar b)
        |> Core.skip (Core.many hSpace)

    Core.buildPrimitiveParser \input ->
        Core.parsePartial
            (
                Core.between (terms (Core.oneOrMore hSpace)) (bracket '[') (bracket ']')
                |> Core.map (\list -> Quotation list)
            )
            input
expect
    result = String.parseStr quotation "[]"
    result == Ok (Quotation [])
expect
    String.parseStr quotation "foo" |> Result.isErr
expect
    out = String.parseStr quotation "[true]"
    out == Ok (Quotation [Builtin "true"])
expect
    result = String.parseStr quotation "[ false 7 + - [] ]"
    result == Ok (Quotation [Builtin "false", Number 7, Builtin "+", Builtin "-", Quotation []])

space =
    [' ', '\t', '\n']
    |> List.map String.scalar
    |> Core.oneOf

hSpace = 
    [' ', '\t']
    |> List.map String.scalar
    |> Core.oneOf

expect String.parseStr space "\n" |> Result.isOk
expect String.parseStr hSpace "\n" |> Result.isErr
