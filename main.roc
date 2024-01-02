app "gob"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Task
    ]
    provides [main] to pf

main =
    Task.ok {}

Term : [
    Number I64,
    Quotation (List Term),
]

stepBuiltin : List Term -> List Term
stepBuiltin = \stack ->
    when stack is
        [.., Number x, Number y] ->
            # dbg stack
            [Number (x + y)]

        _ -> []

expect
    result = stepBuiltin [Number 4, Number 8]
    result == [Number 12]
