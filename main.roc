app "main"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.5.0/Cufzl36_SnJ4QbOoEmiJ5dIpUxBvdB3NEySvuH82Wio.tar.br" }
    imports [
        pf.Stdout,
        pf.Task.{ Task },
        pf.File,
        pf.Arg,
        pf.Path
    ]
    provides [main] to pf

main : Task {} I32
main = 
    args <- Arg.list |> Task.await
    when List.get args 1 is 
        Err _ -> Stdout.line "I couldn't find any command line arguments. Please try again with the path to a stack program."
        Ok arg -> 
            file <- Path.fromStr arg 
                |> File.readUtf8 
                |> Task.mapErr \_ -> 1
                |> Task.await
            Stdout.line file
            # Stdout.line "foobar"

