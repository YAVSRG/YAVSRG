namespace YAVSRG.CLI

open System.IO

module Test =

    let run () : bool =
        if Shell.Exec("dotnet", "build") <> 0 then
            printfn "Everything must build!"
            false
        elif
            Shell
                .At(Path.Combine(YAVSRG_PATH, "prelude", "tests"))
                .Exec("dotnet", "test --no-build -- NUnit.ConsoleOut=0")
            <> 0
        then
            printfn "Prelude unit tests must pass!"
            false
        elif
            Shell
                .At(Path.Combine(YAVSRG_PATH, "online", "tests", "domain"))
                .Exec("dotnet", "test --no-build -- NUnit.ConsoleOut=0")
            <> 0
        then
            printfn "Server unit tests must pass!"
            false
        else
            true
