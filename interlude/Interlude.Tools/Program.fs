open System
open Interlude.Tools

[<EntryPoint>]
let main argv =
    printfn "%s" Utils.YAVSRG_PATH

    printfn "Press enter to bundle assets"
    Console.ReadLine() |> ignore
    Features.Bundle_Assets.main()

    0
