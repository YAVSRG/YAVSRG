open System
open Interlude.Tools

[<EntryPoint>]
let main argv =
    printfn "%s" Utils.YAVSRG_PATH

    printfn "Press enter to run scripts"
    Console.ReadLine() |> ignore
    //Features.Asset_Script.main()
    Features.Bundle_Assets.main()

    0
