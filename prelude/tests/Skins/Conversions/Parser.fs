namespace Prelude.Tests.Skins.Conversions

open NUnit.Framework
open System.IO
open Prelude.Skins.Conversions.Osu

module SkinIniParser =

    let string_to_stream (s: string) =
        let ms = new MemoryStream()
        let sw = new StreamWriter(ms)
        sw.Write s
        sw.Flush()
        ms.Position <- 0
        ms

    [<Test>]
    let EmptyIni () =
        let result =
            """
            """
            |> string_to_stream
            |> SkinIni.FromStream

        printfn "%A" result

        Assert.True(Result.isOk result)

    [<Test>]
    let SampleIni () =
        let result = SkinIni.FromFile("./Data/skin.ini")

        printfn "%A" result

        Assert.True(Result.isOk result)