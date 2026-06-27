namespace Interlude.Resources

open System
open System.Reflection
open System.IO

type EmbeddedResource =

    static member GetStream(name: string) : Stream =
        Assembly
            .GetExecutingAssembly()
            .GetManifestResourceStream("Interlude.Resources." + name)

    static member GetLocale (id: string) : Stream =
        EmbeddedResource.GetStream("Locale." + id + ".txt")

    static member GetText (name: string) : string =
        use s = EmbeddedResource.GetStream(name)
        use tr = new StreamReader(s)
        tr.ReadToEnd()

    static member SplashMessageGenerator (name: string) : unit -> string =
        let text = EmbeddedResource.GetText(name).Trim()
        let lines = text.Split("\n")

        fun () -> lines.[Random.Shared.Next lines.Length]