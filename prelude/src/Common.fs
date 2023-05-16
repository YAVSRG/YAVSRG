namespace Prelude

open System
open System.IO
open SixLabors.ImageSharp
open System.Drawing
open System.Collections.Generic
open Percyqaz.Json
open Percyqaz.Common

[<AutoOpen>]
module Common =

    #if DEBUG
    let DEV_MODE = true
    #else
    let DEV_MODE = false
    #endif

    type SettingCodec<'T, 'Config>() =
        inherit Json.Codec<Setting<'T, 'Config>>()
        override this.To (ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()
            fun o -> cdc.To o.Value
        override this.From (ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()
            fun o json -> o.Value <- cdc.From o.Value json; o
        override this.Default (ctx: Json.Context) =
            let cdc = ctx.GetCodec<'T>()
            fun () ->
                let v = cdc.Default()
                { Set = ignore; Get = K v; Config = Unchecked.defaultof<_> }

    type ColorCodec() =
        inherit Json.Codec<Color>()
        override this.To (ctx: Json.Context) =
            let cdc = ctx.GetCodec<byte * byte * byte * byte>()
            fun col -> cdc.To (col.A, col.R, col.G, col.B)
        override this.From (ctx: Json.Context) =
            let cdc = ctx.GetCodec<byte * byte * byte * byte>()
            fun _ json -> 
                let (a, r, g, b) = cdc.FromDefault json
                Color.FromArgb(int a, int r, int g, int b)
        override this.Default (ctx: Json.Context) =
            fun () -> Color.White

    type ConcurrentDictionaryCodec<'K, 'V when 'K : equality>() =
        inherit Json.Codec<Collections.Concurrent.ConcurrentDictionary<'K, 'V>>()
        override this.To (ctx: Json.Context) =
            let cdc = ctx.GetCodec<Dictionary<'K, 'V>>()
            fun cd -> cdc.To (Dictionary cd)
        override this.From (ctx: Json.Context) =
            let cdc = ctx.GetCodec<Dictionary<'K, 'V>>()
            fun _ json -> Collections.Concurrent.ConcurrentDictionary(cdc.FromDefault json)
        override this.Default (ctx: Json.Context) =
            fun () -> Collections.Concurrent.ConcurrentDictionary()

    module Setting =
        open Percyqaz.Common.Setting
        let rate x = bounded x 0.5f 2.0f |> roundf 2

(*
    Localisation
*)

    module Localisation =
        let private mapping = new Dictionary<string, string>()
        let mutable private loadedPath = ""

        let loadFile path =
            let path = Path.Combine ("Locale", path)
            try
                let lines = File.ReadAllLines path
                Array.iter(
                    fun (l: string) ->
                        let s: string[] = l.Split ([|'='|], 2)
                        mapping.Add (s.[0], s.[1].Replace("\\n","\n"))
                    ) lines
                loadedPath <- Path.GetFullPath path
            with err -> Logging.Critical ("Failed to load localisation file: " + path, err)

        let localise str : string =
            if mapping.ContainsKey str then mapping.[str]
            else
                mapping.Add (str, str)
                if DEV_MODE && loadedPath <> "" then File.AppendAllText (loadedPath, "\n"+str+"="+str)
                str

        let localiseWith xs str =
            let mutable s = localise str
            List.iteri (fun i x -> s <- s.Replace ("%" + i.ToString(), x)) xs
            s

(*
    Misc helpers (mostly data storage)
*)
    
    let JSON =
        Json(Json.Settings.Default)
            .WithDefaults()
            .WithCodec<SettingCodec<_,_>>()
            .WithCodec<ColorCodec>()
            .WithCodec<ConcurrentDictionaryCodec<_,_>>()

    type Bitmap = Image<PixelFormats.Rgba32>
    module Bitmap =
        let load (stream: Stream) : Bitmap = Bitmap.Load<PixelFormats.Rgba32> stream
    type Color = Drawing.Color


    let getDataPath name =
        let p = Path.Combine(Directory.GetCurrentDirectory(), name)
        Directory.CreateDirectory p |> ignore
        p

    let loadImportantJsonFile<'T> name path prompt =
        if File.Exists path then
            match JSON.FromFile path with
            | Ok data -> data
            | Error err ->
                Logging.Critical (sprintf "Could not load %s! Maybe it is corrupt?" (Path.GetFileName path), err)
                if prompt then
                    Logging.Critical "If you would like to launch anyway (WILL WIPE THIS DATA!!), press ENTER."
                    Logging.Critical "If you would like to try and fix the problem youself, CLOSE THIS WINDOW NOW."
                    Console.ReadLine() |> ignore
                    Logging.Critical "User has chosen to launch game with default data."
                JSON.Default<'T>()
        else
            Logging.Info (sprintf "No %s file found, creating it." name)
            JSON.Default<'T>()

    let saveImportantJsonFile<'T> path (data: 'T) =
        let write = Path.ChangeExtension (path, ".new")
        let bak = Path.ChangeExtension (path, ".bak")
        let bak2 = Path.ChangeExtension (path, ".bak2")
        JSON.ToFile (write, true) data
        if File.Exists bak2 then File.Delete bak2
        if File.Exists bak then File.Move (bak, bak2)
        if File.Exists path then File.Move (path, bak)
        File.Move (write, path)