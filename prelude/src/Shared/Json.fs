namespace Prelude

open System.IO
open System.Drawing
open Percyqaz.Data
open Percyqaz.Common

type private SettingCodec<'T, 'Config>() =
    inherit Json.Codec<Setting<'T, 'Config>>()

    override this.To(ctx: Json.Context) =
        let cdc = ctx.GetCodec<'T>()
        fun o -> cdc.To o.Value

    override this.From(ctx: Json.Context) =
        let cdc = ctx.GetCodec<'T>()

        fun o json ->
            o.Value <- cdc.From o.Value json
            o

    override this.Default(ctx: Json.Context) =
        let cdc = ctx.GetCodec<'T>()

        fun () ->
            let v = cdc.Default()

            {
                Set = ignore
                Get = K v
                Config = Unchecked.defaultof<_>
            }

type private ColorCodec() =
    inherit Json.Codec<Color>()

    override this.To(ctx: Json.Context) =
        let cdc = ctx.GetCodec<byte * byte * byte * byte>()
        fun col -> cdc.To(col.A, col.R, col.G, col.B)

    override this.From(ctx: Json.Context) =
        let cdc = ctx.GetCodec<byte * byte * byte * byte>()

        fun _ json ->
            let a, r, g, b = cdc.FromDefault json
            Color.FromArgb(int a, int r, int g, int b)

    override this.Default(_: Json.Context) = fun () -> Color.White

module [<AutoOpen>] Json =

    let JSON =
        Json(Json.Settings.Default)
            .WithDefaults()
            .WithCodec<SettingCodec<_, _>>()
            .WithCodec<ColorCodec>()

    let load_important_json_file<'T> (name: string) (path: string) (prompt: bool) : 'T =
        if File.Exists path then
            match JSON.FromFile path with
            | Ok data -> data
            | Error err ->
                Logging.Critical "Could not load '%s'! Maybe it is corrupt?\n%O" (Path.GetFileName path) err

                let backup = Path.ChangeExtension(path, ".bak")
                match JSON.FromFile backup with
                | Ok data ->
                    Logging.Info "Loading with backup instead"
                    data
                | Error backup_err ->
                    Logging.Critical "Could not load '%s' either!\n%O" (Path.GetFileName backup) backup_err

                    if prompt then
                        Logging.Critical "This is likely a typo from manually editing the JSON yourself."
                        Logging.Critical "Please correct the typo, or if you can't, delete the file and a fresh new one will be created."

                        failwithf "Unable to parse JSON in %s" path

                    JSON.Default<'T>()
        else
            Logging.Info "No %s file found, creating it." name
            JSON.Default<'T>()

    let save_important_json_file<'T> (path: string) (data: 'T) =
        let write = Path.ChangeExtension(path, ".new")
        let bak = Path.ChangeExtension(path, ".bak")
        let bak2 = Path.ChangeExtension(path, ".bak2")
        JSON.ToFile (write, true) data

        if File.Exists bak2 then
            File.Delete bak2

        if File.Exists bak then
            File.Move(bak, bak2)

        if File.Exists path then
            File.Move(path, bak)

        File.Move(write, path)