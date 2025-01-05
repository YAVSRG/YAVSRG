namespace Prelude.Skins.HudLayouts

open System.IO
open System.IO.Compression
open Prelude.Skins

type HudLayout(storage) as this =
    inherit Storage(storage)

    let mutable config: HudConfig = HudConfig.Default

    do
        this.ReloadFromDisk()

    member this.Config
        with set conf =
            config <- conf
            this.WriteJson(config, "hud.json")
        and get () = config

    override this.ReloadFromDisk() =
        base.ReloadFromDisk()
        config <-
            match this.TryGetJson<HudConfig>(true, "hud.json") with
            | Some data -> data // todo: data.Validate method
            | _ -> failwith "hud.json was missing or didn't load properly"

    member this.GetTexture(name: string) : TextureLoadResult =
        this.LoadTexture(name, HudTextureRules.get this.Config name)

    member this.RequiredTextures =
        HudTextureRules.list ()
        |> Seq.filter (HudTextureRules.get this.Config >> _.IsRequired)

    member this.Validate() : ValidationMessage seq =
        seq {
            for texture_id in HudTextureRules.list () do
                yield! this.ValidateTexture(texture_id, HudTextureRules.get this.Config texture_id)
        }

    static member FromZipStream(stream: Stream) =
        new HudLayout(Embedded(new ZipArchive(stream)))

    static member FromPath(path: string) =
        try
            new HudLayout(Folder path)
            |> Ok
        with err -> Error err

    static member Exists(path: string) =
        Directory.Exists path && File.Exists (Path.Combine(path, "hud.json"))