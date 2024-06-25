namespace Prelude.Skinning.HudLayouts

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude
open Prelude.Skinning

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

    member this.TextureFileMode(name: string) = this.GetTextureConfig(name).Mode

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

    /// Call when no HUD loaded successfully
    static member CreateDefault(path: string) : HudLayout =
        if not (Directory.Exists path) then
            Directory.CreateDirectory path |> ignore

        let config = Path.Combine(path, "hud.json")
        if File.Exists config then
            match JSON.FromFile(config) with
            | Ok data -> Logging.Warn(sprintf "CreateDefault should not have been called with a working HUD already at %s" path)
            | Error _ -> 

            Logging.Critical("Your default HUD's hud.json doesn't parse! Did you make a typo?\nIn future, use the ingame editor to avoid formatting mistakes.")
            Logging.Critical("If you want to FULLY reset your HUD file to defaults, type 'reset' now, otherwise go and fix it and then relaunch the game.")

            if System.Console.ReadLine().Trim().ToLower() <> "reset" then
                failwith "User chose to crash the game so they can fix their HUD config"
            
        JSON.ToFile(config, true) HudConfig.Default
        match HudLayout.FromPath path with
        | Ok hud -> hud
        | Error err -> 
            Logging.Critical("Something terrible has happened while creating default HUD", err)
            raise err