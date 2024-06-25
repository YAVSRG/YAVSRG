namespace Prelude.Skinning.Noteskins

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Percyqaz.Data
open Prelude.Skinning

module NoteskinExplosionMigration =

    [<RequireQualifiedAccess>]
    [<Json.AutoCodec>]
    type ExplosionColorsOld =
        | Column
        | Judgements

    [<Json.AutoCodec(false)>]
    type Explosions =
        {
            Enable: bool option
            Scale: float32
            FadeTime: float32
            ExpandAmount: float32
            ExplodeOnMiss: bool
            AnimationFrameTime: float
            Colors: ExplosionColorsOld
        }

    [<Json.AutoCodec(false)>]
    type NoteskinMigrationModel = { Explosions: Explosions option }

    let patch (model: NoteskinMigrationModel) (config: NoteskinConfig) =
        match model.Explosions with
        | Some explosions ->
            Logging.Info(sprintf "Migrating noteskin '%s' to new explosions system" config.Name)

            if explosions.ExplodeOnMiss then
                Logging.Warn(
                    "Explosions on miss is no longer supported, complain to me if you were using it and want it back"
                )

            { config with
                UseExplosions = explosions.Enable |> Option.defaultValue true
                NoteExplosionSettings =
                    {
                        AnimationFrameTime = explosions.AnimationFrameTime
                        UseBuiltInAnimation = true
                        Scale = explosions.Scale
                        Offset = 0.0f
                        ExpandAmount = explosions.ExpandAmount
                        Colors =
                            match explosions.Colors with
                            | ExplosionColorsOld.Column -> ExplosionColors.Note
                            | ExplosionColorsOld.Judgements -> ExplosionColors.Judgements
                        Duration = 300.0
                    }
                HoldExplosionSettings =
                    {
                        AnimationFrameTime = explosions.AnimationFrameTime
                        ReleaseUseBuiltInAnimation = true
                        UseReleaseExplosion = false
                        Scale = explosions.Scale
                        Offset = 0.0f
                        ExpandAmount = explosions.ExpandAmount
                        Colors =
                            match explosions.Colors with
                            | ExplosionColorsOld.Column -> ExplosionColors.Note
                            | ExplosionColorsOld.Judgements -> ExplosionColors.Judgements
                        Duration = 300.0
                    }
            }
        | None -> config

type Noteskin(storage) as this =
    inherit Storage(storage)

    let mutable config: NoteskinConfig = NoteskinConfig.Default

    do
        this.ReloadFromDisk()

    member this.Config
        with set conf =
            config <- conf
            this.WriteJson(config, "noteskin.json")
        and get () = config

    override this.ReloadFromDisk() =
        base.ReloadFromDisk()
        config <-
            match
                this.TryGetJson<NoteskinExplosionMigration.NoteskinMigrationModel>(false, "noteskin.json"),
                this.TryGetJson<NoteskinConfig>(true, "noteskin.json")
            with
            | Some migration_patch, Some data -> NoteskinExplosionMigration.patch migration_patch data.Validate
            | _ -> failwith "noteskin.json was missing or didn't load properly"

    member this.GetTexture(name: string) : TextureLoadResult =
        this.LoadTexture(name, NoteskinTextureRules.get this.Config name)

    member this.TextureFileMode(name: string) = this.GetTextureConfig(name).Mode

    member this.RequiredTextures =
        NoteskinTextureRules.list ()
        |> Seq.filter (NoteskinTextureRules.get this.Config >> _.IsRequired)

    member this.Validate() : ValidationMessage seq =
        seq {
            for texture_id in NoteskinTextureRules.list () do
                yield! this.ValidateTexture(texture_id, NoteskinTextureRules.get this.Config texture_id)
        }
        
    static member FromZipStream(stream: Stream) =
        new Noteskin(Embedded(new ZipArchive(stream)))

    static member FromPath(path: string) =
        try
            new Noteskin(Folder path)
            |> Ok
        with err -> Error err

    static member Exists(path: string) =
        Directory.Exists path && File.Exists (Path.Combine(path, "noteskin.json"))
