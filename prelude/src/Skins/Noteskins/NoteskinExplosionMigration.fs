namespace Prelude.Skins.Noteskins

open System.IO
open System.IO.Compression
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Skins

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
    type NoteskinMigrationModel = { Name: string; Explosions: Explosions option }

    let patch (model: NoteskinMigrationModel) (config: NoteskinConfig) : NoteskinConfig =
        match model.Explosions with
        | Some explosions ->
            Logging.Info "Migrating noteskin '%s' to new explosions system" model.Name

            if explosions.ExplodeOnMiss then
                Logging.Warn(
                    "Explosions on miss is no longer supported, complain to me if you were using it and want it back"
                )

            { config with
                UseExplosions = explosions.Enable |> Option.defaultValue true
                NoteExplosionSettings =
                    {
                        AnimationFrameTime = float32 explosions.AnimationFrameTime * 1.0f<ms / rate>
                        UseBuiltInAnimation = true
                        Scale = explosions.Scale
                        Offset = 0.0f
                        ExpandAmount = explosions.ExpandAmount
                        Colors =
                            match explosions.Colors with
                            | ExplosionColorsOld.Column -> ExplosionColors.Note
                            | ExplosionColorsOld.Judgements -> ExplosionColors.Judgements
                        Duration = 300.0f<ms / rate>
                    }
                HoldExplosionSettings =
                    {
                        AnimationFrameTime = float32 explosions.AnimationFrameTime * 1.0f<ms / rate>
                        ReleaseUseBuiltInAnimation = true
                        UseReleaseExplosion = false
                        Scale = explosions.Scale
                        Offset = 0.0f
                        ExpandAmount = explosions.ExpandAmount
                        Colors =
                            match explosions.Colors with
                            | ExplosionColorsOld.Column -> ExplosionColors.Note
                            | ExplosionColorsOld.Judgements -> ExplosionColors.Judgements
                        Duration = 300.0f<ms / rate>
                    }
            }
        | None -> config
