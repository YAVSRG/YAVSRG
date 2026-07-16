namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Skins
open Prelude.Skins.Conversions.Osu

module internal StageConverter =
    
    let convert_stage_textures(ctx: NoteskinConverterContext) : unit =
        try
            let stage_left =
                TextureSearchResult.Create(ctx.KeymodeSettings.StageLeft, ctx.DefaultSettings.StageLeft, ctx.FileSystem)
                    .ThrowIfNotFound()
                    .Load(ctx.FileSystem)
                    .Image
            let stage_right =
                TextureSearchResult.Create(ctx.KeymodeSettings.StageRight, ctx.DefaultSettings.StageRight, ctx.FileSystem)
                    .ThrowIfNotFound()
                    .Load(ctx.FileSystem)
                    .Image

            stage_left.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "stageleft" (0, 0)))
            stage_right.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "stageright" (0, 0)))
            ctx.StageTextures <- true
        with err ->
            Logging.Warn "Error converting stage left/right textures: %O" err