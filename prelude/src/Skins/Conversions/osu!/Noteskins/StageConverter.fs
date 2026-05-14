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
                Texture.find (ctx.KeymodeSettings.StageLeft, ctx.DefaultSettings.StageLeft, ctx.Source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.Image
            let stage_right =
                Texture.find (ctx.KeymodeSettings.StageRight, ctx.DefaultSettings.StageRight, ctx.Source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.Image

            stage_left.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "stageleft" (0, 0)))
            stage_right.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "stageright" (0, 0)))
            ctx.StageTextures <- true
        with err ->
            Logging.Warn "Error converting stage left/right textures: %O" err