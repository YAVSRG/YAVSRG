namespace Prelude.Skins.Conversions.Osu.HUD

open System.IO
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Skins
open Prelude.Skins.Conversions.Osu

module internal HudJudgementConverter =

    let convert_judgement_textures (ctx: HudConverterContext) : unit =
        try
            let images =
                [
                    ctx.KeymodeSettings.Hit300g, ctx.DefaultSettings.Hit300g
                    ctx.KeymodeSettings.Hit300, ctx.DefaultSettings.Hit300
                    ctx.KeymodeSettings.Hit200, ctx.DefaultSettings.Hit200
                    ctx.KeymodeSettings.Hit100, ctx.DefaultSettings.Hit100
                    ctx.KeymodeSettings.Hit50, ctx.DefaultSettings.Hit50
                    ctx.KeymodeSettings.Hit0, ctx.DefaultSettings.Hit0
                ]
                |> List.map (fun (x, fallback) -> TextureAnimationSearchResult.Create(x, fallback, ctx.FileSystem))
                |> List.map _.Load(ctx.FileSystem)
                |> List.map (List.map _.As2x)
            let max_frames = images |> List.map (fun x -> x.Length) |> List.max
            let max_width = images |> List.map (List.map _.Width >> List.max) |> List.max
            let max_height = images |> List.map (List.map _.Height >> List.max) |> List.max

            for row = 0 to images.Length - 1 do
                for column = 0 to max_frames - 1 do
                    let image = let r = images.[row] in r[column % r.Length]

                    let padded = ImageOperations.pad (max_width, max_height) image
                    padded.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "judgements" (column, row)))
            ctx.JudgementTextures <- true
        with err ->
            Logging.Warn "Error converting judgement textures: %O" err
    
    let convert_judgement_counter_textures(ctx: HudConverterContext) : unit =
        try
            let images =
                [
                    ctx.DefaultSettings.Hit300g
                    ctx.DefaultSettings.Hit300
                    ctx.DefaultSettings.Hit200
                    ctx.DefaultSettings.Hit100
                    ctx.DefaultSettings.Hit50
                    ctx.DefaultSettings.Hit0
                ]
                |> List.map (fun x -> TextureAnimationSearchResult.Create(x, x, ctx.FileSystem))
                |> List.map _.FirstFrame
                |> List.map _.Load(ctx.FileSystem)
                |> List.map _.As2x
            let max_width = images |> List.map _.Width |> List.max
            let max_height = images |> List.map _.Height |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad (max_width, max_height) image
                padded.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "judgement-counter-judgements" (0, i)))
            ctx.JudgementCounterTextures <- true
        with err ->
            Logging.Warn "Error converting judgement counter judgement textures: %O" err