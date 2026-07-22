namespace Prelude.Skins.Conversions.Osu.HUD

open System.IO
open Percyqaz.Common
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Prelude
open Prelude.Skins
open Prelude.Skins.Conversions.Osu

module internal HudFontConverter =

    let private dot_to_colon (dot_texture: Bitmap) : Bitmap =
        let new_bmp = dot_texture.Clone()
        new_bmp.Mutate(fun img ->
            img
                .Flip(FlipMode.Vertical)
                .DrawImage(dot_texture, 1.0f)
            |> ignore)
        new_bmp

    let convert_font (fs: OsuSkinFileSystem, target: string, osu_skin_prefix: string, osu_overlap: int, element_name: string) : Result<float32, exn> =
        try
            let mutable scale_2x = 1.0f
            let images =
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i, sprintf "score-%i" i)
                |> Seq.map (fun (id, fallback) -> fs.SearchForTexture(id, fallback).ThrowIfNotFound())
                |> Seq.map (function TextureSearchResult.Ok t -> (if t.EndsWith("@2x.png") then scale_2x <- 2.0f); TextureSearchResult.Ok t | otherwise -> otherwise)
                |> Seq.map _.Load(fs).As2x
                |> Array.ofSeq

            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max

            let optional_extras =
                try
                    let dot =
                        fs
                            .SearchForTexture(sprintf "%s-dot" osu_skin_prefix, "score-dot")
                            .ThrowIfNotFound()
                            .Load(fs)
                            .As2x
                    let colon = dot_to_colon dot
                    let percent =
                        fs
                            .SearchForTexture(sprintf "%s-percent" osu_skin_prefix, "score-percent")
                            .ThrowIfNotFound()
                            .Load(fs)
                            .As2x
                    percent.Mutate(fun img -> img.Crop(min percent.Width max_width, percent.Height) |> ignore)
                    [|dot; colon; percent|]
                with _ -> [||]

            for i, img in Array.append images optional_extras |> Array.indexed do
                let padded = ImageOperations.pad (max_width, max_height) img
                padded.Save(Path.Combine(target, TextureFileName.to_loose element_name (0, i)))
            Ok (-scale_2x * float32 osu_overlap / float32 max_width)
        with err ->
            Error err

    let convert_font_with_extras (fs: OsuSkinFileSystem, target: string, osu_skin_prefix: string, osu_overlap: int, element_name: string) : Result<ConvertedFont, exn> =
        try
            let mutable scale_2x = 1.0f
            let dot =
                fs
                    .SearchForTexture(sprintf "%s-dot" osu_skin_prefix, "score-dot")
                    .ThrowIfNotFound()
                    .Load(fs)
                    .As2x
            let colon = dot_to_colon dot
            let percent =
                fs
                    .SearchForTexture(sprintf "%s-percent" osu_skin_prefix, "score-percent")
                    .ThrowIfNotFound()
                    .Load(fs)
                    .As2x

            let images =
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i, sprintf "score-%i" i)
                |> Seq.map (fun (id, fallback) -> fs.SearchForTexture(id, fallback).ThrowIfNotFound())
                |> Seq.map (function TextureSearchResult.Ok t -> (if t.EndsWith("@2x.png") then scale_2x <- 2.0f); TextureSearchResult.Ok t | otherwise -> otherwise)
                |> Seq.map _.Load(fs).As2x
                |> Array.ofSeq

            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max

            percent.Mutate(fun img -> img.Crop(min percent.Width max_width, percent.Height) |> ignore)

            for i, img in Array.append images [|dot; colon; percent|] |> Array.indexed do
                let padded = ImageOperations.pad (max_width, max_height) img
                padded.Save(Path.Combine(target, TextureFileName.to_loose element_name (0, i)))

            Ok {
                Spacing = -scale_2x * float32 osu_overlap / float32 max_width
                DotExtraSpacing = -0.5f * float32 (max_width - dot.Width) / float32 max_width
                ColonExtraSpacing = -0.5f * float32 (max_width - colon.Width) / float32 max_width
                PercentExtraSpacing = -0.5f * float32 (max_width - percent.Width) / float32 max_width
            }
        with err ->
            Error err
            
    let convert_all_fonts(ctx: HudConverterContext) : unit =
        match
            convert_font (
                ctx.FileSystem,
                ctx.Target,
                ctx.SkinIni.Fonts.ComboPrefix,
                ctx.SkinIni.Fonts.ComboOverlap,
                "combo-font"
            )
        with
        | Ok spacing ->
            ctx.ComboFontSpacing <- Some spacing
        | Error err ->
            Logging.Warn "Error converting combo font: %O" err

        match
            convert_font_with_extras (
                ctx.FileSystem,
                ctx.Target,
                ctx.SkinIni.Fonts.ScorePrefix,
                ctx.SkinIni.Fonts.ScoreOverlap,
                "accuracy-font"
            )
        with
        | Ok info ->
            ctx.AccuracyFont <- Some info
        | Error err ->
            Logging.Warn "Error converting accuracy font: %O" err

        match
            convert_font_with_extras (
                ctx.FileSystem,
                ctx.Target,
                ctx.SkinIni.Fonts.ScorePrefix,
                ctx.SkinIni.Fonts.ScoreOverlap,
                "progress-meter-font"
            )
        with
        | Ok info ->
            ctx.ProgressMeterFont <- Some info
        | Error err ->
            Logging.Warn "Error converting progress meter font: %O" err

        match
            convert_font_with_extras (
                ctx.FileSystem,
                ctx.Target,
                ctx.SkinIni.Fonts.ScorePrefix,
                ctx.SkinIni.Fonts.ScoreOverlap,
                "judgement-counter-font"
            )
        with
        | Ok info ->
            ctx.JudgementCounterFont <- Some info
        | Error err ->
            Logging.Warn "Error converting judgement counter font: %O" err