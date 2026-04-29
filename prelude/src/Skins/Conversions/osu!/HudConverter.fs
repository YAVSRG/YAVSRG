namespace Prelude.Skins.Conversions.Osu

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Prelude
open Prelude.Skins
open Prelude.Skins.HudLayouts

module HudConverter =

    let private dot_to_colon (dot_texture: Bitmap) : Bitmap =
        let new_bmp = dot_texture.Clone()
        new_bmp.Mutate(fun img ->
            img
                .Flip(FlipMode.Vertical)
                .DrawImage(dot_texture, 1.0f)
            |> ignore)
        new_bmp

    let private convert_font (source: string, target: string, osu_skin_prefix: string, osu_overlap: int, element_name: string) : Result<float32, exn> =
        try
            let mutable scale_2x = 1.0f
            let images =
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i, sprintf "score-%i" i)
                |> Seq.map (fun (id, fallback) -> Texture.find(id, fallback, source) |> Texture.expect)
                |> Seq.map (function Ok t -> (if t.Is2x then scale_2x <- 2.0f); Ok t | otherwise -> otherwise)
                |> Seq.map (Texture.load_single_texture >> _.As2x)
                |> Array.ofSeq

            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max

            let optional_extras =
                try
                    let dot =
                        Texture.find(sprintf "%s-dot" osu_skin_prefix, "score-dot", source)
                        |> Texture.expect
                        |> Texture.load_single_texture
                        |> _.As2x
                    let colon = dot_to_colon dot
                    let percent =
                        Texture.find(sprintf "%s-percent" osu_skin_prefix, "score-percent", source)
                        |> Texture.expect
                        |> Texture.load_single_texture
                        |> _.As2x
                    percent.Mutate(fun img -> img.Crop(min percent.Width max_width, percent.Height) |> ignore)
                    [|dot; colon; percent|]
                with _ -> [||]

            for i, img in Array.append images optional_extras |> Array.indexed do
                let padded = ImageOperations.pad (max_width, max_height) img
                padded.Save(Path.Combine(target, TextureFileName.to_loose element_name (0, i)))
            Ok (-scale_2x * float32 osu_overlap / float32 max_width)
        with err ->
            Error err

    type internal ConvertedFont =
        {
            Spacing: float32
            DotExtraSpacing: float32
            ColonExtraSpacing: float32
            PercentExtraSpacing: float32
        }

    let private convert_font_with_extras (source: string, target: string, osu_skin_prefix: string, osu_overlap: int, element_name: string) : Result<ConvertedFont, exn> =
        try
            let mutable scale_2x = 1.0f
            let dot =
                Texture.find(sprintf "%s-dot" osu_skin_prefix, "score-dot", source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.As2x
            let colon = dot_to_colon dot
            let percent =
                Texture.find(sprintf "%s-percent" osu_skin_prefix, "score-percent", source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.As2x

            let images =
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i, sprintf "score-%i" i)
                |> Seq.map (fun (id, fallback) -> Texture.find(id, fallback, source) |> Texture.expect)
                |> Seq.map (function Ok t -> (if t.Is2x then scale_2x <- 2.0f); Ok t | otherwise -> otherwise)
                |> Seq.map (Texture.load_single_texture >> _.As2x)
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

    let convert_to_hud (ini: SkinIni) (source: string) (target: string) (keymode: int) =

        if Directory.Exists target then
            failwith "a folder with this name already exists!"

        Directory.CreateDirectory target |> ignore

        let version =
            match System.Decimal.TryParse(ini.General.Version, System.Globalization.CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> 3.0m

        let default_settings = Mania.Default keymode version

        let keymode_settings =
            ini.Mania
            |> List.tryFind (fun m -> m.Keys = keymode)
            |> Option.defaultValue default_settings

        let mutable combo_font_spacing : float32 option = None
        let mutable accuracy_font_info : ConvertedFont option = None
        let mutable progress_meter_font_info : ConvertedFont option = None
        let mutable judgement_counter_font_info : ConvertedFont option = None

        let mutable judgement_textures = false
        let mutable judgement_counter_textures = false

        // Convert judgement textures
        try
            let images =
                [
                    keymode_settings.Hit300g, default_settings.Hit300g
                    keymode_settings.Hit300, default_settings.Hit300
                    keymode_settings.Hit200, default_settings.Hit200
                    keymode_settings.Hit100, default_settings.Hit100
                    keymode_settings.Hit50, default_settings.Hit50
                    keymode_settings.Hit0, default_settings.Hit0
                ]
                |> List.map (fun (x, fallback) -> Texture.find_animated(x, fallback, source))
                |> List.map Texture.load_animated_texture
                |> List.map (List.map _.As2x)
            let max_frames = images |> List.map (fun x -> x.Length) |> List.max
            let max_width = images |> List.map (List.map _.Width >> List.max) |> List.max
            let max_height = images |> List.map (List.map _.Height >> List.max) |> List.max

            for row = 0 to images.Length - 1 do
                for column = 0 to max_frames - 1 do
                    let image = let r = images.[row] in r[column % r.Length]

                    let padded = ImageOperations.pad (max_width, max_height) image
                    padded.Save(Path.Combine(target, TextureFileName.to_loose "judgements" (column, row)))
            judgement_textures <- true
        with err ->
            Logging.Warn "Error converting judgement textures: %O" err

        // Convert judgement counter textures
        try
            let images =
                [
                    default_settings.Hit300g
                    default_settings.Hit300
                    default_settings.Hit200
                    default_settings.Hit100
                    default_settings.Hit50
                    default_settings.Hit0
                ]
                |> List.map (fun x -> Texture.find_animated_fallback(x, source))
                |> List.map (Result.map List.head)
                |> List.map Texture.load_single_texture
                |> List.map _.As2x
            let max_width = images |> List.map _.Width |> List.max
            let max_height = images |> List.map _.Height |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad (max_width, max_height) image
                padded.Save(Path.Combine(target, TextureFileName.to_loose "judgement-counter-judgements" (0, i)))
            judgement_counter_textures <- true
        with err ->
            Logging.Warn "Error converting judgement counter judgement textures: %O" err

        // Convert various fonts
        match
            convert_font (
                source,
                target,
                ini.Fonts.ComboPrefix,
                ini.Fonts.ComboOverlap,
                "combo-font"
            )
        with
        | Ok spacing ->
            combo_font_spacing <- Some spacing
        | Error err ->
            Logging.Warn "Error converting combo font: %O" err

        match
            convert_font_with_extras (
                source,
                target,
                ini.Fonts.ScorePrefix,
                ini.Fonts.ScoreOverlap,
                "accuracy-font"
            )
        with
        | Ok info ->
            accuracy_font_info <- Some info
        | Error err ->
            Logging.Warn "Error converting accuracy font: %O" err

        match
            convert_font_with_extras (
                source,
                target,
                ini.Fonts.ScorePrefix,
                ini.Fonts.ScoreOverlap,
                "progress-meter-font"
            )
        with
        | Ok info ->
            progress_meter_font_info <- Some info
        | Error err ->
            Logging.Warn "Error converting progress meter font: %O" err

        match
            convert_font_with_extras (
                source,
                target,
                ini.Fonts.ScorePrefix,
                ini.Fonts.ScoreOverlap,
                "judgement-counter-font"
            )
        with
        | Ok info ->
            judgement_counter_font_info <- Some info
        | Error err ->
            Logging.Warn "Error converting judgement counter font: %O" err

        let config: HudConfig =
            { HudConfig.Default with
                JudgementMeterFrameTime = 16.7f<ms / rate>
                JudgementMeterUseTexture = judgement_textures
                JudgementMeterCustomDisplay =
                    if judgement_textures then
                        Map.ofSeq [6, Array.init 6 JudgementDisplayType.Texture]
                    else Map.empty

                ComboUseFont = combo_font_spacing.IsSome
                ComboFontSpacing = combo_font_spacing |> Option.defaultValue 0.0f

                AccuracyUseFont = accuracy_font_info.IsSome
                AccuracyFontSpacing = accuracy_font_info |> Option.map _.Spacing |> Option.defaultValue 0.0f
                AccuracyDotExtraSpacing = accuracy_font_info |> Option.map _.DotExtraSpacing |> Option.defaultValue 0.0f
                AccuracyPercentExtraSpacing = accuracy_font_info |> Option.map _.PercentExtraSpacing |> Option.defaultValue 0.0f

                JudgementCounterUseFont = judgement_counter_font_info.IsSome
                JudgementCounterFontSpacing = judgement_counter_font_info |> Option.map _.Spacing |> Option.defaultValue 0.0f
                JudgementCounterDotExtraSpacing = judgement_counter_font_info |> Option.map _.DotExtraSpacing |> Option.defaultValue 0.0f
                JudgementCounterColonExtraSpacing = judgement_counter_font_info |> Option.map _.ColonExtraSpacing |> Option.defaultValue 0.0f

                JudgementCounterCustomDisplay =
                    if judgement_counter_textures then
                        Map.ofSeq [6, Array.init 6 Some]
                    else Map.empty

                ProgressMeterUseFont = progress_meter_font_info.IsSome
                ProgressMeterFontSpacing = progress_meter_font_info |> Option.map _.Spacing |> Option.defaultValue 0.0f
                ProgressMeterColonExtraSpacing = progress_meter_font_info |> Option.map _.ColonExtraSpacing |> Option.defaultValue 0.0f
                ProgressMeterPercentExtraSpacing = progress_meter_font_info |> Option.map _.PercentExtraSpacing |> Option.defaultValue 0.0f
            }

        JSON.ToFile (Path.Combine(target, "hud.json"), false) config