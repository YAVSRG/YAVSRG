namespace Prelude.Skins.Conversions.Osu

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Percyqaz.Common
open Prelude
open Prelude.Calculator
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts

module Image =

    let pad (width: int, height: int) (image: Bitmap) : Bitmap =
        assert (image.Width <= width)

        if image.Width <> width || image.Height <> height then
            let new_image = new Bitmap(width, height)
            new_image.Mutate(fun img -> img.DrawImage(image, Point((-image.Width + width) / 2, (-image.Height + height) / 2), 1.0f) |> ignore)

            new_image
        else
            image

    let pad_to_square (target_width: int) (image: Bitmap) : Bitmap = pad (target_width, target_width) image

    let upscale_to_square (target_width: int) (image: Bitmap) : Bitmap =
        assert(image.Width <= target_width)
        assert(image.Height <= target_width)
        if image.Height > image.Width then
            image.Mutate(fun img -> img.Resize(0, target_width) |> ignore)
        else
            image.Mutate(fun img -> img.Resize(target_width, 0) |> ignore)
        pad_to_square target_width image

    let stretch_to_square (target_width: int) (image: Bitmap) : Bitmap =
        assert (image.Width = target_width)
        image.Mutate(fun img -> img.Resize(target_width, target_width) |> ignore)
        image

    let scale_y (scale: float32) (image: Bitmap) : Bitmap =
        let image = image.Clone()
        image.Mutate(fun img -> img.Resize(image.Width, float32 image.Height * scale |> round |> int) |> ignore)
        image

    let rotate (rotate_mode: RotateMode) (image: Bitmap) : Bitmap =
        let image = pad_to_square image.Width image
        image.Mutate(fun img -> img.Rotate(rotate_mode) |> ignore)
        image

    let grayscale (brightness: float32) (image: Bitmap) : Bitmap =
        let new_image = image.Clone()
        new_image.Mutate(fun img -> img.Grayscale().Brightness(brightness) |> ignore)
        new_image

    type PixelFormats.Rgba32 with
        member this.BlackToTransparent =
            let new_alpha =
                min
                    this.A
                    (max this.R this.G |> max this.B)
            PixelFormats.Rgba32(this.R, this.G, this.B, new_alpha)

    let remove_black_bg (image: Bitmap) : Bitmap =
        let newImage = new Bitmap(image.Width, image.Height)
        for x = 0 to image.Width - 1 do
            for y = 0 to image.Height - 1 do
                newImage.[x, y] <- image.[x, y].BlackToTransparent
        newImage

module OsuSkinConverter =

    let check_before_convert (source: string) : Result<SkinIni, string> =
        SkinIni.FromFile (Path.Combine(source, "skin.ini"))

    let scale_receptor (target_width: int) (height: int) (is_2x_res: bool) (image: Bitmap) : Bitmap =
        if is_2x_res then
            image.Mutate(fun img -> img.Resize(target_width, image.Height) |> ignore)
        else
            image.Mutate(fun img -> img.Resize(target_width, image.Height * 2) |> ignore)
        let new_image = new Bitmap(target_width, height)
        new_image.Mutate(fun img -> img.DrawImage(image, Point(0, 0), 1.0f) |> ignore)
        new_image

    let convert_element_textures (target: string) (element_name: string) (images: Bitmap list list) =
        let animation_frames = List.map List.length images |> List.max
        let colors = List.length images

        let max_width = images |> List.map (List.map _.Width >> List.max) |> List.max
        let max_height = images |> List.map (List.map _.Height >> List.max) |> List.max
        let size = max max_width max_height

        for row = 0 to (colors - 1) do
            for column = 0 to (animation_frames - 1) do
                let image = let r = images.[row] in r[column % r.Length]

                let square_image = Image.upscale_to_square size image
                square_image.Save(Path.Combine(target, TextureFileName.to_loose element_name (column, row)))
                square_image.Dispose()

    let convert_hold_body_textures (target: string) (images: Bitmap list list) : bool =
        let animation_frames = List.map List.length images |> List.max
        let colors = List.length images
        let width = images.Head.Head.Width

        let mutable percy_ln_fix = false

        for row = 0 to (colors - 1) do
            for column = 0 to (animation_frames - 1) do
                let image = let r = images.[row] in r[column % r.Length]

                let square_image =
                    if image.Width > image.Height then
                        Image.stretch_to_square width image
                    else
                        Image.pad_to_square width image

                square_image.Save(Path.Combine(target, TextureFileName.to_loose "holdbody" (column, row)))

                square_image.Dispose()

                if image.Height > 2000 then
                    Logging.Debug("Detected 'Percy LN' in skin, guessing what the tail texture should be..")
                    let mutable i = 0
                    while i < image.Height && image.[image.Width / 2, i].A < 5uy do
                        i <- i + 1
                    let percy_tail_texture = new Bitmap(width, 128)
                    percy_tail_texture.Mutate(fun img ->
                        img
                            .DrawImage(image, Point((-image.Width + width) / 2, -i), 1.0f)
                            .Flip(FlipMode.Vertical)
                            .Resize(width, width)
                        |> ignore)

                    percy_tail_texture.Save(Path.Combine(target, TextureFileName.to_loose "holdtail" (column, row)))

                    percy_ln_fix <- true

        percy_ln_fix

    let detect_square_receptors (sample_receptor: Bitmap) : (Bitmap -> Bitmap) option =
        let mutable top = 0
        while top < sample_receptor.Height && sample_receptor.[sample_receptor.Width / 2, top].A < 5uy do
            top <- top + 1

        let mutable bottom = sample_receptor.Height - 1
        while bottom > top && sample_receptor.[sample_receptor.Width / 2, bottom].A < 5uy do
            bottom <- bottom - 1

        let height = bottom - top
        if height > sample_receptor.Width / 2 && height < sample_receptor.Width then
            Some (fun bmp ->
                let new_bmp = new Bitmap(sample_receptor.Width, sample_receptor.Width)
                new_bmp.Mutate(fun img -> img.DrawImage(bmp, Point((-bmp.Width + sample_receptor.Width) / 2, -top + (sample_receptor.Width - height) / 2), 1.0f) |> ignore)
                new_bmp
            )
        else None

    let arrow_fix_4k (images: Bitmap list list) : Bitmap list list =
        match images with
        | left :: down :: up :: right :: [] ->
            [
                left |> List.map (Image.rotate RotateMode.Rotate270)
                down
                up |> List.map (Image.rotate RotateMode.Rotate180)
                right |> List.map (Image.rotate RotateMode.Rotate90)
            ]
        | not_4k -> not_4k

    type ColumnTextures =
        {
            Note: Result<Texture list, string list>
            Head: Result<Texture list, string list>
            Body: Result<Texture list, string list>
            Tail: Result<Texture list, string list>
        }
        member this.Fingerprint =
            Result.toOption this.Note,
            Result.toOption this.Head,
            Result.toOption this.Body,
            Result.toOption this.Tail

    let expect_texture (t: Result<'T, string list>) : Result<'T, string list> =
        match t with
        | Ok f -> Ok f
        | Error expected_files -> failwithf "Couldn't find any matching image %s" (expected_files |> Seq.map (sprintf "'%s'") |> String.concat ", ")

    let dot_to_colon (dot_texture: Bitmap) : Bitmap =
        let new_bmp = dot_texture.Clone()
        new_bmp.Mutate(fun img ->
            img
                .Flip(FlipMode.Vertical)
                .DrawImage(dot_texture, 1.0f)
            |> ignore)
        new_bmp

    let convert_font (source: string, target: string, osu_skin_prefix: string, osu_overlap: int, element_name: string) : Result<float32, exn> =
        try
            let mutable scale_2x = 1.0f
            let images =
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i, sprintf "score-%i" i)
                |> Seq.map (fun (id, fallback) -> Texture.find(id, fallback, source) |> expect_texture)
                |> Seq.map (function Ok t -> (if t.Is2x then scale_2x <- 2.0f); Ok t | otherwise -> otherwise)
                |> Seq.map (Texture.load_single_texture >> _.As2x)
                |> Array.ofSeq

            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max

            let optional_extras =
                try
                    let dot =
                        Texture.find(sprintf "%s-dot" osu_skin_prefix, "score-dot", source)
                        |> expect_texture
                        |> Texture.load_single_texture
                        |> _.As2x
                    let colon = dot_to_colon dot
                    let percent =
                        Texture.find(sprintf "%s-percent" osu_skin_prefix, "score-percent", source)
                        |> expect_texture
                        |> Texture.load_single_texture
                        |> _.As2x
                    percent.Mutate(fun img -> img.Crop(min percent.Width max_width, percent.Height) |> ignore)
                    [|dot; colon; percent|]
                with _ -> [||]

            for i, img in Array.append images optional_extras |> Array.indexed do
                let padded = Image.pad (max_width, max_height) img
                padded.Save(Path.Combine(target, TextureFileName.to_loose element_name (0, i)))
            Ok (-scale_2x * float32 osu_overlap / float32 max_width)
        with err ->
            Error err

    type ConvertedFont =
        {
            Spacing: float32
            DotExtraSpacing: float32
            ColonExtraSpacing: float32
            PercentExtraSpacing: float32
        }

    let convert_font_with_extras (source: string, target: string, osu_skin_prefix: string, osu_overlap: int, element_name: string) : Result<ConvertedFont, exn> =
        try
            let mutable scale_2x = 1.0f
            let dot =
                Texture.find(sprintf "%s-dot" osu_skin_prefix, "score-dot", source)
                |> expect_texture
                |> Texture.load_single_texture
                |> _.As2x
            let colon = dot_to_colon dot
            let percent =
                Texture.find(sprintf "%s-percent" osu_skin_prefix, "score-percent", source)
                |> expect_texture
                |> Texture.load_single_texture
                |> _.As2x

            let images =
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i, sprintf "score-%i" i)
                |> Seq.map (fun (id, fallback) -> Texture.find(id, fallback, source) |> expect_texture)
                |> Seq.map (function Ok t -> (if t.Is2x then scale_2x <- 2.0f); Ok t | otherwise -> otherwise)
                |> Seq.map (Texture.load_single_texture >> _.As2x)
                |> Array.ofSeq

            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max

            percent.Mutate(fun img -> img.Crop(min percent.Width max_width, percent.Height) |> ignore)

            for i, img in Array.append images [|dot; colon; percent|] |> Array.indexed do
                let padded = Image.pad (max_width, max_height) img
                padded.Save(Path.Combine(target, TextureFileName.to_loose element_name (0, i)))

            Ok {
                Spacing = -scale_2x * float32 osu_overlap / float32 max_width
                DotExtraSpacing = -0.5f * float32 (max_width - dot.Width) / float32 max_width
                ColonExtraSpacing = -0.5f * float32 (max_width - colon.Width) / float32 max_width
                PercentExtraSpacing = -0.5f * float32 (max_width - percent.Width) / float32 max_width
            }
        with err ->
            Error err

    let private convert_to_hud (ini: SkinIni) (source: string) (target: string) (keymode: int) =

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

                    let padded = Image.pad (max_width, max_height) image
                    padded.Save(Path.Combine(target, TextureFileName.to_loose "judgements" (column, row)))
                    padded.Dispose()
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
                let padded = Image.pad (max_width, max_height) image
                padded.Save(Path.Combine(target, TextureFileName.to_loose "judgement-counter-judgements" (0, i)))
                padded.Dispose()
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

    let private convert_to_noteskin (ini: SkinIni) (source: string) (target: string) (keymode: int) (is_arrows: bool) =

        if Directory.Exists target then
            failwith "a folder with this name already exists!"

        Directory.CreateDirectory target |> ignore

        let version =
            match System.Decimal.TryParse(ini.General.Version, System.Globalization.CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> 3.0m

        Logging.Debug "Converting skin '%s', version %A" ini.General.Name version

        let default_settings = Mania.Default keymode version

        let keymode_settings =
            ini.Mania
            |> List.tryFind (fun m -> m.Keys = keymode)
            |> Option.defaultValue default_settings

        let note_height_scale =
            match keymode_settings.WidthForNoteHeightScale with
            | None -> 1.0f
            | Some h ->
                let s = float32 h / float32 keymode_settings.ColumnWidth.[0]
                Logging.Debug "WidthForNoteHeightScale is %.3fx column width, using that as scale" s
                s

        let colors = Array.zeroCreate 10
        let receptor_colors =
            [|
                [|0; 2; 0|]; [|0; 1; 1; 0|]; [|0; 1; 2; 1; 0|];
                [|0; 1; 0; 0; 1; 0|]; [|0; 1; 0; 2; 0; 1; 0|]; [|0; 1; 1; 0; 0; 1; 1; 0|];
                [|0; 1; 0; 1; 2; 1; 0; 1; 0|]; [|0; 1; 2; 1; 0; 0; 1; 2; 1; 0|]
            |]
        let column_light_colors =
            [|
                [|0; 2; 0|]; [|0; 1; 1; 0|]; [|0; 1; 2; 1; 0|];
                [|0; 1; 0; 0; 1; 0|]; [|0; 1; 0; 2; 0; 1; 0|]; [|0; 1; 1; 0; 0; 1; 1; 0|];
                [|0; 1; 0; 1; 2; 1; 0; 1; 0|]; [|0; 1; 2; 1; 0; 0; 1; 2; 1; 0|]
            |]

        let core_textures =
            let textures =
                Array.init keymode (fun k ->
                    {
                        Note = Texture.find_animated(keymode_settings.NoteImageΔ.[k], default_settings.NoteImageΔ.[k], source)
                        Head = Texture.find_animated(keymode_settings.NoteImageΔH.[k], default_settings.NoteImageΔH.[k], source)
                        Body = Texture.find_animated(keymode_settings.NoteImageΔL.[k], default_settings.NoteImageΔL.[k], source)
                        Tail = Texture.find_animated(keymode_settings.NoteImageΔT.[k], default_settings.NoteImageΔT.[k], source)
                    }
                )
            let distinct = ResizeArray<_>()
            let mutable k = 0
            seq {
                for t in textures do
                    let f = t.Fingerprint
                    if not (distinct.Contains f) then
                        distinct.Add f
                        yield t
                    colors.[k] <- byte (distinct.IndexOf f)
                    k <- k + 1
            }
            |> List.ofSeq

        Logging.Debug "%i column variations to convert (Distinct by note/head/body/tail textures)" core_textures.Length

        let mutable flipholdtail = keymode_settings.NoteFlipWhenUpsideDownΔT.[0]
        let mutable useholdtail = true
        let mutable stage_textures = false
        let mutable skip_tail_conversion = false

        let mutable key_receptors = false
        let mutable receptor_offset = 0.0f

        let mutable column_lights = false
        let mutable column_light_offset = 0.0f

        let mutable judgement_line = false
        let mutable judgement_line_scale = 1.0f

        let mutable note_explosions_scale = None
        let mutable hold_explosions_scale = None

        // Generate main textures
        try
            core_textures
            |> List.map (fun x -> x.Note)
            |> List.map Texture.load_animated_texture
            |> List.map (List.map _.As2x)
            |> List.map (List.map (Image.scale_y note_height_scale))
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "note"
        with err ->
            Logging.Warn "Error converting note textures: %O" err

        try
            let percy_ln_detected =
                core_textures
                |> List.map (fun x -> x.Body)
                |> List.map Texture.load_animated_texture
                |> List.map (List.map _.As2x)
                |> convert_hold_body_textures target
            if percy_ln_detected then
                skip_tail_conversion <- true
                flipholdtail <- true
        with err ->
            Logging.Warn "Error converting hold body textures: %O" err

        try
            core_textures
            |> List.map (fun x -> x.Head)
            |> List.map Texture.load_animated_texture
            |> List.map (List.map _.As2x)
            |> List.map (List.map (Image.scale_y note_height_scale))
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "holdhead"
        with err ->
            Logging.Warn "Error converting hold head textures: %O" err
            skip_tail_conversion <- true

        if not skip_tail_conversion then
            try
                core_textures
                |> List.map (fun x -> x.Tail)
                |> List.map expect_texture
                |> List.map Texture.load_animated_texture
                |> List.map (List.map _.As2x)
                |> List.map (List.map (Image.scale_y note_height_scale))
                |> convert_element_textures target "holdtail"
            with err ->
                Logging.Debug "Error in holdtail textures - Using hold head textures instead: %O" err

                core_textures
                |> List.map (fun x -> x.Head)
                |> List.map Texture.load_animated_texture
                |> List.map (List.map _.As2x)
                |> List.map (List.map (Image.scale_y note_height_scale))
                |> if is_arrows then arrow_fix_4k else id
                |> convert_element_textures target "holdtail"

                flipholdtail <- true

        // Convert keys to receptors
        try
            let key_textures =
                let distinct_detection = ResizeArray<Texture option * Texture option>()
                let distinct = ResizeArray<Result<Texture, string list> * Result<Texture, string list>>()
                let not_pressed_images =
                    Array.init keymode (fun i ->
                        Texture.find(keymode_settings.KeyImageΔ.[i], default_settings.KeyImageΔ.[i], source)
                    )
                let pressed_images =
                    Array.init keymode (fun i ->
                        Texture.find(keymode_settings.KeyImageΔD.[i], default_settings.KeyImageΔD.[i], source)
                    )
                Array.zip not_pressed_images pressed_images
                |> Array.iteri (fun i (np, p) ->
                    let f = Result.toOption np, Result.toOption p
                    if not (distinct_detection.Contains f) then
                        distinct_detection.Add f
                        distinct.Add((np, p))
                    receptor_colors.[keymode - 3].[i] <- distinct_detection.IndexOf f
                )

                distinct
                |> Seq.map (fun (not_pressed, pressed) -> [not_pressed; pressed])
                |> List.concat
                |> List.map Texture.load_single_texture

            let height =
                key_textures
                |> Seq.map (fun { Image = bmp; Is2x = is_2x } -> if is_2x then bmp.Height else bmp.Height * 2)
                |> Seq.max

            let width = float32 keymode_settings.ColumnWidth.[0] * 3.2f |> int

            let scaled =
                key_textures
                |> Seq.map (fun { Image = bmp; Is2x = is_2x } -> scale_receptor width height is_2x bmp)
                |> Array.ofSeq

            match detect_square_receptors scaled.[0] with
            | Some transform ->
                Logging.Debug "Detected 'receptors' in osu mania key textures"
                scaled
                |> Seq.indexed
                |> Seq.iter (fun (i, img) -> (transform img).Save(Path.Combine(target, TextureFileName.to_loose "receptor" (0, i))))
            | None ->
                scaled
                |> Seq.indexed
                |> Seq.iter (fun (i, img) -> img.Save(Path.Combine(target, TextureFileName.to_loose "receptor" (0, i))))
                key_receptors <- true
                let column_width_osu_px = float32 keymode_settings.ColumnWidth.[0]
                let top_of_key_osu_px = float32 height / float32 width * column_width_osu_px
                let hitposition_osu_px = 480f - float32 keymode_settings.HitPosition
                receptor_offset <- (top_of_key_osu_px - hitposition_osu_px) / column_width_osu_px - 0.5f

        with err ->
            Logging.Warn "Error converting keys to receptors: %O" err
            try
                use receptor_base =
                    core_textures.[if core_textures.Length > 1 then 1 else 0].Note
                    |> Texture.load_animated_texture
                    |> List.head
                    |> _.Image

                use not_pressed = Image.grayscale 0.5f receptor_base |> Image.pad_to_square receptor_base.Width
                use pressed = Image.grayscale 1.0f receptor_base |> Image.pad_to_square receptor_base.Width

                not_pressed.Save(Path.Combine(target, TextureFileName.to_loose "receptor" (0, 0)))
                pressed.Save(Path.Combine(target, TextureFileName.to_loose "receptor" (0, 1)))
            with err ->
                Logging.Warn "Error generating placeholder receptors from note textures: %O" err

        // Convert stage hint
        try
            let stage_hint =
                Texture.find (keymode_settings.StageHint, default_settings.StageHint, source)
                |> expect_texture
                |> Texture.load_single_texture
                |> _.As2x
            let intended_height_interlude_px = float32 stage_hint.Height
            judgement_line_scale <- intended_height_interlude_px / (float32 keymode_settings.ColumnWidth.[0] * 1080f / 480f)
            stage_hint.Save(Path.Combine(target, TextureFileName.to_loose "judgementline" (0, 0)))
            judgement_line <- true
        with err ->
            Logging.Warn "Error converting stage hint to judgement line: %O" err

        // Convert stage textures
        try
            let stage_left =
                Texture.find (keymode_settings.StageLeft, default_settings.StageLeft, source)
                |> expect_texture
                |> Texture.load_single_texture
                |> _.Image
            let stage_right =
                Texture.find (keymode_settings.StageRight, default_settings.StageRight, source)
                |> expect_texture
                |> Texture.load_single_texture
                |> _.Image

            stage_left.Save(Path.Combine(target, TextureFileName.to_loose "stageleft" (0, 0)))
            stage_right.Save(Path.Combine(target, TextureFileName.to_loose "stageright" (0, 0)))
            stage_textures <- true
        with err ->
            Logging.Warn "Error converting stage left/right textures: %O" err

        // Convert column lighting textures
        try
            let base_image =
                Texture.find (keymode_settings.StageLight, default_settings.StageLight, source)
                |> expect_texture
                |> Texture.load_single_texture
                |> _.Image

            let distinct_colors = ResizeArray<Color>()

            for k = 0 to keymode - 1 do
                let stage_light_color = keymode_settings.ColourLightΔ.[k]

                if not (distinct_colors.Contains stage_light_color) then
                    use colored = base_image.Clone()

                    let color = SixLabors.ImageSharp.Color.FromRgb(stage_light_color.R, stage_light_color.G, stage_light_color.B)
                    colored.Mutate(fun img ->
                        img.Fill(
                            GraphicsOptions(
                                ColorBlendingMode = PixelFormats.PixelColorBlendingMode.Multiply,
                                AlphaCompositionMode = PixelFormats.PixelAlphaCompositionMode.SrcAtop
                            ), color
                        )
                            .Opacity(float32 stage_light_color.A / 255.0f)
                        |> ignore
                    )
                    colored.Save(Path.Combine(target, TextureFileName.to_loose "receptorlighting" (0, distinct_colors.Count)))

                    distinct_colors.Add stage_light_color
                column_light_colors.[keymode - 3].[k] <- distinct_colors.IndexOf stage_light_color

            column_lights <- true
            let column_width_osu_px = float32 keymode_settings.ColumnWidth.[0]
            let light_position_osu_px = 480.0f - float32 keymode_settings.LightPosition
            let hitposition_osu_px = 480f - float32 keymode_settings.HitPosition
            column_light_offset <- (light_position_osu_px - hitposition_osu_px) / column_width_osu_px + 0.5f
        with err ->
            Logging.Warn "Error converting column lighting: %O" err

        // Convert explosions
        try
            let images =
                Texture.find_animated(keymode_settings.LightingN, default_settings.LightingN, source)
                |> Texture.load_animated_texture
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = Image.pad_to_square max_dim (Image.remove_black_bg image)
                padded.Save(Path.Combine(target, TextureFileName.to_loose "noteexplosion" (i, 0)))
                padded.Dispose()

            note_explosions_scale <- Some (float32 max_dim / 49.0f)
        with err ->
            Logging.Warn "Error converting note explosions: %O" err

        try
            let images =
                Texture.find_animated(keymode_settings.LightingL, default_settings.LightingL, source)
                |> Texture.load_animated_texture
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = Image.pad_to_square max_dim (Image.remove_black_bg image)
                padded.Save(Path.Combine(target, TextureFileName.to_loose "holdexplosion" (i, 0)))
                padded.Dispose()

            hold_explosions_scale <- Some (float32 max_dim / 49.0f)
        with err ->
            Logging.Warn "Error converting hold explosions: %O" err

        // Generate noteskin.json
        let color_config: ColorConfig =
            { ColorConfig.Default with
                Style = ColorScheme.Column
                UseGlobalColors = false
            }

        color_config.Colors.[keymode - 2] <- colors

        let config: NoteskinConfig =
            { NoteskinConfig.Default with
                NoteColors = color_config
                FlipHoldTail = flipholdtail
                UseHoldTailTexture = useholdtail
                HoldNoteTrim = if skip_tail_conversion then 1.5f else 0.0f
                PlayfieldColor = keymode_settings.ColourΔ.[0]
                ColumnWidth = 1080f / 480f * float32 keymode_settings.ColumnWidth.[0]
                AnimationFrameTime = 16.7f<ms / rate>
                UseRotation = is_arrows
                EnableStageTextures = stage_textures

                EnableColumnLight = column_lights
                ColumnLightColors = column_light_colors
                ColumnLightOffset = column_light_offset

                UseReceptors = true
                ReceptorStyle = if key_receptors then ReceptorStyle.Keys else ReceptorStyle.Receptors
                ReceptorColors = receptor_colors
                ReceptorOffset = receptor_offset
                NotesUnderReceptors = not keymode_settings.KeysUnderNotes

                UseJudgementLine = judgement_line
                JudgementLineScale = judgement_line_scale

                UseExplosions = note_explosions_scale.IsSome && hold_explosions_scale.IsSome
                NoteExplosionSettings =
                    match note_explosions_scale with
                    | None -> NoteExplosionConfig.Default
                    | Some scale -> { NoteExplosionConfig.Default with UseBuiltInAnimation = false; Scale = scale }
                HoldExplosionSettings =
                    match hold_explosions_scale with
                    | None -> HoldExplosionConfig.Default
                    | Some scale -> { HoldExplosionConfig.Default with Scale = scale }
            }

        JSON.ToFile (Path.Combine(target, "noteskin.json"), false) config

    let convert_to_skin (ini: SkinIni) (source: string) (target: string) (keymode: int) (is_arrows: bool) =
        convert_to_noteskin ini source (Path.Combine(target, "Noteskin")) keymode is_arrows
        convert_to_hud ini source (Path.Combine(target, "HUD")) keymode
        JSON.ToFile
            (Path.Combine(target, "skin.json"), false)
            {
                Name = ini.General.Name
                Author = ini.General.Author
                Editor = None
            }