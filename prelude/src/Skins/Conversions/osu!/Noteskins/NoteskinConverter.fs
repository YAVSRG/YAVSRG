namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Percyqaz.Common
open Prelude
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.Conversions.Osu

module NoteskinConverter =

    let private scale_receptor (target_width: int) (height: int) (is_2x_res: bool) (image: Bitmap) : Bitmap =
        if is_2x_res then
            image.Clone().Mutate(fun img -> img.Resize(target_width, image.Height) |> ignore)
        else
            image.Clone().Mutate(fun img -> img.Resize(target_width, image.Height * 2) |> ignore)
        let new_image = new Bitmap(target_width, height)
        new_image.Mutate(fun img -> img.DrawImage(image, Point(0, 0), 1.0f) |> ignore)
        new_image

    let private convert_element_textures (target: string) (element_name: string) (images: Bitmap list list) =
        let animation_frames = List.map List.length images |> List.max
        let colors = List.length images

        let max_width = images |> List.map (List.map _.Width >> List.max) |> List.max
        let max_height = images |> List.map (List.map _.Height >> List.max) |> List.max
        let size = max max_width max_height

        for row = 0 to (colors - 1) do
            for column = 0 to (animation_frames - 1) do
                let image = let r = images.[row] in r[column % r.Length]

                let square_image = ImageOperations.upscale_to_square size image
                square_image.Save(Path.Combine(target, TextureFileName.to_loose element_name (column, row)))

    let private convert_hold_body_textures (target: string) (images: Bitmap list list) : bool =
        let animation_frames = List.map List.length images |> List.max
        let colors = List.length images
        let width = images.Head.Head.Width

        let mutable percy_ln_fix = false

        for row = 0 to (colors - 1) do
            for column = 0 to (animation_frames - 1) do
                let image = let r = images.[row] in r[column % r.Length]

                let square_image =
                    if image.Width > image.Height then
                        ImageOperations.stretch_to_square width image
                    else
                        ImageOperations.pad_to_square width image

                square_image.Save(Path.Combine(target, TextureFileName.to_loose "holdbody" (column, row)))

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

    let private detect_square_receptors (sample_receptor: Bitmap) : (Bitmap -> Bitmap) option =
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

    let private arrow_fix_4k (images: Bitmap list list) : Bitmap list list =
        match images with
        | left :: down :: up :: [ right ] ->
            [
                left |> List.map (ImageOperations.rotate RotateMode.Rotate270)
                down
                up |> List.map (ImageOperations.rotate RotateMode.Rotate180)
                right |> List.map (ImageOperations.rotate RotateMode.Rotate90)
            ]
        | not_4k -> not_4k

    let convert_to_noteskin (ini: SkinIni) (source: string) (target: string) (keymode: int) (is_arrows: bool) =

        if Directory.Exists target then
            failwith "a folder with this name already exists!"

        Directory.CreateDirectory target |> ignore

        let ctx = NoteskinConverterContext.Create(source, target, ini, keymode)

        Logging.Debug "Converting skin '%s', version %.1f" ini.General.Name ctx.Version

        let note_height_scale =
            match ctx.KeymodeSettings.WidthForNoteHeightScale with
            | None -> 1.0f
            | Some h ->
                let s = float32 h / float32 ctx.KeymodeSettings.ColumnWidth.[0]
                Logging.Debug "WidthForNoteHeightScale is %.3fx column width, using that as scale" s
                s

        let core_textures : ColumnTextures list =
            let textures =
                Array.init keymode (fun k ->
                    {
                        Note = Texture.find_animated(ctx.KeymodeSettings.NoteImageΔ.[k], ctx.DefaultSettings.NoteImageΔ.[k], source)
                        Head = Texture.find_animated(ctx.KeymodeSettings.NoteImageΔH.[k], ctx.DefaultSettings.NoteImageΔH.[k], source)
                        Body = Texture.find_animated(ctx.KeymodeSettings.NoteImageΔL.[k], ctx.DefaultSettings.NoteImageΔL.[k], source)
                        Tail = Texture.find_animated(ctx.KeymodeSettings.NoteImageΔT.[k], ctx.DefaultSettings.NoteImageΔT.[k], source)
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
                    ctx.Colors.[k] <- byte (distinct.IndexOf f)
                    k <- k + 1
            }
            |> List.ofSeq

        Logging.Debug "%i column variations to convert (Distinct by note/head/body/tail textures)" core_textures.Length

        // Generate main textures
        try
            core_textures
            |> List.map _.Note
            |> List.map Texture.load_animated_texture
            |> List.map (List.map _.As2x)
            |> List.map (List.map (ImageOperations.scale_y note_height_scale))
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "note"
        with err ->
            Logging.Warn "Error converting note textures: %O" err

        try
            let percy_ln_detected =
                core_textures
                |> List.map _.Body
                |> List.map Texture.load_animated_texture
                |> List.map (List.map _.As2x)
                |> convert_hold_body_textures target
            if percy_ln_detected then
                ctx.SkipTailConversion <- true
                ctx.FlipHoldTail <- true
        with err ->
            Logging.Warn "Error converting hold body textures: %O" err

        try
            core_textures
            |> List.map _.Head
            |> List.map Texture.load_animated_texture
            |> List.map (List.map _.As2x)
            |> List.map (List.map (ImageOperations.scale_y note_height_scale))
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "holdhead"
        with err ->
            Logging.Warn "Error converting hold head textures: %O" err
            ctx.SkipTailConversion <- true

        if not ctx.SkipTailConversion then
            try
                core_textures
                |> List.map _.Tail
                |> List.map Texture.expect
                |> List.map Texture.load_animated_texture
                |> List.map (List.map _.As2x)
                |> List.map (List.map (ImageOperations.scale_y note_height_scale))
                |> convert_element_textures target "holdtail"
            with err ->
                Logging.Debug "Error in holdtail textures - Using hold head textures instead: %O" err

                core_textures
                |> List.map _.Head
                |> List.map Texture.load_animated_texture
                |> List.map (List.map _.As2x)
                |> List.map (List.map (ImageOperations.scale_y note_height_scale))
                |> if is_arrows then arrow_fix_4k else id
                |> convert_element_textures target "holdtail"

                ctx.FlipHoldTail <- true

        Receptors.convert_receptors(ctx, core_textures)

        // Convert stage hint
        try
            let stage_hint =
                Texture.find (ctx.KeymodeSettings.StageHint, ctx.DefaultSettings.StageHint, source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.As2x
            let intended_height_interlude_px = float32 stage_hint.Height
            ctx.JudgementLineScale <- intended_height_interlude_px / (float32 ctx.KeymodeSettings.ColumnWidth.[0] * 1080f / 480f)
            stage_hint.Save(Path.Combine(target, TextureFileName.to_loose "judgementline" (0, 0)))
            ctx.JudgementLine <- true
        with err ->
            Logging.Warn "Error converting stage hint to judgement line: %O" err

        // Convert stage textures
        try
            let stage_left =
                Texture.find (ctx.KeymodeSettings.StageLeft, ctx.DefaultSettings.StageLeft, source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.Image
            let stage_right =
                Texture.find (ctx.KeymodeSettings.StageRight, ctx.DefaultSettings.StageRight, source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.Image

            stage_left.Save(Path.Combine(target, TextureFileName.to_loose "stageleft" (0, 0)))
            stage_right.Save(Path.Combine(target, TextureFileName.to_loose "stageright" (0, 0)))
            ctx.StageTextures <- true
        with err ->
            Logging.Warn "Error converting stage left/right textures: %O" err

        // Convert column lighting textures
        try
            let base_image =
                Texture.find (ctx.KeymodeSettings.StageLight, ctx.DefaultSettings.StageLight, source)
                |> Texture.expect
                |> Texture.load_single_texture
                |> _.Image

            let distinct_colors = ResizeArray<Color>()

            for k = 0 to keymode - 1 do
                let stage_light_color = ctx.KeymodeSettings.ColourLightΔ.[k]

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
                ctx.ColumnLightColors.[keymode - 3].[k] <- distinct_colors.IndexOf stage_light_color

            ctx.ColumnLights <- true
            let column_width_osu_px = float32 ctx.KeymodeSettings.ColumnWidth.[0]
            let light_position_osu_px = 480.0f - float32 ctx.KeymodeSettings.LightPosition
            let hitposition_osu_px = 480f - float32 ctx.KeymodeSettings.HitPosition
            ctx.ColumnLightsOffset <- (light_position_osu_px - hitposition_osu_px) / column_width_osu_px + 0.5f
        with err ->
            Logging.Warn "Error converting column lighting: %O" err

        // Convert explosions
        try
            let images =
                Texture.find_animated(ctx.KeymodeSettings.LightingN, ctx.DefaultSettings.LightingN, source)
                |> Texture.load_animated_texture
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad_to_square max_dim (ImageOperations.remove_black_bg image)
                padded.Save(Path.Combine(target, TextureFileName.to_loose "noteexplosion" (i, 0)))

            ctx.NoteExplosionsScale <- Some (float32 max_dim / 49.0f)
        with err ->
            Logging.Warn "Error converting note explosions: %O" err

        try
            let images =
                Texture.find_animated(ctx.KeymodeSettings.LightingL, ctx.DefaultSettings.LightingL, source)
                |> Texture.load_animated_texture
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad_to_square max_dim (ImageOperations.remove_black_bg image)
                padded.Save(Path.Combine(target, TextureFileName.to_loose "holdexplosion" (i, 0)))

            ctx.HoldExplosionsScale <- Some (float32 max_dim / 49.0f)
        with err ->
            Logging.Warn "Error converting hold explosions: %O" err

        // Generate noteskin.json
        let color_config: ColorConfig =
            { ColorConfig.Default with
                Style = ColorScheme.Column
                UseGlobalColors = false
            }

        color_config.Colors.[keymode - 2] <- ctx.Colors

        let config: NoteskinConfig =
            { NoteskinConfig.Default with
                NoteColors = color_config
                FlipHoldTail = ctx.FlipHoldTail
                UseHoldTailTexture = ctx.UseHoldTail
                HoldNoteTrim = if ctx.SkipTailConversion then 1.5f else 0.0f
                PlayfieldColor = ctx.KeymodeSettings.ColourΔ.[0]
                ColumnWidth = 1080f / 480f * float32 ctx.KeymodeSettings.ColumnWidth.[0]
                AnimationFrameTime = 16.7f<ms / rate>
                UseRotation = is_arrows
                EnableStageTextures = ctx.StageTextures

                EnableColumnLight = ctx.ColumnLights
                ColumnLightColors = ctx.ColumnLightColors
                ColumnLightOffset = ctx.ColumnLightsOffset

                UseReceptors = true
                ReceptorStyle = if ctx.KeyReceptors then ReceptorStyle.Keys else ReceptorStyle.Receptors
                ReceptorColors = ctx.ReceptorColors
                ReceptorOffset = ctx.ReceptorOffset
                NotesUnderReceptors = not ctx.KeymodeSettings.KeysUnderNotes

                UseJudgementLine = ctx.JudgementLine
                JudgementLineScale = ctx.JudgementLineScale

                UseExplosions = ctx.NoteExplosionsScale.IsSome && ctx.HoldExplosionsScale.IsSome
                NoteExplosionSettings =
                    match ctx.NoteExplosionsScale with
                    | None -> NoteExplosionConfig.Default
                    | Some scale -> { NoteExplosionConfig.Default with UseBuiltInAnimation = false; Scale = scale }
                HoldExplosionSettings =
                    match ctx.HoldExplosionsScale with
                    | None -> HoldExplosionConfig.Default
                    | Some scale -> { HoldExplosionConfig.Default with Scale = scale }
            }

        JSON.ToFile (Path.Combine(target, "noteskin.json"), false) config