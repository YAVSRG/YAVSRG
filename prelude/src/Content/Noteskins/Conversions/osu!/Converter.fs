namespace Prelude.Content.Noteskins.Conversion

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Prelude.Content
open Prelude.Content.Noteskins

module OsuSkinConverter =

    let check_before_convert (source: string) =
        try
            SkinIni.parse (Path.Combine(source, "skin.ini")) |> Ok
        with err ->
            Error err.Message

    let get_single_filename (id: string, path: string) : string option =
        let file = Path.Combine(path, id)

        if File.Exists(file + "@2x.png") then Some(file + "@2x.png")
        elif File.Exists(file + ".png") then Some(file + ".png")
        else None

    let get_animation_frame_filenames (id: string, path: string) : string list =

        let file = Path.Combine(path, id)

        let rec find_2x_animation i =
            if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                file + "-" + i.ToString() + "@2x.png" :: find_2x_animation (i + 1)
            else []

        let rec find_animation i =
            if File.Exists(file + "-" + i.ToString() + ".png") then
                file + "-" + i.ToString() + ".png" :: find_animation (i + 1)
            else []

        let animation_2x = find_2x_animation 0
        let animation = find_animation 0

        if animation_2x <> [] then
            animation_2x
        elif File.Exists(file + "@2x.png") then
            [ file + "@2x.png" ]
        elif animation <> [] then
            animation
        elif File.Exists(file + ".png") then
            [ file + ".png" ]
        else
            failwithf "could not find texture in skin folder for %A" id

    let load_bmp f =
        use s = File.Open(f, FileMode.Open)
        Bitmap.from_stream false s |> Option.get

    let load_animation_frame_images (paths: string list list) =
        paths |> List.map (fun row -> row |> List.map load_bmp)

    type ColumnTextures =
        {
            Note: string
            Head: string
            Body: string
            Tail: string
        }

    let pad (width: int, height: int) (image: Bitmap) : Bitmap =
        assert (image.Width <= width)

        if image.Width <> width || image.Height <> height then
            let new_image = new Bitmap(width, height)
            new_image.Mutate(fun img -> img.DrawImage(image, Point((-image.Width + width) / 2, (-image.Height + height) / 2), 1.0f) |> ignore)

            new_image
        else
            image

    let pad_to_square (width: int) (image: Bitmap) : Bitmap = pad (width, width) image

    let stretch_to_square (width: int) (image: Bitmap) : Bitmap =
        assert (image.Width = width)
        image.Mutate(fun img -> img.Resize(width, width) |> ignore)
        image

    let rotate (rotate_mode: RotateMode) (image: Bitmap) : Bitmap =
        let image = pad_to_square image.Width image
        image.Mutate(fun img -> img.Rotate(rotate_mode) |> ignore)
        image

    let grayscale (brightness: float32) (image: Bitmap) : Bitmap =
        let new_image = image.Clone()
        new_image.Mutate(fun i -> i.Grayscale().Brightness(brightness) |> ignore)
        new_image

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
        let width = images.Head.Head.Width

        for row = 0 to (colors - 1) do
            for column = 0 to (animation_frames - 1) do
                let image = let r = images.[row] in r[column % r.Length]

                let square_image = pad_to_square width image
                square_image.Save(Path.Combine(target, sprintf "%s-%i-%i.png" element_name row column))
                square_image.Dispose()

        JSON.ToFile
            (Path.Combine(target, element_name + ".json"), false)
            {
                Rows = colors
                Columns = animation_frames
                Mode = Loose
            }

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
                        stretch_to_square width image
                    else
                        pad_to_square width image

                square_image.Save(Path.Combine(target, sprintf "holdbody-%i-%i.png" row column))

                square_image.Dispose()

                if image.Height > 2000 then
                    Logging.Info("Detected 'percy ln' in skin, trying to guess what the tail texture should be")
                    let mutable i = 0
                    while i < image.Height && image.[image.Width / 2, i].A < 5uy do
                        i <- i + 1
                    let percy_tail_texture = new Bitmap(width, width / 2)
                    percy_tail_texture.Mutate(fun img -> 
                        img
                            .DrawImage(image, Point((-image.Width + width) / 2, -i), 1.0f)
                            .Flip(FlipMode.Vertical)
                            .Resize(width, width) 
                        |> ignore)

                    percy_tail_texture.Save(Path.Combine(target, "holdtail-0-0.png"))
                    JSON.ToFile
                        (Path.Combine(target, "holdtail.json"), false)
                        {
                            Rows = 1
                            Columns = 1
                            Mode = Loose
                        }

                    percy_ln_fix <- true

        JSON.ToFile
            (Path.Combine(target, "holdbody.json"), false)
            {
                Rows = colors
                Columns = animation_frames
                Mode = Loose
            }

        percy_ln_fix

    let arrow_fix_4k (images: Bitmap list list) =
        match images with
        | left :: down :: up :: right :: [] ->
            [
                left |> List.map (rotate RotateMode.Rotate270)
                down
                up |> List.map (rotate RotateMode.Rotate180)
                right |> List.map (rotate RotateMode.Rotate90)
            ]
        | not_4k -> not_4k

    let convert (ini: SkinIni) (source: string) (target: string) (keymode: int) (is_arrows: bool) =

        if Directory.Exists target then
            failwith "a folder with this name already exists!"

        Directory.CreateDirectory target |> ignore

        let keymode_settings =
            ini.Mania
            |> List.tryFind (fun m -> m.Keys = keymode)
            |> Option.defaultValue (SkinIni.Mania.Default keymode)

        let colors = Array.zeroCreate 10

        let core_textures =
            let result = ResizeArray<ColumnTextures>()

            for k = 0 to (keymode - 1) do
                let tex =
                    {
                        Note = keymode_settings.NoteImageΔ.[k]
                        Head = keymode_settings.NoteImageΔH.[k]
                        Body = keymode_settings.NoteImageΔL.[k]
                        Tail = keymode_settings.NoteImageΔT.[k]
                    }

                if not (result.Contains tex) then
                    result.Add tex

                colors.[k] <- byte (result.IndexOf tex)

            result |> List.ofSeq

        let mutable flipholdtail = false
        let mutable useholdtail = true
        let mutable columnlighting = false
        let mutable stage_textures = false
        let mutable key_receptors = false
        let mutable combo_font = false
        let mutable combo_font_spacing = 0.0f
        let mutable skip_tail_conversion = false

        // Generate main textures
        try
            core_textures
            |> List.map (fun x -> x.Note)
            |> List.map (fun x -> get_animation_frame_filenames (x, source))
            |> load_animation_frame_images
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "note"
        with err ->
            Logging.Warn("Error converting note textures", err)

        try
            core_textures
            |> List.map (fun x -> x.Head)
            |> List.map (fun x -> get_animation_frame_filenames (x, source))
            |> load_animation_frame_images
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "holdhead"
        with err ->
            Logging.Warn("Error converting hold head textures", err)

        try
            let percy_ln_detected =
                core_textures
                |> List.map (fun x -> x.Body)
                |> List.map (fun x -> get_animation_frame_filenames (x, source))
                |> load_animation_frame_images
                |> convert_hold_body_textures target
            if percy_ln_detected then
                skip_tail_conversion <- true
                flipholdtail <- true
        with err ->
            Logging.Warn("Error converting hold body textures", err)

        if not skip_tail_conversion then
            try
                core_textures
                |> List.map (fun x -> x.Tail)
                |> List.map (fun x -> get_animation_frame_filenames (x, source))
                |> load_animation_frame_images
                |> convert_element_textures target "holdtail"
            with err ->
                Logging.Warn("Error in holdtail textures - Using hold head textures for compatibility", err)
                flipholdtail <- true
                useholdtail <- false

        // Convert keys to receptors
        try
            let key_textures = 
                Array.zip keymode_settings.KeyImageΔ keymode_settings.KeyImageΔD
                // todo: distinct once receptor texture order exists
                |> Seq.map (fun (not_pressed, pressed) -> [not_pressed; pressed])
                |> List.concat
                |> List.map (fun id -> get_single_filename(id, source).Value)
                |> List.map (fun path -> (if path.Contains "@2" then true else false), load_bmp path)
            let height =
                key_textures
                |> Seq.map (fun (is_2x, bmp) -> if is_2x then bmp.Height else bmp.Height * 2)
                |> Seq.max
            let width = float32 keymode_settings.ColumnWidth.[0] * 3.2f |> int
            key_textures
            |> Seq.map (fun (is_2x, bmp) -> scale_receptor width height is_2x bmp)
            |> Seq.indexed
            |> Seq.iter (fun (i, img) -> img.Save(Path.Combine(target, sprintf "receptor-%i-0.png" i)))
            JSON.ToFile (Path.Combine(target, "receptor.json"), false) { Rows = key_textures.Length; Columns = 1; Mode = Loose }
            key_receptors <- true
        with err ->
            Logging.Warn("Error converting keys to receptors", err)
            try
                use receptor_base =
                    get_animation_frame_filenames (core_textures.[if core_textures.Length > 1 then 1 else 0].Note, source)
                    |> List.head
                    |> load_bmp

                use not_pressed = grayscale 0.5f receptor_base |> pad_to_square receptor_base.Width
                use pressed = grayscale 1.0f receptor_base |> pad_to_square receptor_base.Width

                not_pressed.Save(Path.Combine(target, "receptor-0-0.png"))
                pressed.Save(Path.Combine(target, "receptor-1-0.png"))

                JSON.ToFile (Path.Combine(target, "receptor.json"), false) { Rows = 2; Columns = 1; Mode = Loose }
            with err ->
                Logging.Warn("Error generating placeholder receptors from note textures", err)

        // Generate stage textures
        match get_single_filename (keymode_settings.StageLeft, source) with
        | Some stage_left ->
            match get_single_filename (keymode_settings.StageRight, source) with
            | Some stage_right ->
                (load_bmp stage_left).Save(Path.Combine(target, "stageleft.png"))
                (load_bmp stage_right).Save(Path.Combine(target, "stageright.png"))
                stage_textures <- true
            | None -> ()
        | None -> ()

        // Generate column lighting textures
        match get_single_filename (keymode_settings.StageLight, source) with
        | Some stage_light ->
            use base_image = load_bmp stage_light

            for k = 0 to keymode - 1 do
                let stage_light_color = keymode_settings.ColourLightΔ.[k]
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
                colored.Save(Path.Combine(target, sprintf "receptorlighting-%i-0.png" k))

            JSON.ToFile
                (Path.Combine(target, "receptorlighting.json"), false)
                {
                    Rows = keymode
                    Columns = 1
                    Mode = Loose
                }

            columnlighting <- true
        | None -> ()

        // Generate judgement textures
        let mutable judgements = false
        try
            let images =
                [
                    keymode_settings.Hit300g
                    keymode_settings.Hit300
                    keymode_settings.Hit200
                    keymode_settings.Hit100
                    keymode_settings.Hit50
                    keymode_settings.Hit0
                ]
                |> List.map (fun x -> get_animation_frame_filenames (x, source))
                |> load_animation_frame_images
            let max_frames = images |> List.map (fun x -> x.Length) |> List.max
            let max_width = images |> List.map (List.map _.Width >> List.max) |> List.max
            let max_height = images |> List.map (List.map _.Height >> List.max) |> List.max

            for row = 0 to images.Length - 1 do
                for column = 0 to max_frames - 1 do
                    let image = let r = images.[row] in r[column % r.Length]

                    let padded = pad (max_width, max_height) image
                    padded.Save(Path.Combine(target, sprintf "judgements-%i-%i.png" row column))
                    padded.Dispose()

            JSON.ToFile
                (Path.Combine(target, "judgements.json"), false)
                {
                    Rows = images.Length
                    Columns = max_frames
                    Mode = Loose
                }
            judgements <- true
        with err ->
            Logging.Warn("Error converting judgement textures", err)

        // Generate combo font
        try
            let images = 
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" ini.Fonts.ComboPrefix i)
                |> Seq.map (fun id -> match get_single_filename(id, source) with Some f -> f | None -> failwithf "Couldn't find font image '%s'" id)
                |> Seq.map load_bmp
                |> Array.ofSeq
            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max
            for i in 0 .. 9 do
                let padded = pad (max_width, max_height) images.[i]
                padded.Save(Path.Combine(target, sprintf "combo-font-%i-0.png" i))
                padded.Dispose()
                images.[i].Dispose()
            JSON.ToFile
                (Path.Combine(target, "combo-font.json"), false)
                {
                    Rows = 10
                    Columns = 1
                    Mode = Loose
                }
            combo_font <- true
            combo_font_spacing <- -2.0f * float32 ini.Fonts.ComboOverlap / float32 max_width
        with err ->
            Logging.Warn("Error converting combo font", err)

        // Generate noteskin.json
        let color_config: ColorConfig =
            { ColorConfig.Default with
                Style = ColorScheme.Column
                UseGlobalColors = false
            }

        color_config.Colors.[keymode - 2] <- colors

        let config: NoteskinConfig =
            { NoteskinConfig.Default with
                Name = ini.General.Name
                Author = ini.General.Author
                NoteColors = color_config
                FlipHoldTail = flipholdtail
                UseHoldTailTexture = useholdtail
                HoldNoteTrim = if skip_tail_conversion then 1.5f else 0.0f
                PlayfieldColor = keymode_settings.ColourΔ.[0]
                ColumnWidth = 1080f / 512f * float32 keymode_settings.ColumnWidth.[0]
                AnimationFrameTime = 1000.0 / 60.0
                UseRotation = is_arrows
                EnableStageTextures = stage_textures
                EnableColumnLight = columnlighting
                ReceptorStyle = if key_receptors then ReceptorStyle.Flip else ReceptorStyle.Rotate

                HUD = 
                    { HUDNoteskinOptions.Default with
                        JudgementMeterFrameTime = 16.7f
                        JudgementMeterUseTexture = judgements
                        JudgementMeterCustomDisplay =
                            if judgements then 
                                Map.ofSeq [6, Array.init 6 JudgementDisplayType.Texture]
                            else Map.empty
                        ComboUseFont = combo_font
                        ComboFontSpacing = combo_font_spacing
                    }
            }

        JSON.ToFile (Path.Combine(target, "noteskin.json"), false) config
