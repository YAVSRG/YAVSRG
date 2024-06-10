namespace Prelude.Skinning.Noteskins.Conversion

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Percyqaz.Common
open Prelude
open Prelude.Charts.Processing
open Prelude.Skinning
open Prelude.Skinning.Noteskins

module private TextureFile =

    let load_bmp f =
        use s = File.Open(f, FileMode.Open)
        Bitmap.from_stream false s |> Option.get

    type Loaded =
        {
            Image: Bitmap
            Is2x: bool
        }

    type Found =
        {
            Path: string
            Is2x: bool
        }
        member this.Load = { Image = load_bmp this.Path; Is2x = this.Is2x }

    let find (id: string, path: string) : Found option =
        let file = Path.Combine(path, id)

        if File.Exists(file + "@2x.png") then Some { Path = file + "@2x.png"; Is2x = true }
        elif File.Exists(file + ".png") then Some { Path = file + ".png"; Is2x = false }
        else None

    let find_animation_frames (id: string, path: string) : Found list =

        let file = Path.Combine(path, id)

        let rec find_2x_animation i =
            if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                { Path = file + "-" + i.ToString() + "@2x.png"; Is2x = true } :: find_2x_animation (i + 1)
            else []

        let rec find_animation i =
            if File.Exists(file + "-" + i.ToString() + ".png") then
                { Path = file + "-" + i.ToString() + ".png"; Is2x = false } :: find_animation (i + 1)
            else []

        let animation_2x = find_2x_animation 0
        let animation = find_animation 0

        if animation_2x <> [] then
            animation_2x
        elif File.Exists(file + "@2x.png") then
            [ { Path = file + "@2x.png"; Is2x = true } ]
        elif animation <> [] then
            animation
        elif File.Exists(file + ".png") then
            [ { Path = file + ".png"; Is2x = false } ]
        else
            failwithf "could not find texture in skin folder for %A" id

    let load_animation_frames (paths: Found list list) =
        paths |> List.map (fun row -> row |> List.map _.Load)

module private Image =

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
        for x = 0 to image.Width - 1 do
            for y = 0 to image.Height - 1 do
                image.[x, y] <- image.[x, y].BlackToTransparent
        image

module OsuSkinConverter =

    let check_before_convert (source: string) =
        try
            SkinIni.parse (Path.Combine(source, "skin.ini")) |> Ok
        with err ->
            Error err.Message

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
                        Image.stretch_to_square width image
                    else
                        Image.pad_to_square width image

                square_image.Save(Path.Combine(target, sprintf "holdbody-%i-%i.png" row column))

                square_image.Dispose()

                if image.Height > 2000 then
                    Logging.Debug("Detected 'percy ln' in skin, trying to guess what the tail texture should be")
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
                        (Path.Combine(target, "holdtail.json"), true)
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

    let detect_square_receptors (sample_receptor: Bitmap) : (Bitmap -> Bitmap) option =
        let mutable top = 0
        while top < sample_receptor.Height && sample_receptor.[sample_receptor.Width / 2, top].A < 5uy do
            top <- top + 1
        
        let mutable bottom = sample_receptor.Height - 1
        while bottom > top && sample_receptor.[sample_receptor.Width / 2, bottom].A < 5uy do
            bottom <- bottom - 1

        let height = bottom - top
        if height > sample_receptor.Width / 2 && height < sample_receptor.Width * 3 / 2 then
            Some (fun bmp ->
                let new_bmp = new Bitmap(sample_receptor.Width, sample_receptor.Width)
                new_bmp.Mutate(fun img -> img.DrawImage(bmp, Point((-bmp.Width + sample_receptor.Width) / 2, -top + (sample_receptor.Width - height) / 2), 1.0f) |> ignore)
                new_bmp
            )
        else None

    let arrow_fix_4k (images: Bitmap list list) =
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
            Note: string
            Head: string
            Body: string
            Tail: string
        }

    let dot_to_colon (dot_texture: Bitmap) =
        let new_bmp = dot_texture.Clone()
        new_bmp.Mutate(fun img ->
            img
                .Flip(FlipMode.Vertical)
                .DrawImage(dot_texture, 1.0f) 
            |> ignore)
        new_bmp

    let convert_font (source: string, target: string, osu_skin_prefix: string, osu_overlap: int, element_name: string) =
        try
            let images = 
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i)
                |> Seq.map (fun id -> match TextureFile.find(id, source) with Some f -> f | None -> failwithf "Couldn't find font image '%s'" id)
                |> Seq.map (_.Load >> _.Image) // todo: handle mismatched 2x and 1x images
                |> Array.ofSeq

            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max

            let optional_extras =
                try
                    let dot = TextureFile.find(sprintf "%s-dot" osu_skin_prefix, source).Value.Load.Image
                    let colon = dot_to_colon dot
                    let percent = TextureFile.find(sprintf "%s-percent" osu_skin_prefix, source).Value.Load.Image
                    percent.Mutate(fun img -> img.Crop(min percent.Width max_width, percent.Height) |> ignore)
                    [|dot; colon; percent|]
                with _ -> [||]

            for i, img in Array.append images optional_extras |> Array.indexed do
                let padded = Image.pad (max_width, max_height) img
                padded.Save(Path.Combine(target, sprintf "%s-%i-0.png" element_name i))
            JSON.ToFile
                (Path.Combine(target, element_name + ".json"), false)
                {
                    Rows = images.Length + optional_extras.Length
                    Columns = 1
                    Mode = Loose
                }
            Ok (-2.0f * float32 osu_overlap / float32 max_width)
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
            let dot = TextureFile.find(sprintf "%s-dot" osu_skin_prefix, source).Value.Load.Image
            let colon = dot_to_colon dot
            let percent = TextureFile.find(sprintf "%s-percent" osu_skin_prefix, source).Value.Load.Image

            let images = 
                seq { 0 .. 9 }
                |> Seq.map (fun i -> sprintf "%s-%i" osu_skin_prefix i)
                |> Seq.map (fun id -> match TextureFile.find(id, source) with Some f -> f | None -> failwithf "Couldn't find font image '%s'" id)
                |> Seq.map (_.Load >> _.Image) // todo: handle mismatched 2x and 1x images
                |> Array.ofSeq

            let max_width = images |> Seq.map _.Width |> Seq.max
            let max_height = images |> Seq.map _.Height |> Seq.max

            percent.Mutate(fun img -> img.Crop(min percent.Width max_width, percent.Height) |> ignore)

            for i, img in Array.append images [|dot; colon; percent|] |> Array.indexed do
                let padded = Image.pad (max_width, max_height) img
                padded.Save(Path.Combine(target, sprintf "%s-%i-0.png" element_name i))

            JSON.ToFile
                (Path.Combine(target, element_name + ".json"), false)
                {
                    Rows = images.Length + 3
                    Columns = 1
                    Mode = Loose
                }

            Ok {
                Spacing = -2.0f * float32 osu_overlap / float32 max_width
                DotExtraSpacing = -0.5f * float32 (max_width - dot.Width) / float32 max_width
                ColonExtraSpacing = -0.5f * float32 (max_width - colon.Width) / float32 max_width
                PercentExtraSpacing = -0.5f * float32 (max_width - percent.Width) / float32 max_width
            }
        with err ->
            Error err

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
        let mutable skip_tail_conversion = false

        let mutable combo_font_spacing : float32 option = None
        let mutable accuracy_font_info : ConvertedFont option = None
        let mutable progress_meter_font_info : ConvertedFont option = None
        let mutable judgement_counter_font_info : ConvertedFont option = None

        let mutable judgement_textures = false
        let mutable judgement_counter_textures = false

        let mutable note_explosions_scale = None
        let mutable hold_explosions_scale = None

        // Generate main textures
        try
            core_textures
            |> List.map (fun x -> x.Note)
            |> List.map (fun x -> TextureFile.find_animation_frames(x, source))
            |> TextureFile.load_animation_frames
            |> List.map (List.map _.Image)
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "note"
        with err ->
            Logging.Warn("Error converting note textures", err)

        try
            core_textures
            |> List.map (fun x -> x.Head)
            |> List.map (fun x -> TextureFile.find_animation_frames(x, source))
            |> TextureFile.load_animation_frames
            |> List.map (List.map _.Image)
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "holdhead"
        with err ->
            Logging.Warn("Error converting hold head textures", err)

        try
            let percy_ln_detected =
                core_textures
                |> List.map (fun x -> x.Body)
                |> List.map (fun x -> TextureFile.find_animation_frames(x, source))
                |> TextureFile.load_animation_frames
                |> List.map (List.map _.Image)
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
                |> List.map (fun x -> TextureFile.find_animation_frames(x, source))
                |> TextureFile.load_animation_frames
                |> List.map (List.map _.Image)
                |> convert_element_textures target "holdtail"
            with err ->
                Logging.Debug("Error in holdtail textures - Using hold head textures for compatibility", err)
                flipholdtail <- true
                useholdtail <- false

        // Convert keys to receptors
        try
            let key_textures = 
                Array.zip keymode_settings.KeyImageΔ keymode_settings.KeyImageΔD
                // todo: distinct once receptor texture order exists
                |> Seq.map (fun (not_pressed, pressed) -> [not_pressed; pressed])
                |> List.concat
                |> List.map (fun id -> TextureFile.find(id, source).Value)
                |> List.map _.Load

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
                Logging.Debug("Detected 'receptors' in osu mania key textures")
                scaled
                |> Seq.indexed
                |> Seq.iter (fun (i, img) -> (transform img).Save(Path.Combine(target, sprintf "receptor-%i-0.png" i)))
            | None ->
                scaled
                |> Seq.indexed
                |> Seq.iter (fun (i, img) -> img.Save(Path.Combine(target, sprintf "receptor-%i-0.png" i)))
                key_receptors <- true
            JSON.ToFile (Path.Combine(target, "receptor.json"), false) { Rows = key_textures.Length; Columns = 1; Mode = Loose }
        with err ->
            Logging.Warn("Error converting keys to receptors", err)
            try
                use receptor_base =
                    TextureFile.find_animation_frames(core_textures.[if core_textures.Length > 1 then 1 else 0].Note, source)
                    |> List.head
                    |> _.Load
                    |> _.Image

                use not_pressed = Image.grayscale 0.5f receptor_base |> Image.pad_to_square receptor_base.Width
                use pressed = Image.grayscale 1.0f receptor_base |> Image.pad_to_square receptor_base.Width

                not_pressed.Save(Path.Combine(target, "receptor-0-0.png"))
                pressed.Save(Path.Combine(target, "receptor-1-0.png"))

                JSON.ToFile (Path.Combine(target, "receptor.json"), false) { Rows = 2; Columns = 1; Mode = Loose }
            with err ->
                Logging.Warn("Error generating placeholder receptors from note textures", err)

        // Convert stage textures
        match TextureFile.find (keymode_settings.StageLeft, source) with
        | Some stage_left ->
            match TextureFile.find (keymode_settings.StageRight, source) with
            | Some stage_right ->
                stage_left.Load.Image.Save(Path.Combine(target, "stageleft.png"))
                stage_right.Load.Image.Save(Path.Combine(target, "stageright.png"))
                stage_textures <- true
            | None -> ()
        | None -> ()

        // Convert column lighting textures
        match TextureFile.find (keymode_settings.StageLight, source) with
        | Some stage_light ->
            use base_image = stage_light.Load.Image

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

        // Convert judgement textures
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
                |> List.map (fun x -> TextureFile.find_animation_frames (x, source))
                |> TextureFile.load_animation_frames
                // todo: scale up images to 2x where not
                |> List.map (List.map _.Image)
            let max_frames = images |> List.map (fun x -> x.Length) |> List.max
            let max_width = images |> List.map (List.map _.Width >> List.max) |> List.max
            let max_height = images |> List.map (List.map _.Height >> List.max) |> List.max

            for row = 0 to images.Length - 1 do
                for column = 0 to max_frames - 1 do
                    let image = let r = images.[row] in r[column % r.Length]

                    let padded = Image.pad (max_width, max_height) image
                    padded.Save(Path.Combine(target, sprintf "judgements-%i-%i.png" row column))
                    padded.Dispose()

            JSON.ToFile
                (Path.Combine(target, "judgements.json"), false)
                {
                    Rows = images.Length
                    Columns = max_frames
                    Mode = Loose
                }
            judgement_textures <- true
        with err ->
            Logging.Warn("Error converting judgement textures", err)

        // Convert judgement counter textures
        try
            let images =
                [
                    "mania-hit300g"
                    "mania-hit300"
                    "mania-hit200"
                    "mania-hit100"
                    "mania-hit50"
                    "mania-hit0"
                ]
                |> List.map (fun x -> TextureFile.find_animation_frames(x, source).Head.Load.Image)
            let max_width = images |> List.map _.Width |> List.max
            let max_height = images |> List.map _.Height |> List.max

            for i, image in List.indexed images do
                let padded = Image.pad (max_width, max_height) image
                padded.Save(Path.Combine(target, sprintf "judgement-counter-judgements-%i-0.png" i))
                padded.Dispose()

            JSON.ToFile
                (Path.Combine(target, "judgement-counter-judgements.json"), false)
                {
                    Rows = images.Length
                    Columns = 1
                    Mode = Loose
                }
            judgement_counter_textures <- true
        with err ->
            Logging.Warn("Error converting judgement counter judgement textures", err)

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
            Logging.Warn("Error converting combo font", err)
        
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
            Logging.Warn("Error converting accuracy font", err)
        
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
            Logging.Warn("Error converting progress meter font", err)

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
            Logging.Warn("Error converting judgement counter font", err)

        // Convert explosions
        try
            let images = 
                TextureFile.find_animation_frames (keymode_settings.LightingN, source)
                |> List.map (_.Load >> _.Image)
            
            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = Image.pad_to_square max_dim (Image.remove_black_bg image)
                padded.Save(Path.Combine(target, sprintf "noteexplosion-0-%i.png" i))
                padded.Dispose()
            

            JSON.ToFile
                (Path.Combine(target, "noteexplosion.json"), false)
                {
                    Rows = 1
                    Columns = images.Length
                    Mode = Loose
                }
            note_explosions_scale <- Some (480.0f / float32 max_dim)
        with err ->
            Logging.Warn("Error converting note explosions", err)
        
        try
            let images = 
                TextureFile.find_animation_frames (keymode_settings.LightingL, source)
                |> List.map (_.Load >> _.Image)
            
            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = Image.pad_to_square max_dim (Image.remove_black_bg image)
                padded.Save(Path.Combine(target, sprintf "holdexplosion-0-%i.png" i))
                padded.Dispose()
            

            JSON.ToFile
                (Path.Combine(target, "holdexplosion.json"), false)
                {
                    Rows = 1
                    Columns = images.Length
                    Mode = Loose
                }
            hold_explosions_scale <- Some (480.0f / float32 max_dim)
        with err ->
            Logging.Warn("Error converting hold explosions", err)

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
                        
                        JudgementCounterUseJudgementTextures = judgement_counter_textures
                        JudgementCounterCustomDisplay =
                            if judgement_counter_textures then 
                                Map.ofSeq [6, Array.init 6 Some]
                            else Map.empty

                        ProgressMeterUseFont = progress_meter_font_info.IsSome
                        ProgressMeterFontSpacing = progress_meter_font_info |> Option.map _.Spacing |> Option.defaultValue 0.0f
                        ProgressMeterColonExtraSpacing = progress_meter_font_info |> Option.map _.ColonExtraSpacing |> Option.defaultValue 0.0f
                        ProgressMeterPercentExtraSpacing = progress_meter_font_info |> Option.map _.PercentExtraSpacing |> Option.defaultValue 0.0f
                    }

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
