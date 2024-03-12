namespace Prelude.Content.Noteskins.Conversion

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Drawing.Processing
open Percyqaz.Common
open Prelude.Common
open Prelude.Charts.Processing.NoteColors
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
        if File.Exists(file + "@2x.png") then
            Some (file + "@2x.png")
        elif File.Exists(file + ".png") then
            Some (file + ".png")
        else None

    let get_animation_frame_filenames (id: string, path: string) : string list =

        let file = Path.Combine(path, id)

        if File.Exists(file + "@2x.png") then
            [ file + "@2x.png" ]
        elif File.Exists(file + ".png") then
            [ file + ".png" ]
        else
            let rec f i =
                if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                    file + "-" + i.ToString() + "@2x.png" :: f (i + 1)
                elif File.Exists(file + "-" + i.ToString() + ".png") then
                    file + "-" + i.ToString() + ".png" :: f (i + 1)
                else
                    []

            let result = f 0

            if result.IsEmpty then
                failwithf "could not find texture in skin folder for %A" id

            result
        
    let load_bmp f =
        use s = File.Open(f, FileMode.Open)
        Bitmap.from_stream false s |> Option.get

    let load_animation_frame_images (paths: string list list) =
        paths
        |> List.map (fun row -> 
            row |> List.map load_bmp
        )

    type ColumnTextures =
        {
            Note: string
            Head: string
            Body: string
            Tail: string
        }

    let pad_to_square (width: int) (image: Bitmap) : Bitmap =
        assert(image.Width = width)
        if image.Height <> width then
            let new_image = new Bitmap(width, width)
            new_image.Mutate(fun img ->
                img.DrawImage(image, Point(0, (-image.Height + width) / 2), 1.0f) |> ignore
            )

            image.Dispose()
            new_image
        else image
    
    let stretch_to_square (width: int) (image: Bitmap) : Bitmap =
        assert(image.Width = width)
        image.Mutate(fun img ->
            img.Resize(width, width) |> ignore
        )
        image

    let rotate (rotate_mode: RotateMode) (image: Bitmap) : Bitmap =
        let image = pad_to_square image.Width image
        image.Mutate(fun img ->
            img.Rotate(rotate_mode) |> ignore
        )
        image
    
    let grayscale (brightness: float32) (image: Bitmap) : Bitmap =
        let new_image = image.Clone()
        new_image.Mutate(fun i -> i.Grayscale().Brightness(brightness) |> ignore)
        new_image
    
    let convert_element_textures (target: string) (element_name: string) (images: Bitmap list list) =
        let animation_frames = List.map List.length images |> List.max
        let colors = List.length images
        let width = images.Head.Head.Width
    
        // todo: warn if widths don't match
    
        for row = 0 to (colors - 1) do
            for column = 0 to (animation_frames - 1) do
                let image = let r = images.[row] in r[column % r.Length]
                
                let square_image =
                    if element_name = "holdbody" then
                        stretch_to_square width image
                    else 
                        pad_to_square width image
    
                square_image.Save(Path.Combine(target, sprintf "%s-%i-%i.png" element_name row column))

                square_image.Dispose()
    
        JSON.ToFile
            (Path.Combine(target, element_name + ".json"), false)
            {
                Rows = colors
                Columns = animation_frames
                Mode = Loose
            }
    
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
        let textures = 
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

        // Generate main textures
        try
            textures
            |> List.map (fun x -> x.Note)
            |> List.map (fun x -> get_animation_frame_filenames (x, source))
            |> load_animation_frame_images
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "note"
        with err ->
            Logging.Warn("Error converting note textures", err)
        
        try
            textures
            |> List.map (fun x -> x.Head)
            |> List.map (fun x -> get_animation_frame_filenames (x, source))
            |> load_animation_frame_images
            |> if is_arrows then arrow_fix_4k else id
            |> convert_element_textures target "holdhead"
        with err ->
            Logging.Warn("Error converting hold head textures", err)
        
        try
            textures
            |> List.map (fun x -> x.Body)
            |> List.map (fun x -> get_animation_frame_filenames (x, source))
            |> load_animation_frame_images
            |> convert_element_textures target "holdbody"
        with err ->
            Logging.Warn("Error converting hold body textures", err)
        
        try
            textures
            |> List.map (fun x -> x.Tail)
            |> List.map (fun x -> get_animation_frame_filenames (x, source))
            |> load_animation_frame_images
            |> convert_element_textures target "holdtail"
        with err ->
            Logging.Warn("Error in holdtail textures - Using hold head textures for compatibility", err)
            flipholdtail <- true
            useholdtail <- false

        // Generate receptors
        try
            use receptor_base = get_animation_frame_filenames (textures.[if textures.Length > 1 then 1 else 0].Note, source) |> List.head |> load_bmp

            use not_pressed = grayscale 0.5f receptor_base |> pad_to_square receptor_base.Width
            use pressed = grayscale 1.0f receptor_base |> pad_to_square receptor_base.Width

            not_pressed.Save(Path.Combine(target, "receptor-0-0.png"))
            pressed.Save(Path.Combine(target, "receptor-1-0.png"))

            JSON.ToFile (Path.Combine(target, "receptor.json"), false) { Rows = 2; Columns = 1; Mode = Loose }
        with err ->
            Logging.Warn("Error generating receptors", err)

        // Generate extra textures
        match get_single_filename (keymode_settings.StageLeft, source) with
        | Some stage_left ->
            match get_single_filename (keymode_settings.StageRight, source) with
            | Some stage_right ->
                (load_bmp stage_left).Save(Path.Combine(target, "stageleft.png"))
                (load_bmp stage_right).Save(Path.Combine(target, "stageright.png"))
                stage_textures <- true
            | None -> ()
        | None -> ()

        match get_single_filename (keymode_settings.StageLight, source) with
        | Some stage_light ->
            use base_image = load_bmp stage_light
            for k = 0 to keymode - 1 do
                let stage_light_color = keymode_settings.ColourLightΔ.[k]
                use colored = base_image.Clone()
                let color_brush = RecolorBrush(
                    SixLabors.ImageSharp.Color.White,
                    SixLabors.ImageSharp.Color.FromRgba(stage_light_color.R, stage_light_color.G, stage_light_color.B, stage_light_color.A),
                    1.0f
                )
                colored.Mutate(fun img -> 
                    img.Fill(color_brush).Opacity(float32 stage_light_color.A / 255.0f) |> ignore
                )
                colored.Save(Path.Combine(target, sprintf "receptorlighting-%i-0.png" k))
            JSON.ToFile (Path.Combine(target, "receptorlighting.json"), false) { Rows = keymode; Columns = 1; Mode = Loose }
            columnlighting <- true
        | None -> ()

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
                HoldNoteTrim = 0.0f
                PlayfieldColor = keymode_settings.ColourΔ.[0]
                ColumnWidth = 1080f / 512f * float32 keymode_settings.ColumnWidth.[0]
                AnimationFrameTime = 1000.0 / 60.0
                UseRotation = is_arrows
                EnableStageTextures = stage_textures
                EnableColumnLight = columnlighting
            }

        JSON.ToFile (Path.Combine(target, "noteskin.json"), false) config
