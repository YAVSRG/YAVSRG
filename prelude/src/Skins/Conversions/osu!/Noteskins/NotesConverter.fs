namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Prelude
open Prelude.Skins
open Prelude.Skins.Conversions.Osu

module internal NotesConverter =
    
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
                
    [<Literal>]
    let LN_TAIL_SLICE_HEIGHT = 230 // Currently a best guess for what will preserve aspect ratio correctly, it's at least very close

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
                    Logging.Debug("Detected 'Percy LN' in skin (>2000 pixels high), splitting into a tail and body")
                    // Strategy: Scan from the top of the image down the middle, looking for the first pixel with ~2% opacity or higher
                    let mutable i = 0
                    while i < image.Height && image.[image.Width / 2, i].A < 5uy do
                        i <- i + 1
                    // Take a slice down from that starting point, use that as the tail
                    let percy_tail_texture = new Bitmap(width, LN_TAIL_SLICE_HEIGHT)
                    percy_tail_texture.Mutate(fun img ->
                        img
                            .DrawImage(image, Point((-image.Width + width) / 2, -i), 1.0f)
                            .Flip(FlipMode.Vertical)
                            .Resize(width, width)
                        |> ignore)
                    percy_tail_texture.Save(Path.Combine(target, TextureFileName.to_loose "holdtail" (column, row)))
                    percy_ln_fix <- true

        percy_ln_fix

    let private arrow_fix_4k (images: Bitmap list list) : Bitmap list list =
        // Rotate notes to all point down, assuming they are in the LDUR layout of DDR
        match images with
        | left :: down :: up :: [ right ] ->
            [
                left |> List.map (ImageOperations.rotate RotateMode.Rotate270)
                down
                up |> List.map (ImageOperations.rotate RotateMode.Rotate180)
                right |> List.map (ImageOperations.rotate RotateMode.Rotate90)
            ]
        | not_4k -> not_4k
        
    let convert_notes(ctx: NoteskinConverterContext, core_textures: ColumnTextures list, note_height_scale: float32) : unit =
        try
            core_textures
            |> List.map _.Note
            |> List.map _.Load(ctx.FileSystem)
            |> List.map (List.map _.As2x)
            |> List.map (List.map (ImageOperations.scale_y note_height_scale))
            |> if ctx.IsArrows then arrow_fix_4k else id
            |> convert_element_textures ctx.Target "note"
        with err ->
            Logging.Warn "Error converting note textures: %O" err
            
    let convert_hold_bodies(ctx: NoteskinConverterContext, core_textures: ColumnTextures list) : unit =
        try
            let percy_ln_detected =
                core_textures
                |> List.map _.Body
                |> List.map _.Load(ctx.FileSystem)
                |> List.map (List.map _.As2x)
                |> convert_hold_body_textures ctx.Target
            if percy_ln_detected then
                ctx.SkipTailConversion <- true
                ctx.FlipHoldTail <- true
        with err ->
            Logging.Warn "Error converting hold body textures: %O" err
            
    let convert_hold_heads(ctx: NoteskinConverterContext, core_textures: ColumnTextures list, note_height_scale: float32) : unit =
        try
            core_textures
            |> List.map _.Head
            |> List.map _.Load(ctx.FileSystem)
            |> List.map (List.map _.As2x)
            |> List.map (List.map (ImageOperations.scale_y note_height_scale))
            |> if ctx.IsArrows then arrow_fix_4k else id
            |> convert_element_textures ctx.Target "holdhead"
        with err ->
            Logging.Warn "Error converting hold head textures: %O" err
            ctx.SkipTailConversion <- true
            
    let convert_hold_tails(ctx: NoteskinConverterContext, core_textures: ColumnTextures list, note_height_scale: float32) : unit =
        if not ctx.SkipTailConversion then
            try
                core_textures
                |> List.map _.Tail
                |> List.map _.ThrowIfNotFound()
                |> List.map _.Load(ctx.FileSystem)
                |> List.map (List.map _.As2x)
                |> List.map (List.map (ImageOperations.scale_y note_height_scale))
                |> convert_element_textures ctx.Target "holdtail"
            with err -> // todo: if there is no texture, return an error instead of exception-oriented-programming
                Logging.Warn "Error in holdtail textures - Using hold head textures instead: %O" err

                core_textures
                |> List.map _.Head
                |> List.map _.Load(ctx.FileSystem)
                |> List.map (List.map _.As2x)
                |> List.map (List.map (ImageOperations.scale_y note_height_scale))
                |> if ctx.IsArrows then arrow_fix_4k else id
                |> convert_element_textures ctx.Target "holdtail"

                ctx.FlipHoldTail <- true
        else
            Logging.Debug("Skipping holdtail conversion - tail already generated by 'Percy LN' fix")
            
    let convert_elements(ctx: NoteskinConverterContext, core_textures: ColumnTextures list, note_height_scale: float32) : unit =
        convert_notes(ctx, core_textures, note_height_scale)
        convert_hold_bodies(ctx, core_textures)
        convert_hold_heads(ctx, core_textures, note_height_scale)
        convert_hold_tails(ctx, core_textures, note_height_scale)
        
    let get_note_height_scale(ctx: NoteskinConverterContext) : float32 =
        match ctx.KeymodeSettings.WidthForNoteHeightScale with
        | None -> 1.0f
        | Some h ->
            let s = float32 h / float32 ctx.KeymodeSettings.ColumnWidth.[0]
            Logging.Debug "WidthForNoteHeightScale is %.3fx column width, using that as scale" s
            s
            
    let get_core_textures(ctx: NoteskinConverterContext) : ColumnTextures list =
        let textures =
            Array.init ctx.Keymode (fun k ->
                {
                    Note = ctx.FileSystem.SearchForAnimation(ctx.KeymodeSettings.NoteImageΔ.[k], ctx.DefaultSettings.NoteImageΔ.[k])
                    Head = ctx.FileSystem.SearchForAnimation(ctx.KeymodeSettings.NoteImageΔH.[k], ctx.DefaultSettings.NoteImageΔH.[k])
                    Body = ctx.FileSystem.SearchForAnimation(ctx.KeymodeSettings.NoteImageΔL.[k], ctx.DefaultSettings.NoteImageΔL.[k])
                    Tail = ctx.FileSystem.SearchForAnimation(ctx.KeymodeSettings.NoteImageΔT.[k], ctx.DefaultSettings.NoteImageΔT.[k])
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