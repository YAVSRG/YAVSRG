namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Prelude
open Prelude.Skins
open Prelude.Skins.Conversions.Osu

module internal ReceptorsConverter =

    // Rescales a receptor image (based on if @2x or not) into a 1:1 aspect ratio of how it would show on screen instead
    let private scale_receptor (target_width: int, height: int, is_2x_res: bool, image: Bitmap) : Bitmap =
        let image = image.Clone()
        if is_2x_res then
            image.Mutate(fun img -> img.Resize(target_width, image.Height) |> ignore)
        else
            image.Mutate(fun img -> img.Resize(target_width, image.Height * 2) |> ignore)
        let new_image = new Bitmap(target_width, height)
        new_image.Mutate(fun img -> img.DrawImage(image, Point(0, 0), 1.0f) |> ignore)
        new_image

    // osu!mania skins use 'Keys' but commonly these are used as 'Receptors' like stepmania might have
    // to do this they have a square texture but stretched and warped so that osu's skinning system un-warps it to look right ingame
    // If we can detect this, treating them like receptors as they were intended will make for a better Interlude convert
    let private detect_square_receptors (sample_receptor: Bitmap) : (Bitmap -> Bitmap) option =
        // ! scale_receptor has been run on `sample_receptor` before processed here, so the aspect ratio has been corrected to 1:1 already
        
        // Strategy: Scan from the top of the image down the middle, looking for the first pixel with ~2% opacity or higher
        let mutable top = 0
        while top < sample_receptor.Height && sample_receptor.[sample_receptor.Width / 2, top].A < 5uy do
            top <- top + 1

        // Then similarly scan from the bottom of the image up, first pixel with ~2% opacity or higher
        let mutable bottom = sample_receptor.Height - 1
        while bottom > top && sample_receptor.[sample_receptor.Width / 2, bottom].A < 5uy do
            bottom <- bottom - 1

        // This estimates the height of the non-empty-space part of a key texture
        let height = bottom - top
        // If 0 (like if the texture is all transparent) or a high number, treat it like a 'key'
        // If height is comparable to width, this looks like a receptor! Return a transform that extracts just this square receptor part
        if height > sample_receptor.Width / 2 && height < sample_receptor.Width then
            Some (fun bmp ->
                let new_bmp = new Bitmap(sample_receptor.Width, sample_receptor.Width)
                new_bmp.Mutate(fun img -> img.DrawImage(bmp, Point((-bmp.Width + sample_receptor.Width) / 2, -top + (sample_receptor.Width - height) / 2), 1.0f) |> ignore)
                new_bmp
            )
        else None
        
    let get_distinct_receptors_raw(ctx: NoteskinConverterContext) : LoadedTexture list =
        let distinct_detection = ResizeArray<Texture option * Texture option>()
        let distinct = ResizeArray<Result<Texture, string list> * Result<Texture, string list>>()
        
        let not_pressed_images =
            Array.init ctx.Keymode (fun i ->
                Texture.find(ctx.KeymodeSettings.KeyImageΔ.[i], ctx.DefaultSettings.KeyImageΔ.[i], ctx.Source)
            )
            
        let pressed_images =
            Array.init ctx.Keymode (fun i ->
                Texture.find(ctx.KeymodeSettings.KeyImageΔD.[i], ctx.DefaultSettings.KeyImageΔD.[i], ctx.Source)
            )
            
        Array.zip not_pressed_images pressed_images
        |> Array.iteri (fun i (np, p) ->
            let f = Result.toOption np, Result.toOption p
            if not (distinct_detection.Contains f) then
                distinct_detection.Add f
                distinct.Add((np, p))
            ctx.ReceptorColors.[ctx.Keymode - 3].[i] <- distinct_detection.IndexOf f
        )

        distinct
        |> Seq.map (fun (not_pressed, pressed) -> [not_pressed; pressed])
        |> List.concat
        |> List.map Texture.load_single_texture
        
        // Textures are returned not-pressed/pressed interleaved e.g. Key1, Key1D, Key2, Key2D, KeyS, KeySD
    
    let convert_existing_receptors (ctx: NoteskinConverterContext) : unit =
        let raw_key_textures = get_distinct_receptors_raw(ctx)

        let max_height =
            raw_key_textures
            |> Seq.map (fun { Image = bmp; Is2x = is_2x } -> if is_2x then bmp.Height else bmp.Height * 2)
            |> Seq.max

        let width = float32 ctx.KeymodeSettings.ColumnWidth.[0] * 3.2f |> int

        let scaled_key_textures = 
            raw_key_textures
            |> Seq.map (fun { Image = bmp; Is2x = is_2x } -> scale_receptor (width, max_height, is_2x, bmp))
            |> Array.ofSeq

        match detect_square_receptors scaled_key_textures.[0] with
        | Some transform ->
            Logging.Debug "Detected square 'receptors' in osu! key textures, treating them as receptors"
            scaled_key_textures
            |> Seq.indexed
            |> Seq.iter (fun (i, img) -> (transform img).Save(Path.Combine(ctx.Target, TextureFileName.to_loose "receptor" (0, i))))
        | None ->
            Logging.Debug "Did not detect 'receptors' in osu! key textures, treating them as keys"
            scaled_key_textures
            |> Seq.indexed
            |> Seq.iter (fun (i, img) -> img.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "receptor" (0, i))))
            let column_width_osu_px = float32 ctx.KeymodeSettings.ColumnWidth.[0]
            let top_of_key_osu_px = float32 max_height / float32 width * column_width_osu_px
            let hitposition_osu_px = 480f - float32 ctx.KeymodeSettings.HitPosition
            ctx.KeyReceptors <- true
            ctx.ReceptorOffset <- (top_of_key_osu_px - hitposition_osu_px) / column_width_osu_px - 0.5f
            
    let generate_placeholder_receptors(ctx: NoteskinConverterContext, core_textures: ColumnTextures list, note_height_scale: float32) : unit =
        let receptor_base =
            core_textures.[if core_textures.Length > 1 then 1 else 0].Note
            |> Texture.load_animated_texture
            |> List.head
            |> _.Image
            |> ImageOperations.scale_y note_height_scale

        let not_pressed = ImageOperations.grayscale 0.5f receptor_base |> ImageOperations.pad_to_square receptor_base.Width
        let pressed = ImageOperations.grayscale 1.0f receptor_base |> ImageOperations.pad_to_square receptor_base.Width

        not_pressed.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "receptor" (0, 0)))
        pressed.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "receptor" (0, 1)))

    let convert_receptors (ctx: NoteskinConverterContext, core_textures: ColumnTextures list, note_height_scale: float32) : unit =
        try
            convert_existing_receptors(ctx)
        with err -> // todo: when does this happen? when is it intended to happen?
            Logging.Warn "Error converting keys to receptors: %O" err
            Logging.Debug "Attempting to generate placeholders from notes instead"
            try
                generate_placeholder_receptors(ctx, core_textures, note_height_scale)
            with err ->
                Logging.Warn "Error generating placeholder receptors from note textures: %O" err