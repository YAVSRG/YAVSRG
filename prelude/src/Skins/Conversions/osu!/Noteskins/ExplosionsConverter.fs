namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Skins
open Prelude.Skins.Conversions.Osu
            
module internal ExplosionsConverter =
    
    let convert_note_explosions(ctx: NoteskinConverterContext) : unit =
        try
            let images =
                TextureAnimationSearchResult
                    .Create(ctx.KeymodeSettings.LightingN, ctx.DefaultSettings.LightingN, ctx.FileSystem)
                    .Load(ctx.FileSystem)
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad_to_square max_dim (ImageOperations.remove_black_bg image)
                padded.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "noteexplosion" (i, 0)))

            ctx.NoteExplosionsScale <- Some (float32 max_dim / 49.0f)
        with err ->
            Logging.Warn "Error converting note explosions: %O" err

    let convert_hold_explosions(ctx: NoteskinConverterContext) : unit =
        try
            let images =
                TextureAnimationSearchResult
                    .Create(ctx.KeymodeSettings.LightingL, ctx.DefaultSettings.LightingL, ctx.FileSystem)
                    .Load(ctx.FileSystem)
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad_to_square max_dim (ImageOperations.remove_black_bg image)
                padded.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "holdexplosion" (i, 0)))

            ctx.HoldExplosionsScale <- Some (float32 max_dim / 49.0f)
        with err ->
            Logging.Warn "Error converting hold explosions: %O" err
            
    let convert_explosions(ctx: NoteskinConverterContext) : unit =
        convert_note_explosions(ctx)
        convert_hold_explosions(ctx)