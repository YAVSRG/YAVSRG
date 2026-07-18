namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Skins
open Prelude.Skins.Conversions.Osu
            
module internal ExplosionsConverter =
    
    [<Literal>]
    let OSU_EXPLOSION_SIZE_AT_2X = 98.0f
    
    let convert_note_explosions(ctx: NoteskinConverterContext) : unit =
        try
            let images =
                ctx.FileSystem
                    .SearchForAnimation(ctx.KeymodeSettings.LightingN, ctx.DefaultSettings.LightingN)
                    .Load(ctx.FileSystem)
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad_to_square max_dim (ImageOperations.remove_black_bg image)
                padded.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "noteexplosion" (i, 0)))

            ctx.NoteExplosionsScale <- Some (float32 max_dim / OSU_EXPLOSION_SIZE_AT_2X)
        with err ->
            Logging.Warn "Error converting note explosions: %O" err

    let convert_hold_explosions(ctx: NoteskinConverterContext) : unit =
        try
            let images =
                ctx.FileSystem
                    .SearchForAnimation(ctx.KeymodeSettings.LightingL, ctx.DefaultSettings.LightingL)
                    .Load(ctx.FileSystem)
                |> List.map _.As2x

            let max_dim = images |> List.map (fun i -> max i.Width i.Height) |> List.max

            for i, image in List.indexed images do
                let padded = ImageOperations.pad_to_square max_dim (ImageOperations.remove_black_bg image)
                padded.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "holdexplosion" (i, 0)))

            ctx.HoldExplosionsScale <- Some (float32 max_dim / OSU_EXPLOSION_SIZE_AT_2X)
        with err ->
            Logging.Warn "Error converting hold explosions: %O" err
            
    let convert_explosions(ctx: NoteskinConverterContext) : unit =
        convert_note_explosions(ctx)
        convert_hold_explosions(ctx)