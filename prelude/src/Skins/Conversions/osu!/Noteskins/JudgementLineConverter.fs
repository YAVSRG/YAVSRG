namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Skins
open Prelude.Skins.Conversions.Osu
        
module internal JudgementLineConverter =
    
    let convert_stage_hint(ctx: NoteskinConverterContext) : unit =
        try
            let stage_hint =
                TextureSearchResult.Create(ctx.KeymodeSettings.StageHint, ctx.DefaultSettings.StageHint, ctx.FileSystem)
                    .ThrowIfNotFound()
                    .Load(ctx.FileSystem)
                    .As2x
            let intended_height_interlude_px = float32 stage_hint.Height
            stage_hint.Save(Path.Combine(ctx.Target, TextureFileName.to_loose "judgementline" (0, 0)))
            ctx.JudgementLineScale <- intended_height_interlude_px / (float32 ctx.KeymodeSettings.ColumnWidth.[0] * 1080f / 480f)
            ctx.JudgementLine <- true
        with err ->
            Logging.Warn "Error converting stage hint to judgement line: %O" err