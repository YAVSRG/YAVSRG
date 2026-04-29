namespace Prelude.Skins.Conversions.Osu.Noteskins

open System.IO
open Percyqaz.Common
open Prelude.Skins.Conversions.Osu

module NoteskinConverter =

    let convert_to_noteskin (ini: SkinIni) (source: string) (target: string) (keymode: int) (is_arrows: bool) =

        if Directory.Exists target then
            failwith "a folder with this name already exists!"
        Directory.CreateDirectory target |> ignore

        let ctx = NoteskinConverterContext.Create(source, target, ini, keymode, is_arrows)
        Logging.Debug "Converting skin '%s', version %.1f" ini.General.Name ctx.Version

        let note_height_scale = NotesConverter.get_note_height_scale(ctx)
        let core_textures : ColumnTextures list = NotesConverter.get_core_textures(ctx)

        Logging.Debug "%i column variations to convert (Distinct by note/head/body/tail textures)" core_textures.Length

        NotesConverter.convert_elements(ctx, core_textures, note_height_scale)
        ReceptorsConverter.convert_receptors(ctx, core_textures, note_height_scale)
        JudgementLineConverter.convert_stage_hint(ctx)
        StageConverter.convert_stage_textures(ctx)
        ColumnLightingConverter.convert_column_lighting(ctx)
        ExplosionsConverter.convert_explosions(ctx)

        // Generate noteskin.json
        NoteskinConfigConverter.generate_noteskin_config(ctx)