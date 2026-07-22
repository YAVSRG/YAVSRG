namespace Prelude.Skins.Conversions.Osu.HUD

open System.IO
open Prelude.Skins.Conversions.Osu

module HudConverter =

    let convert_to_hud (ini: SkinIni, fs: OsuSkinFileSystem, target: string, keymode: int) : unit =

        if Directory.Exists target then
            failwith "a folder with this name already exists!"
        Directory.CreateDirectory target |> ignore
        
        let ctx = HudConverterContext.Create(fs, target, ini, keymode)

        HudJudgementConverter.convert_judgement_textures(ctx)
        HudJudgementConverter.convert_judgement_counter_textures(ctx)
        HudFontConverter.convert_all_fonts(ctx)
        
        // Generate hud.json
        HudConfigConverter.convert_hud_config(ctx)