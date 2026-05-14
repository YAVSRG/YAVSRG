namespace Prelude.Skins.Conversions.Osu

open System.IO
open Prelude
open Prelude.Skins
open Prelude.Skins.Conversions.Osu.Noteskins
open Prelude.Skins.Conversions.Osu.HUD

module OsuSkinConverter =

    let check_before_convert (source: string) : Result<SkinIni, string> =
        SkinIni.FromFile (Path.Combine(source, "skin.ini"))

    let convert_to_skin (ini: SkinIni, source: string, target: string, keymode: int, is_arrows: bool) : unit =
        NoteskinConverter.convert_to_noteskin(ini, source, Path.Combine(target, "Noteskin"), keymode, is_arrows)
        HudConverter.convert_to_hud(ini, source, Path.Combine(target, "HUD"), keymode)
        JSON.ToFile
            (Path.Combine(target, "skin.json"), false)
            {
                Name = ini.General.Name
                Author = ini.General.Author
                Editor = None
            }