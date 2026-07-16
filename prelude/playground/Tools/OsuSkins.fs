module OsuSkins

open System.IO
open Percyqaz.Common
open Prelude.Data.Library.Imports
open Prelude.Skins.Conversions.Osu

let osu_skin_path = Path.Combine(OSU_SONG_FOLDER, "..", "Skins")

let main() =
    for skin in Directory.EnumerateDirectories(osu_skin_path) do
        
        let target_path = Path.Combine(".", "skin-converts", Path.GetFileName(skin))
        try Directory.Delete(target_path, true) with _ -> ()
        
        let skin_fs = OsuSkinFileSystem(skin)
        
        match OsuSkinConverter.check_before_convert skin_fs with
        | Ok(skin_ini) ->
            Logging.Info "Converting %s [%s]" skin skin_ini.General.Name
            
            try
                OsuSkinConverter.convert_to_skin(skin_ini, skin_fs, target_path, 4, false)
            with err ->
                Logging.Error "%s\n%s" err.Message err.StackTrace
            
        | Error reason ->
            Logging.Error "'%s': %s" skin reason