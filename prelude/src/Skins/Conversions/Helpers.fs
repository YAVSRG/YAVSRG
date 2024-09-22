namespace Prelude.Skins.Conversions

open System.IO

[<AutoOpen>]
module IdentifySkinImport =

    let (|OsuSkinArchive|OsuSkinFolder|InterludeSkinArchive|StepmaniaNoteskinFolder|Unknown|) (path: string) =
        if Directory.Exists path then
            if File.Exists(Path.Combine(path, "skin.ini")) then
                OsuSkinFolder
            elif File.Exists(Path.Combine(path, "metrics.ini")) then
                StepmaniaNoteskinFolder
            else
                Unknown
        else
            let s = Path.GetExtension(path).ToLower()

            match s with
            | ".isk" -> InterludeSkinArchive
            | ".osk" -> OsuSkinArchive
            | _ -> Unknown
