namespace Prelude.Skinning.Noteskins.Conversion

open System.IO

[<AutoOpen>]
module IdentifyFolder =

    let (|OsuSkinArchive|OsuSkinFolder|InterludeSkinArchive|Unknown|) (path: string) =
        if Directory.Exists path then
            if File.Exists(Path.Combine(path, "skin.ini")) then
                OsuSkinFolder
            else
                Unknown
        else
            let s = Path.GetExtension(path).ToLower()

            match s with
            | ".isk" -> InterludeSkinArchive
            | ".osk" -> OsuSkinArchive
            | _ -> Unknown
