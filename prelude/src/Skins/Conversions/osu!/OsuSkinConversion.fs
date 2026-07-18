namespace Prelude.Skins.Conversions.Osu

open System.IO
open System.IO.Compression
open Prelude
open Prelude.Skins
open Prelude.Skins.Conversions.Osu.Noteskins
open Prelude.Skins.Conversions.Osu.HUD

type OsuSkinConversion =
    { FileSystem: OsuSkinFileSystem; SkinIni: SkinIni }
    
    static member PrepareForConversion(fs: OsuSkinFileSystem) : Result<OsuSkinConversion, string> =
        match fs.Open("skin.ini") with
        | Ok stream ->
            match SkinIni.FromStream(stream) with
            | Ok skin_ini -> Ok { FileSystem = fs; SkinIni = skin_ini }
            | Error reason -> Error(sprintf "skin.ini didn't parse: %s" reason)
        | Error reason -> Error(sprintf "Error getting skin.ini: %s" reason)
        
    static member PrepareToConvertFolder(path: string) : Result<OsuSkinConversion, string> =
        try
            let fs = OsuSkinFolderFileSystem(path)
            OsuSkinConversion.PrepareForConversion(fs)
        with exn -> Error(sprintf "%s: %s" (exn.GetType().Name) exn.Message)
        
    static member PrepareToConvertZipArchive(zip_path: string) : Result<OsuSkinConversion, string> =
        try
            let zip = new ZipArchive(File.Open(zip_path, FileMode.Open), ZipArchiveMode.Read)
            let fs = OsuSkinZipFileSystem(zip)
            OsuSkinConversion.PrepareForConversion(fs)
        with exn -> Error(sprintf "%s: %s" (exn.GetType().Name) exn.Message)
        
    member this.PerformConversion(target_path: string, keymode: int, is_arrows: bool) : unit =
        NoteskinConverter.convert_to_noteskin(this.SkinIni, this.FileSystem, Path.Combine(target_path, "Noteskin"), keymode, is_arrows)
        HudConverter.convert_to_hud(this.SkinIni, this.FileSystem, Path.Combine(target_path, "HUD"), keymode)
        JSON.ToFile
            (Path.Combine(target_path, "skin.json"), false)
            {
                Name = this.SkinIni.General.Name
                Author = this.SkinIni.General.Author
                Editor = None
            }