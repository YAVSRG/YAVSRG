namespace Prelude.Data

open System
open System.IO
open System.IO.Compression
open System.Drawing

module Themes =
    
    type StorageType = Zip of ZipArchive | Folder of string

    type ThemeConfig = {
        JudgementColors: Color array
        JudgementNames: string array
        Font: string
        TextColor: Color
        SelectChart: Color
        DefaultAccentColor: Color
        OverrideAccentColor: bool
        PlayfieldColor: Color
        CursorSize: float32
    }

    type NoteSkinConfig = {
        UseRotation: bool
        Name: string
        FlipHoldTail: bool
        UseHoldTailTexture: bool
        ColumnWidth: float32
        ColumnLightTime: float
    }

    type TextureConfig = {
        Columns: int
        Rows: int
        Tiling: bool
    }

    type Theme(storage) =
        new(path: string) = Theme(Folder path)
        new(zip: ZipArchive) = Theme(Zip zip)

        member this.GetFile([<ParamArray>] path: string array) =
            let p = Path.Combine(path)
            match storage with
            | Zip z -> z.GetEntry(p.Replace(Path.DirectorySeparatorChar, '/')).Open()
            | Folder f -> File.OpenRead(Path.Combine(f, p)) :> Stream

        member this.CopyTo(targetPath) =
            if Directory.Exists(targetPath) then
                match storage with
                | Zip z -> z.ExtractToDirectory(targetPath)
                | Folder f -> failwith "nyi, do this manually for now"
            else //copying to zip
                failwith "nyi, do this manually for now"