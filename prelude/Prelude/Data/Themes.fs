namespace Prelude.Data

open System
open System.IO
open System.IO.Compression
open System.Drawing
open Percyqaz.Json
open Prelude.Common

module Themes =

    //Default config values for themes, textures, noteskins, widget layouts

    type ThemeConfig = {
        JudgementColors: Color array
        JudgementNames: string array
        LampColors: Color array
        LampNames: string array
        GradeColors: Color array
        GradeThresholds: float array
        PBColors: Color array
        Font: string
        TextColor: Color
        SelectChart: Color
        DefaultAccentColor: Color
        OverrideAccentColor: bool
        PlayfieldColor: Color
        CursorSize: float32
    } with
        static member Default: ThemeConfig = {
            JudgementColors = [|Color.FromArgb(127, 127, 255); Color.FromArgb(0, 255, 255); Color.FromArgb(255, 255, 0); Color.FromArgb(255, 255, 0);
                Color.FromArgb(0, 255, 100); Color.FromArgb(0, 0, 255); Color.Fuchsia; Color.FromArgb(255, 127, 0); Color.FromArgb(255, 0, 0); |]
            JudgementNames = [|"Ridiculous"; "Marvellous"; "Perfect"; "OK"; "Great"; "Good"; "Bad"; "Not Good"; "Miss"|]
            LampColors = [|Color.White; Color.FromArgb(255, 160, 160); Color.FromArgb(160, 160, 160); Color.FromArgb(80, 255, 80); Color.FromArgb(160, 255, 160);
                Color.FromArgb(200, 160, 255); Color.FromArgb(255, 255, 80); Color.FromArgb(255, 255, 160); Color.FromArgb(255, 160, 255); Color.FromArgb(160, 255, 255)|]
            LampNames = [|"NONE"; "SINGLE DIGIT COMBO BREAKS"; "MISS FLAG"; "FULL COMBO"; "SINGLE DIGIT GREATS"; "BLACK FLAG"; "PERFECT FULL COMBO"; "SINGLE DIGIT PERFECTS"; "WHITE FLAG"; "MARVELLOUS FULL COMBO"|]
            GradeColors = [|Color.FromArgb(205, 205, 205); Color.FromArgb(246, 234, 128); Color.FromArgb(237, 205, 140); Color.FromArgb(127, 231, 139); Color.FromArgb(134, 227, 183);
                Color.FromArgb(148, 210, 180); Color.FromArgb(149, 193, 220); Color.FromArgb(163, 190, 207); Color.FromArgb(202, 153, 183); Color.FromArgb(194, 162, 182); Color.FromArgb(200, 163, 155)|]
            GradeThresholds = [|0.98995; 0.97995; 0.96995; 0.95995; 0.94995; 0.93995; 0.92995; 0.91995; 0.90995; 0.89995|]
            PBColors = [|Color.Transparent; Color.FromArgb(160, 255, 160); Color.FromArgb(160, 255, 255); Color.FromArgb(255, 160, 80)|]
            Font = "Akrobat Black"
            TextColor = Color.White
            SelectChart = Color.FromArgb(0, 180, 110)
            DefaultAccentColor = Color.FromArgb(0, 255, 160)
            OverrideAccentColor = false
            PlayfieldColor = Color.FromArgb(120, 0, 0, 0)
            CursorSize = 50.0f
        }

    type NoteSkinConfig = {
        UseRotation: bool
        Name: string
        FlipHoldTail: bool
        UseHoldTailTexture: bool
        ColumnWidth: float32
        HoldNoteTrim: float32
        PlayfieldAlignment: float32 * float32
    } with
        static member Default = {
            UseRotation = false
            Name = "?"
            FlipHoldTail = true
            UseHoldTailTexture = true
            ColumnWidth = 150.0f
            HoldNoteTrim = 0.0f
            PlayfieldAlignment =  0.5f, 0.5f
        }

    type TextureConfig = {
        Columns: int
        Rows: int
        Tiling: bool
    } with
        static member Default = {
            Columns = 1
            Rows = 1
            Tiling = true
        }

    type WidgetConfig = { Enabled: bool; Float: bool; Left: float32; LeftA: float32; Top: float32; TopA: float32; Right: float32; RightA: float32; Bottom: float32; BottomA: float32 }
    with
        static member Default = { Enabled = false; Float = true; Left = 0.0f; LeftA = 0.0f; Top = 0.0f; TopA = 0.0f; Right = 0.0f; RightA = 1.0f; Bottom = 0.0f; BottomA = 1.0f }
    module WidgetConfig =
        type AccuracyMeter = { Position: WidgetConfig; GradeColors: bool; ShowName: bool }
        with static member Default = { Position = { Enabled = true; Float = false; Left = -100.0f; LeftA = 0.5f; Top = 40.0f; TopA = 0.0f; Right = 100.0f; RightA = 0.5f; Bottom = 120.0f; BottomA = 0.0f }; GradeColors = true; ShowName = true }
        type HitMeter = { Position: WidgetConfig; AnimationTime: float32; Thickness: float32; ShowGuide: bool }
        with static member Default = { Position = { Enabled = true; Float = false; Left = -300.0f; LeftA = 0.5f; Top = 0.0f; TopA = 0.5f; Right = 300.0f; RightA = 0.5f; Bottom = 25.0f; BottomA = 0.5f }; AnimationTime = 1000.0f; Thickness = 5.0f; ShowGuide = true }
        type Combo = { Position: WidgetConfig; Growth: float32; Pop: float32; LampColors: bool }
        with static member Default = { Position = { Enabled = true; Float = false; Left = -100.0f; LeftA = 0.5f; Top = -10.0f; TopA = 0.45f; Right = 100.0f; RightA = 0.5f; Bottom = 50.0f; BottomA = 0.45f }; Growth = 0.01f; Pop = 5.0f; LampColors = true }
        type JudgementMeter = { Position: WidgetConfig; AnimationTime: float }
        type Banner = { Position: WidgetConfig; AnimationTime: float }
        type SkipButton = { Position: WidgetConfig }
        type ProgressBar = { Position: WidgetConfig }
        //quick settings
        //song info
        //mod info
        //current time
        //judgement counts
        //screencovers
        //pacemaker
        type HitLighting = { AnimationTime: float; Expand: float32 }
        type ColumnLighting = { AnimationTime: float }

    //Basic theme I/O stuff. Additional implementation in Interlude for texture-specific things that depend on Interlude

    type StorageType = Zip of ZipArchive | Folder of string

    type Theme(storage) =

        member this.GetFile([<ParamArray>] path: string array) =
            let p = Path.Combine(path)
            try
                match storage with
                | Zip z -> z.GetEntry(p.Replace(Path.DirectorySeparatorChar, '/')).Open()
                | Folder f -> File.OpenRead(Path.Combine(f, p)) :> Stream
            with
            |  err -> reraise()

        member this.GetFiles([<ParamArray>] path: string array) =
            let p = Path.Combine(path)
            match storage with
            | Zip z ->
                let p = p.Replace(Path.DirectorySeparatorChar, '/')
                seq {
                    for e in z.Entries do
                        if e.FullName = p + "/" + e.Name && Path.HasExtension(e.Name) then yield e.Name
                }
            | Folder f -> Directory.EnumerateFiles(p)

        member this.GetFolders([<ParamArray>] path: string array) =
            let p = Path.Combine(path)
            match storage with
            | Zip z ->
                let p = p.Replace(Path.DirectorySeparatorChar, '/')
                seq {
                    for e in z.Entries do
                        if (e.Name = "" && e.FullName.Length > p.Length) then
                            let s = (e.FullName.Substring(p.Length + 1)).Split('/')
                            if e.FullName = p + "/" + s.[0] + "/" then yield s.[0]
                }
            | Folder f -> Directory.EnumerateDirectories(p)
        
        //todo: pair with boolean marking if this is defaulting - if so can inherit from other themes
        member this.GetJson([<ParamArray>] path: string array) =
            try
                use stream = this.GetFile(path)
                use tr = new StreamReader(stream)
                let json = tr.ReadToEnd() |> Json.fromString |> Json.JsonResult.valueOrRaise
                match storage with
                | Zip _ -> () //do not write data to zip archives
                | Folder f -> Json.toFile(Path.Combine(f, Path.Combine(path)), true) json
                json
            with err -> 
                Logging.Debug("Defaulting on json file: " + String.concat "/" path) (err.ToString())
                "{}" |> Json.fromString |> Json.JsonResult.valueOrRaise

        member this.CopyTo(targetPath) =
            if Directory.Exists(targetPath) then
                match storage with
                | Zip z -> z.ExtractToDirectory(targetPath)
                | Folder f -> failwith "nyi, do this manually for now"
            else //copying to zip
                failwith "nyi, do this manually for now"