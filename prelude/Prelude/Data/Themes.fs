namespace Prelude.Data

open System
open System.IO
open System.IO.Compression
open Prelude.Common
open Prelude.Scoring
open Prelude.Gameplay.NoteColors

module Themes =

    (*
        Default config values for themes, textures, noteskins, widget layouts
    *)

    type ThemeConfig = 
        {
            Name: string
            ClearColors: Color * Color
            PBColors: Color array
            Font: string
            DefaultAccentColor: Color
            OverrideAccentColor: bool
            CursorSize: float32
        } 
        static member Default : ThemeConfig = 
            {
                Name = "Unnamed Theme"
                ClearColors = (Color.FromArgb(255, 127, 255, 180), Color.FromArgb(255, 255, 160, 140))
                PBColors = 
                    [|
                        Color.Transparent
                        Color.FromArgb(160, 255, 160)
                        Color.FromArgb(160, 255, 255)
                        Color.FromArgb(255, 160, 80)
                    |]
                Font = "Interlude"
                DefaultAccentColor = Color.FromArgb(0, 160, 200)
                OverrideAccentColor = false
                CursorSize = 50.0f
            }
        member this.Validate : ThemeConfig =
            { this with
                PBColors = 
                    if this.PBColors.Length <> 4 then
                        Logging.Debug ("Problem with theme: PBColors should have exactly 4 colors")
                        ThemeConfig.Default.PBColors
                    else this.PBColors
            }

    type TextureConfig =
        {
            Columns: int
            Rows: int
            Tiling: bool
        }   
        static member Default =
            {
                Columns = 1
                Rows = 1
                Tiling = true
            }

    type WidgetConfig =
        {
            Enabled: bool
            Float: bool
            Left: float32
            LeftA: float32
            Top: float32
            TopA: float32
            Right: float32
            RightA: float32
            Bottom: float32
            BottomA: float32
        }
        static member Default =
            {
                Enabled = false
                Float = true
                Left = 0.0f
                LeftA = 0.0f
                Top = 0.0f
                TopA = 0.0f
                Right = 0.0f
                RightA = 1.0f
                Bottom = 0.0f
                BottomA = 1.0f
            }

    module WidgetConfig =
        type AccuracyMeter = 
            { 
                Position: WidgetConfig
                GradeColors: bool
                ShowName: bool
            }
            static member Default = 
                {
                    Position =
                        { 
                            Enabled = true
                            Float = false
                            Left = -100.0f
                            LeftA = 0.5f
                            Top = 40.0f
                            TopA = 0.0f
                            Right = 100.0f
                            RightA = 0.5f
                            Bottom = 120.0f
                            BottomA = 0.0f
                        }
                    GradeColors = true
                    ShowName = true
                }

        type HitMeter =
            {
                Position: WidgetConfig
                AnimationTime: float32
                Thickness: float32
                ShowGuide: bool
            }
            static member Default = 
                {
                    Position =
                        { 
                            Enabled = true
                            Float = false
                            Left = -300.0f
                            LeftA = 0.5f
                            Top = 0.0f
                            TopA = 0.5f
                            Right = 300.0f
                            RightA = 0.5f
                            Bottom = 25.0f
                            BottomA = 0.5f
                        }
                    AnimationTime = 1000.0f
                    Thickness = 5.0f
                    ShowGuide = true
                }

        type LifeMeter =
            {
                Position: WidgetConfig
                Horizontal: bool
                EndColor: Color
                FullColor: Color
                EmptyColor: Color
            }
            static member Default =
                {
                    Position =
                        {
                            Enabled = false
                            Float = false
                            Left = 0.0f
                            LeftA = 1.0f
                            Top = 0.0f
                            TopA = 0.6f
                            Right = 20.0f
                            RightA = 1.0f
                            Bottom = 0.0f
                            BottomA = 1.0f
                        }
                    Horizontal = false
                    EndColor = Color.FromArgb(100, Color.Red)
                    FullColor = Color.White
                    EmptyColor = Color.Red
                }

        type Combo =
            {
                Position: WidgetConfig
                Growth: float32
                Pop: float32
                LampColors: bool
            }
            static member Default = 
                {
                    Position =
                        { 
                            Enabled = true
                            Float = false
                            Left = -100.0f
                            LeftA = 0.5f
                            Top = -10.0f
                            TopA = 0.45f
                            Right = 100.0f
                            RightA = 0.5f
                            Bottom = 50.0f
                            BottomA = 0.45f
                        }
                    Growth = 0.01f
                    Pop = 5.0f
                    LampColors = true
                }

        type SkipButton =
            { Position: WidgetConfig }
            static member Default =
                {
                    Position =
                        {
                            Enabled = true
                            Float = true
                            Left = -200.0f
                            LeftA = 0.5f
                            Top = 20.0f
                            TopA = 0.6f
                            Right = 200.0f
                            RightA = 0.5f
                            Bottom = 120.0f
                            BottomA = 0.6f
                        }
                }

        type JudgementMeter =
            {
                Position: WidgetConfig
                AnimationTime: float32
                ShowRDMA: bool
            }
            static member Default = 
                {
                    Position = 
                        {
                            Enabled = true
                            Float = false
                            Left = -128.0f
                            LeftA = 0.5f
                            Top = 30.0f
                            TopA = 0.5f
                            Right = 128.0f
                            RightA = 0.5f
                            Bottom = 86.0f
                            BottomA = 0.5f
                        }
                    AnimationTime = 800.0f
                    ShowRDMA = true
                }

        type ProgressMeter =
            { 
                Position: WidgetConfig
                BarHeight: float32
                BarColor: Color
                GlowSize: float32
                GlowColor: Color
            }
            static member Default =
                {
                    Position =
                        {
                            Enabled = true
                            Float = false
                            Left = -12.0f
                            LeftA = 0.0f
                            Top = 10.0f
                            TopA = 0.0f
                            Right = -5.0f
                            RightA = 0.0f
                            Bottom = -10.0f
                            BottomA = 1.0f
                        }
                    BarHeight = 15.0f
                    BarColor = Color.FromArgb(180, 0, 170, 255)
                    GlowSize = 2.0f
                    GlowColor = Color.FromArgb(100, 80, 190, 255)
                }
        
        type Explosions =
            {
                Scale: float32
                FadeTime: float32
                ExpandAmount: float32
                ExplodeOnMiss: bool
                AnimationFrameTime: float
            }
            static member Default =
                {
                    Scale = 1.0f
                    FadeTime = 1.0f
                    ExpandAmount = 0.15f
                    ExplodeOnMiss = false
                    AnimationFrameTime = 50.0
                }

        //song info
        //mod info
        //current real time
        //judgement counts
        //pacemaker
        
    let noteskinTextures = [|"note"; "noteexplosion"; "receptor"; "holdhead"; "holdbody"; "holdtail"; "holdexplosion"; "receptorlighting"|]
    let themeTextures = [|"background"; "rain"; "logo"; "cursor"|]
    let rulesetTextures = [|"judgement"; "grade-base"; "grade-lamp-overlay"; "grade-overlay"|]
        
    type NoteskinConfig =
        {
            Name: string
            Author: string
            Version: string

            /// Enables rotation for notes. Set this to true if your notes are arrows/should rotate depending on which column they are in
            UseRotation: bool
            /// Contains settings for the color scheme of notes
            NoteColors: ColorConfig

            /// Hold tail textures are oriented for upscroll. Set this to true if you want them to be flipped when not playing in downscroll mode.
            FlipHoldTail: bool
            /// Set to false if you want to use the `holdhead` texture for hold tails too
            UseHoldTailTexture: bool
            /// Visually shortens hold notes by the given number of pixels
            HoldNoteTrim: float32
            /// Sets the color that hold notes should turn when they are not being held
            DroppedHoldColor: Color

            /// Sets the color of the playfield behind notes
            PlayfieldColor: Color
            /// Sets the alignment of the playfield - 0.5, 0.5 lines up the middle of the playfield with the middle of the screen
            PlayfieldAlignment: float32 * float32
            /// Sets the width of columns, in pixels
            ColumnWidth: float32
            // todo: group every animation thing under an animations object
            /// ???
            ColumnLightTime: float32

            /// ???
            AnimationFrameTime: float
            /// Config for explosion animations
            Explosions: WidgetConfig.Explosions
        }
        static member Default =
            {
                Name = "Unnamed Noteskin"
                Author = "Unknown"
                Version = "1.0.0"
                UseRotation = false
                FlipHoldTail = true
                UseHoldTailTexture = true
                HoldNoteTrim = 0.0f
                PlayfieldColor = Color.FromArgb(120, 0, 0, 0)
                PlayfieldAlignment = 0.5f, 0.5f
                DroppedHoldColor = Color.FromArgb(180, 180, 180, 180)
                ColumnWidth = 150.0f
                ColumnLightTime = 0.4f
                AnimationFrameTime = 200.0
                Explosions = WidgetConfig.Explosions.Default
                NoteColors = ColorConfig.Default
            }
        member this.Validate =
            { this with
                NoteColors = this.NoteColors.Validate
            }

    (*
        Basic theme I/O stuff. Additional implementation in Interlude for texture-specific things that depend on Interlude
    *)

    type StorageType = Zip of ZipArchive * source: string option | Folder of string

    type StorageAccess(storage: StorageType) =

        member this.StorageType = storage

        member this.TryReadFile ([<ParamArray>] path: string array) =
            let p = Path.Combine(path)
            try
                match storage with
                | Zip (z, _) -> z.GetEntry(p.Replace(Path.DirectorySeparatorChar, '/')).Open() |> Some
                | Folder f ->
                    let p = Path.Combine(f, p)
                    File.OpenRead p :> Stream |> Some
            with
            | :? FileNotFoundException | :? DirectoryNotFoundException // File doesnt exist in folder storage
            | :? NullReferenceException -> None // File doesnt exist in zip storage
            | _ -> reraise()
        
        member this.GetFiles ([<ParamArray>] path: string array) =
            let p = Path.Combine(path)
            match storage with
            | Zip (z, _) ->
                let p = p.Replace(Path.DirectorySeparatorChar, '/')
                seq {
                    for e in z.Entries do
                        if e.FullName = p + "/" + e.Name && Path.HasExtension e.Name then yield e.Name
                }
            | Folder f ->
                let target = Path.Combine(f, p)
                Directory.CreateDirectory target |> ignore
                Directory.EnumerateFiles target |> Seq.map Path.GetFileName

        member this.GetFolders ([<ParamArray>] path: string array) =
            let p = Path.Combine path
            match storage with
            | Zip (z, _) ->
                let p = p.Replace (Path.DirectorySeparatorChar, '/')
                seq {
                    for e in z.Entries do
                        if e.Name = "" && e.FullName.Length > p.Length then
                            let s = e.FullName.Substring(p.Length + 1).Split('/')
                            if e.FullName = p + "/" + s.[0] + "/" then yield s.[0]
                }
            | Folder f ->
                let target = Path.Combine(f, p)
                Directory.CreateDirectory target |> ignore
                Directory.EnumerateDirectories target |> Seq.map Path.GetFileName

        /// Gets the specified JSON file, or returns None
        /// None is returned with silent fail if the file did not exist
        /// None is returned with an error logged if something goes wrong while reading the file
        member this.TryGetJson<'T> (writeBack: bool, [<ParamArray>] path: string array) : 'T option =
            let json =
                match this.TryReadFile path with
                | Some stream ->
                    use tr = new StreamReader(stream)
                    let result =
                        tr.ReadToEnd()
                        |> JSON.FromString<'T>
                        |> 
                            function
                            | Ok v -> Some v
                            | Error err -> 
                                Logging.Error (sprintf "Failed to load %s in user data" (String.concat "/" path), err)
                                None
                    stream.Dispose()
                    result
                | None -> None
            if writeBack && json.IsSome then this.WriteJson (json.Value, path)
            json

        /// Gets the specified JSON file, or returns the default instance
        /// File is not written back if there was a problem with an existing file
        member this.GetJsonOrDefault<'T> (writeBack: bool, [<ParamArray>] path: string array) : 'T =
            let json, faultInFile =
                match this.TryReadFile path with
                | Some stream ->
                    use tr = new StreamReader(stream)
                    let result =
                        tr.ReadToEnd()
                        |> JSON.FromString<'T>
                        |> 
                            function
                            | Ok v -> v, false
                            | Error err -> 
                                Logging.Error (sprintf "Error loading %s in user data (Will use default values)" (String.concat "/" path), err)
                                JSON.Default<'T>(), true
                    stream.Dispose()
                    result
                | None -> JSON.Default<'T>(), false
            if writeBack && not faultInFile then this.WriteJson (json, path)
            json

        member this.WriteJson<'T> (data: 'T, [<ParamArray>] path: string array) =
            match storage with
            | Zip _ -> () // Zip archive is read-only
            | Folder f ->
                let target = Path.Combine (f, Path.Combine path)
                target |> Path.GetDirectoryName |> Directory.CreateDirectory |> ignore
                JSON.ToFile(target, true) data

        member this.CopyTo targetPath =
            Directory.CreateDirectory targetPath |> ignore
            match storage with
            | Zip (z, _) -> z.ExtractToDirectory targetPath
            | Folder f -> failwith "NYI, do this manually for now"


    type Theme(storage) as this =
        inherit StorageAccess(storage)

        let mutable config : ThemeConfig = ThemeConfig.Default
        do config <- 
            match this.TryGetJson<ThemeConfig> (true, "theme.json") with
            | Some data -> data.Validate
            | None -> failwith "theme.json was missing or didn't load properly"

        member this.Config
            with set conf = config <- conf; this.WriteJson (config, "theme.json")
            and get () = config
        
        member this.GetTexture (name: string) : (Bitmap * TextureConfig) option =
            match this.TryReadFile ("Textures", name + ".png") with
            | Some stream ->
                let img = Bitmap.load stream
                let info : TextureConfig = this.GetJsonOrDefault<TextureConfig> (false, "Textures", name + ".json")
                stream.Dispose()
                Some (img, info)
            | None -> None
            
        member this.GetRulesetTexture (name: string) : (Bitmap * TextureConfig) =
            match this.TryReadFile ("Rulesets", name + ".png") with
            | Some stream ->
                let img = Bitmap.load stream
                let info : TextureConfig = this.GetJsonOrDefault<TextureConfig> (false, "Rulesets", name + ".json")
                stream.Dispose()
                img, info
            | None -> new Bitmap(1, 1), TextureConfig.Default

        member this.GetRulesets() =
            seq {
                for config in this.GetFiles "Rulesets" do
                    if Path.GetExtension(config).ToLower() = ".irs" then
                        match this.TryGetJson<Ruleset>(true, "Rulesets", config) with
                        | Some data -> yield Path.GetFileNameWithoutExtension config, data.Validate
                        | None -> () // Error has already been logged when this happens
            }

        member this.GetFonts() =
            seq {
                for file in this.GetFiles "Fonts" do
                    match Path.GetExtension(file).ToLower() with
                    | ".otf" | ".ttf" ->
                        match this.TryReadFile("Fonts", file) with
                        | Some s -> 
                            // Font loading requires seek
                            use ms = new MemoryStream()
                            s.CopyTo ms
                            ms.Position <- 0
                            yield ms; s.Dispose()
                        | None -> ()
                    | _ -> ()
            }

        member this.GetGameplayConfig<'T> (name: string) = this.GetJsonOrDefault<'T> (true, "Interface", "Gameplay", name + ".json")
        
        static member FromZipFile (file: string) = 
            let stream = File.OpenRead file
            new Theme(Zip (new ZipArchive(stream), Some file))
        static member FromZipStream (stream: Stream) = new Theme(Zip (new ZipArchive(stream), None))
        static member FromFolderName (name: string) = new Theme(Folder <| getDataPath (Path.Combine ("Themes", name)))

    type Noteskin(storage) as this =
        inherit StorageAccess(storage)
        
        let mutable config : NoteskinConfig = NoteskinConfig.Default
        do config <-
            match this.TryGetJson<NoteskinConfig> (true, "noteskin.json") with
            | Some data -> data.Validate
            | None -> failwith "noteskin.json was missing or didn't load properly"
        
        member this.Config
            with set conf = config <- conf; this.WriteJson (config, "noteskin.json")
            and get () = config
            
        member this.GetTexture (name: string) : (Bitmap * TextureConfig) option =
            match this.TryReadFile (name + ".png") with
            | Some stream ->
                let img = Bitmap.load stream
                let info : TextureConfig = this.GetJsonOrDefault<TextureConfig> (false, name + ".json")
                stream.Dispose()
                Some (img, info)
            | None -> None
        
        static member FromZipFile (file: string) = 
            let stream = File.OpenRead file
            new Noteskin(Zip (new ZipArchive(stream), Some file))
        static member FromZipStream (stream: Stream) = new Noteskin(Zip (new ZipArchive(stream), None))
        static member FromFolder (path: string) = new Noteskin(Folder path)