namespace Prelude.Data.Themes

open System.IO
open System.IO.Compression
open Percyqaz.Common
open Percyqaz.Json
open Prelude.Common
open Prelude.Scoring

(*
    Default config values for themes, textures, noteskins, widget layouts
*)

[<Json.AutoCodec(false)>]
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
                    Logging.Debug "Problem with theme: PBColors should have exactly 4 colors"
                    ThemeConfig.Default.PBColors
                else this.PBColors
        }

[<Json.AutoCodec(false)>]
type WidgetConfig =
    {
        Enabled: bool; Float: bool
        Left: float32; LeftA: float32
        Top: float32; TopA: float32
        Right: float32; RightA: float32
        Bottom: float32; BottomA: float32
    }
    static member Default =
        {
            Enabled = false; Float = true
            Left = 0.0f; LeftA = 0.0f
            Top = 0.0f; TopA = 0.0f
            Right = 0.0f; RightA = 1.0f
            Bottom = 0.0f; BottomA = 1.0f
        }

module WidgetConfig =

    [<Json.AutoCodec(false)>]
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
    
    [<Json.AutoCodec(false)>]
    type HitMeter =
        {
            Position: WidgetConfig
            AnimationTime: float32
            Thickness: float32
            ShowGuide: bool
            ShowNonJudgements: bool
            ReleasesExtraHeight: float32
        }
        static member Default = 
            {
                Position =
                    { 
                        Enabled = true
                        Float = false
                        Left = -300.0f
                        LeftA = 0.5f
                        Top = 10.0f
                        TopA = 0.5f
                        Right = 300.0f
                        RightA = 0.5f
                        Bottom = 25.0f
                        BottomA = 0.5f
                    }
                AnimationTime = 1000.0f
                Thickness = 5.0f
                ShowGuide = true
                ShowNonJudgements = true
                ReleasesExtraHeight = 5.0f
            }

    [<Json.AutoCodec(false)>]
    type LifeMeter =
        {
            Position: WidgetConfig
            Horizontal: bool
            TipColor: Color
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
                TipColor = Color.FromArgb(100, Color.Red)
                FullColor = Color.White
                EmptyColor = Color.Red
            }

    [<Json.AutoCodec(false)>]
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

    [<Json.AutoCodec(false)>]
    type SkipButton =
        { Position: WidgetConfig }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
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

    //[<Json.AutoCodec(false)>]
    //type JudgementMeter =
    //    {
    //        Position: WidgetConfig
    //        AnimationTime: float32
    //        ShowRDMA: bool
    //    }
    //    static member Default = 
    //        {
    //            Position = 
    //                {
    //                    Enabled = true
    //                    Float = false
    //                    Left = -128.0f
    //                    LeftA = 0.5f
    //                    Top = 30.0f
    //                    TopA = 0.5f
    //                    Right = 128.0f
    //                    RightA = 0.5f
    //                    Bottom = 86.0f
    //                    BottomA = 0.5f
    //                }
    //            AnimationTime = 800.0f
    //            ShowRDMA = true
    //        }

    [<Json.AutoCodec(false)>]
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
                        Left = -15.0f
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

    [<Json.AutoCodec(false)>]
    type Pacemaker =
        { Position: WidgetConfig }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = -300.0f
                        LeftA = 0.5f
                        Top = -10.0f
                        TopA = 0.55f
                        Right = 300.0f
                        RightA = 0.5f
                        Bottom = 50.0f
                        BottomA = 0.55f
                    }
            }

    //song info
    //mod info
    //clock
    //judgement counts

type Theme(storage) as this =
    inherit Storage(storage)

    let mutable config : ThemeConfig = ThemeConfig.Default
    do config <- 
        match this.TryGetJson<ThemeConfig> (true, "theme.json") with
        | Some data -> data.Validate
        | None -> failwith "theme.json was missing or didn't load properly"

    member this.Config
        with set conf = config <- conf; this.WriteJson (config, "theme.json")
        and get () = config
        
    member this.GetTexture (name: string) : (Bitmap * TextureConfig) option =
        let name = 
            if 
                (name = "logo" || name = "rain")
                && (let dayOfYear = System.DateTime.Today.DayOfYear in dayOfYear < 5 || dayOfYear > 350)
            then name + "-winterlude" else name

        match this.LoadTexture (name, "Textures") with
        | Ok res -> res
        | Error err -> Logging.Error(sprintf "Error loading theme texture '%s': %s" name err.Message); None
            
    member this.GetRulesetTexture (name: string) : (Bitmap * TextureConfig) =
        match this.LoadTexture (name, "Rulesets") with
        | Ok (Some res) -> res
        | _ -> new Bitmap(1, 1), TextureConfig.Default // swallow error or missing file

    member this.GetRulesets() =
        seq {
            for config in this.GetFiles "Rulesets" do
                if Path.GetExtension(config).ToLower() = ".ruleset" then
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
    member this.SetGameplayConfig<'T> (name: string, value: 'T) = this.WriteJson<'T> (value, "Interface", "Gameplay", name + ".json")
        
    static member FromZipFile (file: string) = 
        let stream = File.OpenRead file
        new Theme(Zip (new ZipArchive(stream), Some file))
    static member FromZipStream (stream: Stream) = new Theme(Zip (new ZipArchive(stream), None))
    static member FromPath (path: string) = new Theme(Folder path)
    static member FromFolderName (name: string) = Theme.FromPath(getDataPath (Path.Combine ("Themes", name)))