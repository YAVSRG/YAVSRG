namespace Prelude.Skins.Conversions.Osu

open System
open System.Globalization
open System.IO
open Prelude
open Prelude.Formats.Osu

//https://osu.ppy.sh/wiki/en/Skinning/skin.ini#[mania]

[<AutoOpen>]
module OsuSkinHelpers =

    // todo: actually verify what osu does in various cases for colors
    let rgb_or (default_color: Color) (color: string) : Color =
        let values = color.Split(',')

        let inline color_component (i: int) =
            values.ValueAt(i).ParseInt().DefaultValue(255) |> min 255 |> max 0

        if values.Length >= 3 then
            Color.FromArgb(color_component(0), color_component(1), color_component(2))
        else
            default_color

    let rgba_or (default_color: Color) (color: string) : Color =
        let values = color.Split(',')

        let inline color_component (i: int) =
            values.ValueAt(i).ParseInt().DefaultValue(255) |> min 255 |> max 0

        if values.Length >= 3 then
            Color.FromArgb(color_component(3), color_component(0), color_component(1), color_component(2))
        else
            default_color

    let parse_comma_ints_or (default_value: int list) (s: string) : int list =
        s.Split(',', StringSplitOptions.TrimEntries)
        |> Seq.choose(fun s ->
            match Single.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some(int v)
            | false, _ -> None
        )
        |> List.ofSeq
        |> function
            | [] -> default_value
            | xs -> xs

    let key_texture (keycount: int) (key: int) =
        match keycount with
        | 1 -> "S"
        | 2 -> "1"
        | 3 -> [| "1"; "S"; "1" |].[key]
        | 4 -> [| "1"; "2"; "2"; "1" |].[key]
        | 5 -> [| "1"; "2"; "S"; "2"; "1" |].[key]
        | 6 -> [| "1"; "2"; "1"; "1"; "2"; "1" |].[key]
        | 7 -> [| "1"; "2"; "1"; "S"; "1"; "2"; "1" |].[key]
        | 8 -> [| "1"; "2"; "1"; "2"; "2"; "1"; "2"; "1" |].[key]
        | 9 -> [| "1"; "2"; "1"; "2"; "S"; "2"; "1"; "2"; "1" |].[key]
        | 10 -> [| "1"; "2"; "1"; "2"; "1"; "1"; "2"; "1"; "2"; "1" |].[key]
        | _ -> ""

type General =
    {
        Name: string
        Author: string
        Version: string
        AnimationFramerate: int
        AllowSliderBallTint: bool
        ComboBurstRandom: bool
        CursorCentre: bool
        CursorExpand: bool
        CursorRotate: bool
        CursorTrailRotate: bool
        CustomComboBurstSounds: int list
        HitCircleOverlayAboveNumber: bool
        LayeredHitSounds: bool
        SliderBallFlip: bool
        SpinnerFadePlayfield: bool
        SpinnerFrequencyModulate: bool
        SpinnerNoBlink: bool
    }

    static member Default: General =
        {
            Name = ""
            Author = ""
            Version = "1.0"
            AnimationFramerate = -1
            AllowSliderBallTint = false
            ComboBurstRandom = false
            CursorCentre = true
            CursorExpand = true
            CursorRotate = true
            CursorTrailRotate = true
            CustomComboBurstSounds = [ 30; 60; 90; 120; 240; 480 ] //not default but cant find it
            HitCircleOverlayAboveNumber = true
            LayeredHitSounds = true
            SliderBallFlip = true
            SpinnerFadePlayfield = false
            SpinnerFrequencyModulate = true
            SpinnerNoBlink = false
        }

    static member FromMap(properties: Map<string, string>) : General =
        {
            Name = properties.StringOrDefault("Name", "")
            Author = properties.StringOrDefault("Author", "")
            Version = properties.StringOrDefault("Version", "")
            AnimationFramerate = properties.IntOrDefault("AnimationFramerate", -1)
            AllowSliderBallTint = properties.IntOrDefault("AllowSliderBallTint", 0) <> 0
            ComboBurstRandom = properties.IntOrDefault("ComboBurstRandom", 0) <> 0
            CursorCentre = properties.IntOrDefault("CursorCentre", 1) <> 0
            CursorExpand = properties.IntOrDefault("CursorExpand", 1) <> 0
            CursorRotate = properties.IntOrDefault("CursorRotate", 1) <> 0
            CursorTrailRotate = properties.IntOrDefault("CursorTrailRotate", 1) <> 0
            CustomComboBurstSounds =
                properties.StringOrDefault("CustomComboBurstSounds", "")
                |> fun s -> s.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                |> Seq.choose(fun s ->
                    match Int32.TryParse(s, CultureInfo.InvariantCulture) with
                    | true, v -> Some v
                    | false, _ -> None
                )
                |> List.ofSeq
            HitCircleOverlayAboveNumber = properties.IntOrDefault("HitCircleOverlayAboveNumber", 1) <> 0
            LayeredHitSounds = properties.IntOrDefault("LayeredHitSounds", 1) <> 0
            SliderBallFlip = properties.IntOrDefault("SliderBallFlip", 1) <> 0
            SpinnerFadePlayfield = properties.IntOrDefault("SpinnerFadePlayfield", 0) <> 0
            SpinnerFrequencyModulate = properties.IntOrDefault("SpinnerFrequencyModulate", 1) <> 0
            SpinnerNoBlink = properties.IntOrDefault("SpinnerNoBlink", 0) <> 0
        }

type Colours =
    {
        Combo1: Color option
        Combo2: Color option
        Combo3: Color option
        Combo4: Color option
        Combo5: Color option
        Combo6: Color option
        Combo7: Color option
        Combo8: Color option
        InputOverlayText: Color
        MenuGlow: Color
        SliderBall: Color
        SliderBorder: Color
        SliderTrackOverride: Color option
        SongSelectActiveText: Color
        SongSelectInactiveText: Color
        SpinnerBackground: Color
        StarBreakAdditive: Color
    }

    static member Default: Colours =
        {
            Combo1 = Some(Color.FromArgb(255, 192, 0))
            Combo2 = Some(Color.FromArgb(0, 202, 0))
            Combo3 = Some(Color.FromArgb(18, 124, 255))
            Combo4 = Some(Color.FromArgb(242, 24, 57))
            Combo5 = None
            Combo6 = None
            Combo7 = None
            Combo8 = None
            InputOverlayText = Color.Black
            MenuGlow = Color.FromArgb(0, 78, 155)
            SliderBall = Color.FromArgb(2, 170, 255)
            SliderBorder = Color.White
            SliderTrackOverride = None
            SongSelectActiveText = Color.Black
            SongSelectInactiveText = Color.White
            SpinnerBackground = Color.FromArgb(100, 100, 100)
            StarBreakAdditive = Color.FromArgb(255, 182, 193)
        }

    static member FromMap(properties: Map<string, string>) : Colours = Colours.Default // todo: parse the real info

type Fonts =
    {
        HitCirclePrefix: string
        HitCircleOverlap: int
        ScorePrefix: string
        ScoreOverlap: int
        ComboPrefix: string
        ComboOverlap: int
    }

    static member Default =
        {
            HitCirclePrefix = "default"
            HitCircleOverlap = -2
            ScorePrefix = "score"
            ScoreOverlap = -2
            ComboPrefix = "score"
            ComboOverlap = -2
        }

    static member FromMap(properties: Map<string, string>) : Fonts =
        {
            HitCirclePrefix = properties.StringOrDefault("HitCirclePrefix", "default")
            HitCircleOverlap = properties.IntOrDefault("HitCircleOverlap", -2)
            ScorePrefix = properties.StringOrDefault("ScorePrefix", "score")
            ScoreOverlap = properties.IntOrDefault("ScoreOverlap", -2)
            ComboPrefix = properties.StringOrDefault("ComboPrefix", "score")
            ComboOverlap = properties.IntOrDefault("ComboOverlap", -2)
        }

type CatchTheBeat =
    {
        HyperDash: Color
        HyperDashFruit: Color
        HyperDashAfterImage: Color
    }

    static member Default =
        { HyperDash = Color.Red; HyperDashFruit = Color.Red; HyperDashAfterImage = Color.Red }

    static member FromMap(properties: Map<string, string>) : CatchTheBeat = CatchTheBeat.Default // todo: parse the real info

type Mania =
    {
        Keys: int
        ColumnStart: int
        ColumnRight: int
        ColumnSpacing: int list
        ColumnWidth: int list
        ColumnLineWidth: int list
        BarlineHeight: float
        LightingNWidth: int list
        LightingLWidth: int list
        WidthForNoteHeightScale: int option
        HitPosition: int
        LightPosition: int
        ScorePosition: int
        ComboPosition: int
        JudgementLine: bool
        SpecialStyle: int
        ComboBurstStyle: int
        SplitStages: bool option
        StageSeparation: int
        SeparateScore: bool
        KeysUnderNotes: bool
        UpsideDown: bool
        KeyFlipWhenUpsideDown: bool
        KeyFlipWhenUpsideDownΔ: bool array
        NoteFlipWhenUpsideDown: bool
        KeyFlipWhenUpsideDownΔD: bool array
        NoteFlipWhenUpsideDownΔ: bool array
        NoteFlipWhenUpsideDownΔH: bool array
        NoteFlipWhenUpsideDownΔL: bool array
        NoteFlipWhenUpsideDownΔT: bool array
        NoteBodyStyle: int
        NoteBodyStyleΔ: int array
        ColourΔ: Color array
        ColourLightΔ: Color array
        ColourColumnLine: Color
        ColourBarline: Color
        ColourJudgementLine: Color
        ColourKeyWarning: Color
        ColourHold: Color
        ColourBreak: Color
        KeyImageΔ: string array
        KeyImageΔD: string array
        NoteImageΔ: string array
        NoteImageΔH: string array
        NoteImageΔL: string array
        NoteImageΔT: string array
        StageLeft: string
        StageRight: string
        StageBottom: string
        StageHint: string
        StageLight: string
        LightingN: string
        LightingL: string
        WarningArrow: string
        Hit0: string
        Hit50: string
        Hit100: string
        Hit200: string
        Hit300: string
        Hit300g: string
    }

    static member Default (keys: int) (version: decimal) =
        {
            Keys = keys
            ColumnStart = 136
            ColumnRight = 19
            ColumnSpacing = [ 0 ]
            ColumnWidth = [ 30 ]
            ColumnLineWidth = [ 2 ]
            BarlineHeight = 1.2
            LightingNWidth = []
            LightingLWidth = []
            WidthForNoteHeightScale = None
            HitPosition = 402
            LightPosition = 413
            ScorePosition = 80 // 80 isn't the correct default, can't find the true default value online
            ComboPosition = 180 // ^ likewise
            JudgementLine = false
            SpecialStyle = 0
            ComboBurstStyle = 1
            SplitStages = None
            StageSeparation = 40
            SeparateScore = true
            KeysUnderNotes = false
            UpsideDown = false
            KeyFlipWhenUpsideDown = true
            KeyFlipWhenUpsideDownΔ = Array.create keys true
            NoteFlipWhenUpsideDown = true
            KeyFlipWhenUpsideDownΔD = Array.create keys true
            NoteFlipWhenUpsideDownΔ = Array.create keys true
            NoteFlipWhenUpsideDownΔH = Array.create keys true
            NoteFlipWhenUpsideDownΔL = Array.create keys true
            NoteFlipWhenUpsideDownΔT = Array.create keys (version >= 2.5m)
            NoteBodyStyle = 1
            NoteBodyStyleΔ = Array.create keys 1
            ColourΔ = Array.create keys Color.Black
            ColourLightΔ = Array.create keys (Color.FromArgb(55, 255, 255))
            ColourColumnLine = Color.White
            ColourBarline = Color.White
            ColourJudgementLine = Color.White
            ColourKeyWarning = Color.Black
            ColourHold = Color.FromArgb(255, 191, 51, 255)
            ColourBreak = Color.Red
            KeyImageΔ = Array.init keys (fun i -> sprintf "mania-key%s" (key_texture keys i))
            KeyImageΔD = Array.init keys (fun i -> sprintf "mania-key%sD" (key_texture keys i))
            NoteImageΔ = Array.init keys (fun i -> sprintf "mania-note%s" (key_texture keys i))
            NoteImageΔH = Array.init keys (fun i -> sprintf "mania-note%sH" (key_texture keys i))
            NoteImageΔL = Array.init keys (fun i -> sprintf "mania-note%sL" (key_texture keys i))
            NoteImageΔT = Array.init keys (fun i -> sprintf "mania-note%sT" (key_texture keys i))
            StageLeft = "mania-stage-left"
            StageRight = "mania-stage-right"
            StageBottom = "mania-stage-bottom" // ?
            StageHint = "mania-stage-hint"
            StageLight = "mania-stage-light"
            LightingN = "lightingN"
            LightingL = "lightingL"
            WarningArrow = "" // ?
            Hit0 = "mania-hit0"
            Hit50 = "mania-hit50"
            Hit100 = "mania-hit100"
            Hit200 = "mania-hit200"
            Hit300 = "mania-hit300"
            Hit300g = "mania-hit300g"
        }

    static member FromMap(properties: Map<string, string>) : Mania =
        let keys = properties.IntOrDefault("Keys", 0)

        {
            Keys = keys
            ColumnStart = properties.IntOrDefault("ColumnStart", 136)
            ColumnRight = properties.IntOrDefault("ColumnRight", 19)
            ColumnSpacing = properties.StringOrDefault("ColumnSpacing", "") |> parse_comma_ints_or [ 0 ]
            ColumnWidth = properties.StringOrDefault("ColumnWidth", "") |> parse_comma_ints_or [ 30 ]
            ColumnLineWidth = properties.StringOrDefault("ColumnLineWidth", "") |> parse_comma_ints_or [ 2 ]
            BarlineHeight = properties.DoubleOrDefault("BarlineHeight", 1.2)
            LightingNWidth = properties.StringOrDefault("LightingNWidth", "") |> parse_comma_ints_or []
            LightingLWidth = properties.StringOrDefault("LightingLWidth", "") |> parse_comma_ints_or []
            WidthForNoteHeightScale = properties.TryInt("WidthForNoteHeightScale")
            HitPosition = properties.IntOrDefault("HitPosition", 402)
            LightPosition = properties.IntOrDefault("LightPosition", 413)
            ScorePosition = properties.IntOrDefault("ScorePosition", 80)
            ComboPosition = properties.IntOrDefault("ComboPosition", 180)
            JudgementLine = properties.IntOrDefault("JudgementLine", 0) <> 0
            SpecialStyle = properties.IntOrDefault("SpecialStyle", 0)
            ComboBurstStyle = properties.IntOrDefault("ComboBurstStyle", 1)
            SplitStages = properties.TryInt("SplitStages") |> Option.map((<>) 0)
            StageSeparation = properties.IntOrDefault("StageSeparation", 40)
            SeparateScore = properties.IntOrDefault("SeparateScore", 1) <> 0
            KeysUnderNotes = properties.IntOrDefault("KeysUnderNotes", 0) <> 0
            UpsideDown = properties.IntOrDefault("UpsideDown", 0) <> 0
            KeyFlipWhenUpsideDown = properties.IntOrDefault("KeyFlipWhenUpsideDown", 0) <> 0
            KeyFlipWhenUpsideDownΔ =
                Array.init keys (fun i -> properties.IntOrDefault((sprintf "KeyFlipWhenUpsideDown%i" i), 1) <> 0)
            NoteFlipWhenUpsideDown = properties.IntOrDefault("NoteFlipWhenUpsideDown", 1) <> 0
            KeyFlipWhenUpsideDownΔD =
                Array.init keys (fun i -> properties.IntOrDefault((sprintf "KeyFlipWhenUpsideDown%iD" i), 1) <> 0)
            NoteFlipWhenUpsideDownΔ =
                Array.init keys (fun i -> properties.IntOrDefault((sprintf "NoteFlipWhenUpsideDown%i" i), 1) <> 0)
            NoteFlipWhenUpsideDownΔH =
                Array.init keys (fun i -> properties.IntOrDefault((sprintf "NoteFlipWhenUpsideDown%iH" i), 1) <> 0)
            NoteFlipWhenUpsideDownΔL =
                Array.init keys (fun i -> properties.IntOrDefault((sprintf "NoteFlipWhenUpsideDown%iL" i), 1) <> 0)
            NoteFlipWhenUpsideDownΔT =
                Array.init keys (fun i -> properties.IntOrDefault((sprintf "NoteFlipWhenUpsideDown%iT" i), 1) <> 0)
            NoteBodyStyle = properties.IntOrDefault("NoteBodyStyle", 1)
            NoteBodyStyleΔ = Array.init keys (fun i -> properties.IntOrDefault((sprintf "NoteBodyStyle%i" i), 1))
            ColourΔ =
                Array.init
                    keys
                    (fun i -> properties.StringOrDefault((sprintf "Colour%i" (i + 1)), "") |> rgba_or Color.Black)
            ColourLightΔ =
                Array.init
                    keys
                    (fun i -> properties.StringOrDefault((sprintf "ColourLight%i" (i + 1)), "") |> rgba_or Color.White)
            ColourColumnLine = properties.StringOrDefault("ColourColumnLine", "") |> rgba_or Color.White
            ColourBarline = properties.StringOrDefault("ColourBarline", "") |> rgba_or Color.White
            ColourJudgementLine = properties.StringOrDefault("ColourJudgementLine", "") |> rgb_or Color.White
            ColourKeyWarning = properties.StringOrDefault("ColourKeyWarning", "") |> rgb_or Color.Black
            ColourHold = properties.StringOrDefault("ColourHold", "") |> rgba_or(Color.FromArgb(255, 191, 51))
            ColourBreak = properties.StringOrDefault("ColourBreak", "") |> rgb_or(Color.FromArgb(255, 0, 0))
            KeyImageΔ =
                Array.init
                    keys
                    (fun i ->
                        properties.StringOrDefault(
                            (sprintf "KeyImage%i" i),
                            (sprintf "mania-key%s" (key_texture keys i))
                        )
                    )
            KeyImageΔD =
                Array.init
                    keys
                    (fun i ->
                        properties.StringOrDefault(
                            (sprintf "KeyImage%iD" i),
                            (sprintf "mania-key%sD" (key_texture keys i))
                        )
                    )
            NoteImageΔ =
                Array.init
                    keys
                    (fun i ->
                        properties.StringOrDefault(
                            (sprintf "NoteImage%i" i),
                            (sprintf "mania-note%s" (key_texture keys i))
                        )
                    )
            NoteImageΔH =
                Array.init
                    keys
                    (fun i ->
                        properties.StringOrDefault(
                            (sprintf "NoteImage%iH" i),
                            (sprintf "mania-note%sH" (key_texture keys i))
                        )
                    )
            NoteImageΔL =
                Array.init
                    keys
                    (fun i ->
                        properties.StringOrDefault(
                            (sprintf "NoteImage%iL" i),
                            (sprintf "mania-note%sL" (key_texture keys i))
                        )
                    )
            NoteImageΔT =
                Array.init
                    keys
                    (fun i ->
                        properties.StringOrDefault(
                            (sprintf "NoteImage%iT" i),
                            (sprintf "mania-note%sT" (key_texture keys i))
                        )
                    )
            StageLeft = properties.StringOrDefault("StageLeft", "mania-stage-left")
            StageRight = properties.StringOrDefault("StageRight", "mania-stage-right")
            StageBottom = properties.StringOrDefault("StageBottom", "mania-stage-bottom")
            StageHint = properties.StringOrDefault("StageHint", "mania-stage-hint")
            StageLight = properties.StringOrDefault("StageLight", "mania-stage-light")
            LightingN = properties.StringOrDefault("LightingN", "lightingN")
            LightingL = properties.StringOrDefault("LightingL", "lightingL")
            WarningArrow = properties.StringOrDefault("WarningArrow", "")
            Hit0 = properties.StringOrDefault("Hit0", "mania-hit0")
            Hit50 = properties.StringOrDefault("Hit50", "mania-hit50")
            Hit100 = properties.StringOrDefault("Hit100", "mania-hit100")
            Hit200 = properties.StringOrDefault("Hit200", "mania-hit200")
            Hit300 = properties.StringOrDefault("Hit300", "mania-hit300")
            Hit300g = properties.StringOrDefault("Hit300g", "mania-hit300g")
        }

type SkinIni =
    {
        General: General
        Colours: Colours
        Fonts: Fonts
        CatchTheBeat: CatchTheBeat
        Mania: Mania list
    }

module SkinIni =

    [<Struct>]
    type private ParserState =
        | Nothing
        | Header
        | ManiaHeader

    let from_stream (stream: Stream) : SkinIni =
        use reader = new StreamReader(stream)

        let mutable state = Nothing
        let general = ref Map.empty
        let colours = ref Map.empty
        let fonts = ref Map.empty
        let ctb = ref Map.empty
        let mutable section_ref = general

        let mania_sections = ResizeArray<Mania>()

        let change_state (s: ParserState) =
            if state = ManiaHeader then
                mania_sections.Add(Mania.FromMap(section_ref.Value))

            state <- s

        while reader.Peek() >= 0 do
            let line = reader.ReadLine()
            let trimmed = line.Trim()

            match trimmed with
            | "" -> ()
            | _ when line.StartsWith("//") -> ()
            | "[General]" ->
                change_state Header
                section_ref <- general
            | "[Colours]" ->
                change_state Header
                section_ref <- colours
            | "[Fonts]" ->
                change_state Header
                section_ref <- fonts
            | "[CatchTheBeat]" ->
                change_state Header
                section_ref <- ctb
            | "[Mania]" ->
                change_state ManiaHeader
                section_ref <- ref Map.empty
            | _ ->

            match state with
            | Nothing -> ()
            | Header
            | ManiaHeader ->
                let parts = trimmed.Split([| ':' |], 2, StringSplitOptions.TrimEntries)

                if parts.Length = 2 then
                    if not(Map.containsKey parts.[0] section_ref.Value) then
                        section_ref.Value <- Map.add parts.[0] parts.[1] section_ref.Value

        change_state Nothing

        {
            General = General.FromMap(general.Value)
            Colours = Colours.FromMap(colours.Value)
            Fonts = Fonts.FromMap(fonts.Value)
            CatchTheBeat = CatchTheBeat.FromMap(ctb.Value)
            Mania = mania_sections |> List.ofSeq
        }

type SkinIni with

    static member FromStream(stream: Stream) : Result<SkinIni, string> =
        try
            Ok(SkinIni.from_stream stream)
        with err ->
            Error err.Message

    static member FromFile(path: string) : Result<SkinIni, string> =
        try
            use stream = File.OpenRead(path)
            Ok(SkinIni.from_stream stream)
        with err ->
            Error err.Message
