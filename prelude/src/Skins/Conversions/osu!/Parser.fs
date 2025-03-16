namespace Prelude.Skins.Conversions.Osu

open System
open System.Globalization
open System.IO
open Prelude
open Prelude.Formats.Osu

//https://osu.ppy.sh/wiki/en/Skinning/skin.ini#[mania]

[<AutoOpen>]
module OsuSkinHelpers =

    let rgb_or (default_color: Color) (s: string) : Color =
        let split = s.Split(",", StringSplitOptions.TrimEntries)
        match split |> List.ofArray with
        | _ :: _ :: _ :: _ -> Color.FromArgb(CsvHelpers.int_or 0 255 split, CsvHelpers.int_or 1 255 split, CsvHelpers.int_or 2 255 split)
        | _ -> default_color

    let rgba_or (default_color: Color) (s: string) : Color =
        let split = s.Split(",", StringSplitOptions.TrimEntries)
        match split |> List.ofArray with
        | _ :: _ :: _ :: _ :: _ -> Color.FromArgb(CsvHelpers.int_or 3 255 split, CsvHelpers.int_or 0 255 split, CsvHelpers.int_or 1 255 split, CsvHelpers.int_or 2 255 split)
        | _ :: _ :: _ :: _ -> Color.FromArgb(CsvHelpers.int_or 0 255 split, CsvHelpers.int_or 1 255 split, CsvHelpers.int_or 2 255 split)
        | _ -> default_color

    let parse_comma_ints_or (default_value: int list) (s: string) : int list =
        s.Split(",", StringSplitOptions.TrimEntries)
        |> Seq.choose (fun s ->
            match Single.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some (int v)
            | false, _ -> None
        )
        |> List.ofSeq
        |> function [] -> default_value | xs -> xs

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
    static member FromMap (properties: Map<string, string>) =
        {
            Name = MapHelpers.string_or "Name" "" properties
            Author = MapHelpers.string_or "Author" "" properties
            Version = MapHelpers.string_or "Version" "" properties
            AnimationFramerate = MapHelpers.int_or "AnimationFramerate" -1 properties
            AllowSliderBallTint = MapHelpers.int_or "AllowSliderBallTint" 0 properties <> 0
            ComboBurstRandom = MapHelpers.int_or "ComboBurstRandom" 0 properties <> 0
            CursorCentre = MapHelpers.int_or "CursorCentre" 1 properties <> 0
            CursorExpand = MapHelpers.int_or "CursorExpand" 1 properties <> 0
            CursorRotate = MapHelpers.int_or "CursorRotate" 1 properties <> 0
            CursorTrailRotate = MapHelpers.int_or "CursorTrailRotate" 1 properties <> 0
            CustomComboBurstSounds =
                MapHelpers.string_or "CustomComboBurstSounds" "" properties
                |> fun s -> s.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                |> Seq.choose (fun s ->
                    match Int32.TryParse(s, CultureInfo.InvariantCulture) with
                    | true, v -> Some v
                    | false, _ -> None
                )
                |> List.ofSeq
            HitCircleOverlayAboveNumber = MapHelpers.int_or "HitCircleOverlayAboveNumber" 1 properties <> 0
            LayeredHitSounds = MapHelpers.int_or "LayeredHitSounds" 1 properties <> 0
            SliderBallFlip = MapHelpers.int_or "SliderBallFlip" 1 properties <> 0
            SpinnerFadePlayfield = MapHelpers.int_or "SpinnerFadePlayfield" 0 properties <> 0
            SpinnerFrequencyModulate = MapHelpers.int_or "SpinnerFrequencyModulate" 1 properties <> 0
            SpinnerNoBlink = MapHelpers.int_or "SpinnerNoBlink" 0 properties <> 0
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
            Combo1 = Some <| Color.FromArgb(255, 192, 0)
            Combo2 = Some <| Color.FromArgb(0, 202, 0)
            Combo3 = Some <| Color.FromArgb(18, 124, 255)
            Combo4 = Some <| Color.FromArgb(242, 24, 57)
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
    static member FromMap (properties: Map<string, string>) = Colours.Default // todo: parse the real info

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
    static member FromMap (properties: Map<string, string>) =
        {
            HitCirclePrefix = MapHelpers.string_or "HitCirclePrefix" "default" properties
            HitCircleOverlap = MapHelpers.int_or "HitCircleOverlap" -2 properties
            ScorePrefix = MapHelpers.string_or "ScorePrefix" "score" properties
            ScoreOverlap = MapHelpers.int_or "ScoreOverlap" -2 properties
            ComboPrefix = MapHelpers.string_or "ComboPrefix" "score" properties
            ComboOverlap = MapHelpers.int_or "ComboOverlap" -2 properties
        }

type CatchTheBeat =
    {
        HyperDash: Color
        HyperDashFruit: Color
        HyperDashAfterImage: Color
    }
    static member Default =
        {
            HyperDash = Color.Red
            HyperDashFruit = Color.Red
            HyperDashAfterImage = Color.Red
        }
    static member FromMap (properties: Map<string, string>) = CatchTheBeat.Default // todo: parse the real info

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
    static member FromMap (properties: Map<string, string>) =
        let keys = MapHelpers.int_or "Keys" 0 properties
        {
            Keys = keys
            ColumnStart = MapHelpers.int_or "ColumnStart" 136 properties
            ColumnRight = MapHelpers.int_or "ColumnRight" 19 properties
            ColumnSpacing = MapHelpers.string_or "ColumnSpacing" "" properties |> parse_comma_ints_or [0]
            ColumnWidth = MapHelpers.string_or "ColumnWidth" "" properties |> parse_comma_ints_or [30]
            ColumnLineWidth = MapHelpers.string_or "ColumnLineWidth" "" properties |> parse_comma_ints_or [2]
            BarlineHeight = MapHelpers.float_or "BarlineHeight" 1.2 properties
            LightingNWidth = MapHelpers.string_or "LightingNWidth" "" properties |> parse_comma_ints_or []
            LightingLWidth = MapHelpers.string_or "LightingLWidth" "" properties |> parse_comma_ints_or []
            WidthForNoteHeightScale = MapHelpers.int_opt "WidthForNoteHeightScale" properties
            HitPosition = MapHelpers.int_or "HitPosition" 402 properties
            LightPosition = MapHelpers.int_or "LightPosition" 413 properties
            ScorePosition = MapHelpers.int_or "ScorePosition" 80 properties
            ComboPosition = MapHelpers.int_or "ComboPosition" 180 properties
            JudgementLine = MapHelpers.int_or "JudgementLine" 0 properties <> 0
            SpecialStyle = MapHelpers.int_or "SpecialStyle" 0 properties
            ComboBurstStyle = MapHelpers.int_or "ComboBurstStyle" 1 properties
            SplitStages = MapHelpers.int_opt "SplitStages" properties |> Option.map ((<>) 0)
            StageSeparation = MapHelpers.int_or "StageSeparation" 40 properties
            SeparateScore = MapHelpers.int_or "SeparateScore" 1 properties <> 0
            KeysUnderNotes = MapHelpers.int_or "KeysUnderNotes" 0 properties <> 0
            UpsideDown = MapHelpers.int_or "UpsideDown" 0 properties <> 0
            KeyFlipWhenUpsideDown = MapHelpers.int_or "KeyFlipWhenUpsideDown" 0 properties <> 0
            KeyFlipWhenUpsideDownΔ = Array.init keys (fun i -> MapHelpers.int_or (sprintf "KeyFlipWhenUpsideDown%i" i) 1 properties <> 0)
            NoteFlipWhenUpsideDown = MapHelpers.int_or "NoteFlipWhenUpsideDown" 1 properties <> 0
            KeyFlipWhenUpsideDownΔD = Array.init keys (fun i -> MapHelpers.int_or (sprintf "KeyFlipWhenUpsideDown%iD" i) 1 properties <> 0)
            NoteFlipWhenUpsideDownΔ = Array.init keys (fun i -> MapHelpers.int_or (sprintf "NoteFlipWhenUpsideDown%i" i) 1 properties <> 0)
            NoteFlipWhenUpsideDownΔH = Array.init keys (fun i -> MapHelpers.int_or (sprintf "NoteFlipWhenUpsideDown%iH" i) 1 properties <> 0)
            NoteFlipWhenUpsideDownΔL = Array.init keys (fun i -> MapHelpers.int_or (sprintf "NoteFlipWhenUpsideDown%iL" i) 1 properties <> 0)
            NoteFlipWhenUpsideDownΔT = Array.init keys (fun i -> MapHelpers.int_or (sprintf "NoteFlipWhenUpsideDown%iT" i) 1 properties <> 0)
            NoteBodyStyle = MapHelpers.int_or "NoteBodyStyle" 1 properties
            NoteBodyStyleΔ = Array.init keys (fun i -> MapHelpers.int_or (sprintf "NoteBodyStyle%i" i) 1 properties)
            ColourΔ = Array.init keys (fun i -> MapHelpers.string_or (sprintf "Colour%i" (i + 1)) "" properties |> rgba_or Color.Black)
            ColourLightΔ = Array.init keys (fun i -> MapHelpers.string_or (sprintf "ColourLight%i" (i + 1)) "" properties |> rgba_or Color.White)
            ColourColumnLine = MapHelpers.string_or "ColourColumnLine" "" properties |> rgba_or Color.White
            ColourBarline = MapHelpers.string_or "ColourBarline" "" properties |> rgba_or Color.White
            ColourJudgementLine = MapHelpers.string_or "ColourJudgementLine" "" properties |> rgb_or Color.White
            ColourKeyWarning = MapHelpers.string_or "ColourKeyWarning" "" properties |> rgb_or Color.Black
            ColourHold = MapHelpers.string_or "ColourHold" "" properties |> rgba_or (Color.FromArgb(255, 191, 51))
            ColourBreak = MapHelpers.string_or "ColourBreak" "" properties |> rgb_or (Color.FromArgb(255, 0, 0))
            KeyImageΔ = Array.init keys (fun i -> MapHelpers.string_or (sprintf "KeyImage%i" i) (sprintf "mania-key%s" (key_texture keys i)) properties)
            KeyImageΔD = Array.init keys (fun i -> MapHelpers.string_or (sprintf "KeyImage%iD" i) (sprintf "mania-key%sD" (key_texture keys i)) properties)
            NoteImageΔ = Array.init keys (fun i -> MapHelpers.string_or (sprintf "NoteImage%i" i) (sprintf "mania-note%s" (key_texture keys i)) properties)
            NoteImageΔH = Array.init keys (fun i -> MapHelpers.string_or (sprintf "NoteImage%iH" i) (sprintf "mania-note%sH" (key_texture keys i)) properties)
            NoteImageΔL = Array.init keys (fun i -> MapHelpers.string_or (sprintf "NoteImage%iL" i) (sprintf "mania-note%sL" (key_texture keys i)) properties)
            NoteImageΔT = Array.init keys (fun i -> MapHelpers.string_or (sprintf "NoteImage%iT" i) (sprintf "mania-note%sT" (key_texture keys i)) properties)
            StageLeft = MapHelpers.string_or "StageLeft" "mania-stage-left" properties
            StageRight = MapHelpers.string_or "StageRight" "mania-stage-right" properties
            StageBottom = MapHelpers.string_or "StageBottom" "mania-stage-bottom" properties
            StageHint = MapHelpers.string_or "StageHint" "mania-stage-hint" properties
            StageLight = MapHelpers.string_or "StageLight" "mania-stage-light" properties
            LightingN = MapHelpers.string_or "LightingN" "lightingN" properties
            LightingL = MapHelpers.string_or "LightingL" "lightingL" properties
            WarningArrow = MapHelpers.string_or "WarningArrow" "" properties
            Hit0 = MapHelpers.string_or "Hit0" "mania-hit0" properties
            Hit50 = MapHelpers.string_or "Hit50" "mania-hit50" properties
            Hit100 = MapHelpers.string_or "Hit100" "mania-hit100" properties
            Hit200 = MapHelpers.string_or "Hit200" "mania-hit200" properties
            Hit300 = MapHelpers.string_or "Hit300" "mania-hit300" properties
            Hit300g = MapHelpers.string_or "Hit300g" "mania-hit300g" properties
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
            if state = ManiaHeader then mania_sections.Add(Mania.FromMap section_ref.Value)
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
                let parts = trimmed.Split([|':'|], 2, StringSplitOptions.TrimEntries)
                if parts.Length = 2 then
                    section_ref.Value <- Map.add parts.[0] parts.[1] section_ref.Value

        change_state Nothing

        {
            General = General.FromMap general.Value
            Colours = Colours.FromMap colours.Value
            Fonts = Fonts.FromMap fonts.Value
            CatchTheBeat = CatchTheBeat.FromMap ctb.Value
            Mania = mania_sections |> List.ofSeq
        }

type SkinIni with

    static member FromStream(stream: Stream) =
        try Ok (SkinIni.from_stream stream)
        with err -> Error err.Message

    static member FromFile(path: string) =
        try
            use stream = File.OpenRead(path)
            Ok (SkinIni.from_stream stream)
        with err -> Error err.Message