namespace Prelude.Content.Noteskins.Conversion

open Prelude.Common

//https://osu.ppy.sh/wiki/no/Skinning/skin.ini#[mania]
module SkinIni =

    open FParsec
    open Prelude.Charts.Formats.``osu!``

    let private parse_bool = int >> fun x -> x <> 0

    let private parse_rgb (s: string) : Color =
        match s.Split(",") |> List.ofArray |> List.map int with
        | r :: g :: b :: _ -> Color.FromArgb(r, g, b)
        | _ -> failwith "not enough numbers given for RGB"

    let private parse_rgba (s: string) : Color =
        match s.Split(",") |> List.ofArray |> List.map int with
        | r :: g :: b :: a :: _ -> Color.FromArgb(a, r, g, b)
        | r :: g :: b :: _ -> Color.FromArgb(255, r, g, b)
        | _ -> failwith "not enough numbers given for RGBa"

    let private parse_ints (s: string) =
        match run (spaces >>. (sepBy pint32 (pchar ',' .>> spaces))) s with
        | Success(xs, _, _) -> xs
        | Failure(e, _, _) -> failwith e

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

    let private parse_general ((title, settings): Header) : General =
        assert (title = "General")

        let f (s: General) (key, value) =
            match key with
            | "Name" -> { s with Name = value }
            | "Author" -> { s with Author = value }
            | "Version" -> { s with Version = value }
            | "AnimationFramerate" ->
                { s with
                    AnimationFramerate = int value
                }
            | "AllowSliderBallTint" ->
                { s with
                    AllowSliderBallTint = parse_bool value
                }
            | "ComboBurstRandom" ->
                { s with
                    ComboBurstRandom = parse_bool value
                }
            | "CursorCentre" ->
                { s with
                    CursorCentre = parse_bool value
                }
            | "CursorExpand" ->
                { s with
                    CursorExpand = parse_bool value
                }
            | "CursorRotate" ->
                { s with
                    CursorRotate = parse_bool value
                }
            | "CursorTrailRotate" ->
                { s with
                    CursorTrailRotate = parse_bool value
                }
            | "CustomComboBurstSounds" ->
                { s with
                    CustomComboBurstSounds = parse_ints value
                }
            | "HitCircleOverlayAboveNumer"
            | "HitCircleOverlayAboveNumber" ->
                { s with
                    HitCircleOverlayAboveNumber = parse_bool value
                }
            | "LayeredHitSounds" ->
                { s with
                    LayeredHitSounds = parse_bool value
                }
            | "SliderBallFlip" ->
                { s with
                    SliderBallFlip = parse_bool value
                }
            | "SpinnerFadePlayfield" ->
                { s with
                    SpinnerFadePlayfield = parse_bool value
                }
            | "SpinnerFrequencyModulate" ->
                { s with
                    SpinnerFrequencyModulate = parse_bool value
                }
            | "SpinnerNoBlink" ->
                { s with
                    SpinnerNoBlink = parse_bool value
                }
            | _ -> s

        List.fold f General.Default settings

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

    let private parse_colours ((title, settings): Header) : Colours = Colours.Default

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

    let private parse_fonts ((title, settings): Header) : Fonts = Fonts.Default

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

    let private parse_ctb ((title, settings): Header) : CatchTheBeat = CatchTheBeat.Default

    let mania_default_textures k format =
        match k with
        | 1 -> [| "S" |]
        | 2 -> [| "1"; "1" |]
        | 3 -> [| "1"; "S"; "1" |]
        | 4 -> [| "1"; "2"; "2"; "1" |]
        | 5 -> [| "1"; "2"; "S"; "2"; "1" |]
        | 6 -> [| "1"; "2"; "1"; "1"; "2"; "1" |]
        | 7 -> [| "1"; "2"; "1"; "S"; "1"; "2"; "1" |]
        | 8 -> [| "1"; "2"; "1"; "2"; "2"; "1"; "2"; "1" |]
        | 9 -> [| "1"; "2"; "1"; "2"; "S"; "2"; "1"; "2"; "1" |]
        | 10 -> [| "1"; "2"; "1"; "2"; "1"; "1"; "2"; "1"; "2"; "1" |]
        | _ -> Array.create k "1" //not supported
        |> Array.map (sprintf format)

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
            //LightFramePerSecond: unit // nobody knows what this does
            SpecialStyle: string
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
        static member Default k =
            {
                Keys = k
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
                SpecialStyle = "0"
                ComboBurstStyle = 1
                SplitStages = None
                StageSeparation = 40
                SeparateScore = true
                KeysUnderNotes = false
                UpsideDown = false
                KeyFlipWhenUpsideDown = true
                KeyFlipWhenUpsideDownΔ = Array.create k true
                NoteFlipWhenUpsideDown = true
                KeyFlipWhenUpsideDownΔD = Array.create k true
                NoteFlipWhenUpsideDownΔ = Array.create k true
                NoteFlipWhenUpsideDownΔH = Array.create k true
                NoteFlipWhenUpsideDownΔL = Array.create k true
                NoteFlipWhenUpsideDownΔT = Array.create k true
                NoteBodyStyle = 1
                NoteBodyStyleΔ = Array.create k 1
                ColourΔ = Array.create k Color.Black
                ColourLightΔ = Array.create k (Color.FromArgb(55, 255, 255))
                ColourColumnLine = Color.White
                ColourBarline = Color.White
                ColourJudgementLine = Color.White
                ColourKeyWarning = Color.Black
                ColourHold = Color.FromArgb(255, 191, 51, 255)
                ColourBreak = Color.Red
                KeyImageΔ = mania_default_textures k "mania-key%s"
                KeyImageΔD = mania_default_textures k "mania-key%sD"
                NoteImageΔ = mania_default_textures k "mania-note%s"
                NoteImageΔH = mania_default_textures k "mania-note%sH"
                NoteImageΔL = mania_default_textures k "mania-note%sL"
                NoteImageΔT = mania_default_textures k "mania-note%sT"
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

    let (|P_0|_|) (keys: int) (pre: string) (suff: string) (target: string) =
        match run (pstring pre >>. (pint32 .>> pstring suff) .>> eof) target with
        | Success(n, _, _) -> if 0 <= n && n < keys then Some n else None
        | Failure(_, _, _) -> None

    let (|P_1|_|) (keys: int) (pre: string) (suff: string) (target: string) =
        match run (pstring pre >>. (pint32 .>> pstring suff) .>> eof) target with
        | Success(n, _, _) -> if 0 < n && n <= keys then Some(n - 1) else None
        | Failure(_, _, _) -> None

    let private parse_mania ((title, settings): Header) : Mania =
        assert (title = "Mania")

        match settings with
        | ("Keys", keys) :: settings ->
            let keys = int keys

            let f s (key, value: string) =
                match key with
                | "ColumnStart" -> { s with ColumnStart = int value }
                | "ColumnRight" -> { s with ColumnRight = int value }
                | "ColumnSpacing" ->
                    { s with
                        ColumnSpacing = parse_ints value
                    }
                | "ColumnWidth" ->
                    { s with
                        ColumnWidth = parse_ints value
                    }
                | "ColumnLineWidth" ->
                    { s with
                        ColumnLineWidth = parse_ints value
                    }
                | "BarlineHeight" -> { s with BarlineHeight = float value }
                | "LightingNWidth" ->
                    { s with
                        LightingNWidth = parse_ints value
                    }
                | "LightingLWidth" ->
                    { s with
                        LightingLWidth = parse_ints value
                    }
                | "WidthForNoteHeightScale" ->
                    { s with
                        WidthForNoteHeightScale = Some(int value)
                    }
                | "HitPosition" -> { s with HitPosition = int value }
                | "LightPosition" -> { s with LightPosition = int value }
                | "ScorePosition" -> { s with ScorePosition = int value }
                | "ComboPosition" -> { s with ComboPosition = int value }
                | "JudgementLine" ->
                    { s with
                        JudgementLine = parse_bool value
                    }
                | "SpecialStyle" -> { s with SpecialStyle = value }
                | "ComboBurstStyle" -> { s with ComboBurstStyle = int value }
                | "SplitStages" ->
                    { s with
                        SplitStages = Some(parse_bool value)
                    }
                | "StageSeparation" -> { s with StageSeparation = int value }
                | "SeparateScore" ->
                    { s with
                        SeparateScore = parse_bool value
                    }
                | "KeysUnderNotes" ->
                    { s with
                        KeysUnderNotes = parse_bool value
                    }
                | "UpsideDown" -> { s with UpsideDown = parse_bool value }
                | "KeyFlipWhenUpsideDown" ->
                    { s with
                        KeyFlipWhenUpsideDown = parse_bool value
                    }
                | P_0 keys "KeyFlipWhenUpsideDown" "" n ->
                    s.KeyFlipWhenUpsideDownΔ.[n] <- parse_bool value
                    s
                | "NoteFlipWhenUpsideDown" ->
                    { s with
                        NoteFlipWhenUpsideDown = parse_bool value
                    }
                | P_0 keys "KeyFlipWhenUpsideDown" "D" n ->
                    s.KeyFlipWhenUpsideDownΔD.[n] <- parse_bool value
                    s
                | P_0 keys "NoteFlipWhenUpsideDown" "" n ->
                    s.NoteFlipWhenUpsideDownΔ.[n] <- parse_bool value
                    s
                | P_0 keys "NoteFlipWhenUpsideDown" "H" n ->
                    s.NoteFlipWhenUpsideDownΔH.[n] <- parse_bool value
                    s
                | P_0 keys "NoteFlipWhenUpsideDown" "L" n ->
                    s.NoteFlipWhenUpsideDownΔL.[n] <- parse_bool value
                    s
                | P_0 keys "NoteFlipWhenUpsideDown" "T" n ->
                    s.NoteFlipWhenUpsideDownΔT.[n] <- parse_bool value
                    s
                | "NoteBodyStyle" -> { s with NoteBodyStyle = int value }
                | P_0 keys "NoteBodyStyle" "" n ->
                    s.NoteBodyStyleΔ.[n] <- int value
                    s
                | P_1 keys "Colour" "" n ->
                    s.ColourΔ.[n] <- parse_rgba value
                    s
                | P_1 keys "ColourLight" "" n ->
                    s.ColourLightΔ.[n] <- parse_rgba value
                    s
                | "ColourColumnLine" ->
                    { s with
                        ColourColumnLine = parse_rgba value
                    }
                | "ColourBarline" ->
                    { s with
                        ColourBarline = parse_rgba value
                    }
                | "ColourJudgementLine" ->
                    { s with
                        ColourJudgementLine = parse_rgb value
                    }
                | "ColourKeyWarning" ->
                    { s with
                        ColourKeyWarning = parse_rgb value
                    }
                | "ColourHold" -> { s with ColourHold = parse_rgba value }
                | "ColourBreak" -> { s with ColourBreak = parse_rgb value }
                | P_0 keys "KeyImage" "" n ->
                    s.KeyImageΔ.[n] <- value
                    s
                | P_0 keys "KeyImage" "D" n ->
                    s.KeyImageΔD.[n] <- value
                    s
                | P_0 keys "NoteImage" "" n ->
                    s.NoteImageΔ.[n] <- value
                    s
                | P_0 keys "NoteImage" "H" n ->
                    s.NoteImageΔH.[n] <- value
                    s
                | P_0 keys "NoteImage" "L" n ->
                    s.NoteImageΔL.[n] <- value
                    s
                | P_0 keys "NoteImage" "T" n ->
                    s.NoteImageΔT.[n] <- value
                    s
                | "StageLeft" -> { s with StageLeft = value }
                | "StageRight" -> { s with StageRight = value }
                | "StageBottom" -> { s with StageBottom = value }
                | "StageHint" -> { s with StageHint = value }
                | "StageLight" -> { s with StageLight = value }
                | "LightingN" -> { s with LightingN = value }
                | "LightingL" -> { s with LightingL = value }
                | "WarningArrow" -> { s with WarningArrow = value }
                | "Hit0" -> { s with Hit0 = value }
                | "Hit50" -> { s with Hit50 = value }
                | "Hit100" -> { s with Hit100 = value }
                | "Hit200" -> { s with Hit200 = value }
                | "Hit300" -> { s with Hit300 = value }
                | "Hit300g" -> { s with Hit300g = value }
                | _ -> s

            List.fold f (Mania.Default keys) settings
        | _ -> failwith "mania block did not specify keycount"

    type OsuSkinIni =
        {
            General: General
            Colours: Colours
            Fonts: Fonts
            Mania: Mania list
        }

    let private skin_ini_parser =
        tuple5
            (parse_header "General" .>> spaces)
            (opt (parse_header "Colours" .>> spaces))
            (opt (parse_header "Fonts" .>> spaces))
            (opt (parse_header "CatchTheBeat" .>> spaces))
            (many (parse_header "Mania" .>> spaces))
        |>> fun (g, c, f, ctb, ms) ->
            {
                General = parse_general g
                Colours = c |> Option.map parse_colours |> Option.defaultValue Colours.Default
                Fonts = f |> Option.map parse_fonts |> Option.defaultValue Fonts.Default
                Mania = List.map parse_mania ms
            }

    let parse file =
        match runParserOnFile skin_ini_parser () file System.Text.Encoding.UTF8 with
        | Success(s, _, _) -> s
        | Failure(e, _, _) -> failwith e

type SkinIni = SkinIni.OsuSkinIni
