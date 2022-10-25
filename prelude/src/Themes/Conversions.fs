namespace Prelude.Data.Themes

open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
open Percyqaz.Common
open Prelude.Common
open Prelude.Gameplay.NoteColors
open Prelude.Data.Themes


//https://osu.ppy.sh/wiki/no/Skinning/skin.ini#[mania]
module OsuSkin =
    open FParsec
    open Prelude.Charts.Formats.``osu!``

    let parseBool = int >> fun x -> x <> 0

    let parseRGB (s: string) : Color =
        match s.Split (",") |> List.ofArray |> List.map int with
        | r :: g :: b :: _ -> Color.FromArgb(r, g, b)
        | _ -> failwith "not enough numbers given for RGB"

    let parseRGBa (s: string) : Color =
        match s.Split (",") |> List.ofArray |> List.map int with
        | r :: g :: b :: a :: _ -> Color.FromArgb(a, r, g, b)
        | r :: g :: b :: _ -> Color.FromArgb(255, r, g, b)
        | _ -> failwith "not enough numbers given for RGBa"

    let parseInts (s: string) =
        match run (spaces >>. (sepBy pint32 (pchar ',' .>> spaces))) s with
        | Success (xs, _, _) -> xs
        | Failure (e, _, _) -> failwith e

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
        static member Default : General =
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

    let private readGeneral ((title, settings): Header) : General =
        assert (title = "General")

        let f (s: General) (key, value) =
            match key with
            | "Name" -> { s with Name = value }
            | "Author" -> { s with Author = value }
            | "Version" -> { s with Version = value }
            | "AnimationFramerate" -> { s with AnimationFramerate = int value }
            | "AllowSliderBallTint" -> { s with AllowSliderBallTint = parseBool value }
            | "ComboBurstRandom" -> { s with ComboBurstRandom = parseBool value }
            | "CursorCentre" -> { s with CursorCentre = parseBool value }
            | "CursorExpand" -> { s with CursorExpand = parseBool value }
            | "CursorRotate" -> { s with CursorRotate = parseBool value }
            | "CursorTrailRotate" -> { s with CursorTrailRotate = parseBool value }
            | "CustomComboBurstSounds" -> { s with CustomComboBurstSounds = parseInts value }
            | "HitCircleOverlayAboveNumer"
            | "HitCircleOverlayAboveNumber" -> { s with HitCircleOverlayAboveNumber = parseBool value }
            | "LayeredHitSounds" -> { s with LayeredHitSounds = parseBool value }
            | "SliderBallFlip" -> { s with SliderBallFlip = parseBool value }
            | "SpinnerFadePlayfield" -> { s with SpinnerFadePlayfield = parseBool value }
            | "SpinnerFrequencyModulate" -> { s with SpinnerFrequencyModulate = parseBool value }
            | "SpinnerNoBlink" -> { s with SpinnerNoBlink = parseBool value }
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
        static member Default : Colours =
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

    let private readColours ((title, settings): Header) : Colours = Colours.Default

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

    let private readFonts ((title, settings): Header) : Fonts = Fonts.Default

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

    let private readCatchTheBeat ((title, settings): Header) : CatchTheBeat = CatchTheBeat.Default

    let maniaDefaultTextures k format =
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
                ScorePosition = 80 //not default but cant find it
                ComboPosition = 180 //^
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
                KeyImageΔ = maniaDefaultTextures k "mania-key%s"
                KeyImageΔD = maniaDefaultTextures k "mania-key%sD"
                NoteImageΔ = maniaDefaultTextures k "mania-note%s"
                NoteImageΔH = maniaDefaultTextures k "mania-note%sH"
                NoteImageΔL = maniaDefaultTextures k "mania-note%sL"
                NoteImageΔT = maniaDefaultTextures k "mania-note%sT"
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
        | Success (n, _, _) -> if 0 <= n && n < keys then Some n else None
        | Failure (_, _, _) -> None

    let (|P_1|_|) (keys: int) (pre: string) (suff: string) (target: string) =
        match run (pstring pre >>. (pint32 .>> pstring suff) .>> eof) target with
        | Success (n, _, _) -> if 0 < n && n <= keys then Some (n - 1) else None
        | Failure (_, _, _) -> None

    let private readMania ((title, settings): Header) : Mania =
        assert (title = "Mania")

        match settings with
        | ("Keys", keys) :: settings ->
            let keys = int keys

            let f s (key, value: string) =
                match key with
                | "ColumnStart" -> { s with ColumnStart = int value }
                | "ColumnRight" -> { s with ColumnRight = int value }
                | "ColumnSpacing" -> { s with ColumnSpacing = parseInts value }
                | "ColumnWidth" -> { s with ColumnWidth = parseInts value }
                | "ColumnLineWidth" -> { s with ColumnLineWidth = parseInts value }
                | "BarlineHeight" -> { s with BarlineHeight = float value }
                | "LightingNWidth" -> { s with LightingNWidth = parseInts value }
                | "LightingLWidth" -> { s with LightingLWidth = parseInts value }
                | "WidthForNoteHeightScale" -> { s with WidthForNoteHeightScale = Some (int value) }
                | "HitPosition" -> { s with HitPosition = int value }
                | "LightPosition" -> { s with LightPosition = int value }
                | "ScorePosition" -> { s with ScorePosition = int value }
                | "ComboPosition" -> { s with ComboPosition = int value }
                | "JudgementLine" -> { s with JudgementLine = parseBool value }
                | "SpecialStyle" -> { s with SpecialStyle = value }
                | "ComboBurstStyle" -> { s with ComboBurstStyle = int value }
                | "SplitStages" -> { s with SplitStages = Some (parseBool value) }
                | "StageSeparation" -> { s with StageSeparation = int value }
                | "SeparateScore" -> { s with SeparateScore = parseBool value }
                | "KeysUnderNotes" -> { s with KeysUnderNotes = parseBool value }
                | "UpsideDown" -> { s with UpsideDown = parseBool value }
                | "KeyFlipWhenUpsideDown" -> { s with KeyFlipWhenUpsideDown = parseBool value }
                | P_0 keys "KeyFlipWhenUpsideDown" "" n -> s.KeyFlipWhenUpsideDownΔ.[n] <- parseBool value; s
                | "NoteFlipWhenUpsideDown" -> { s with NoteFlipWhenUpsideDown = parseBool value }
                | P_0 keys "KeyFlipWhenUpsideDown" "D" n -> s.KeyFlipWhenUpsideDownΔD.[n] <- parseBool value; s
                | P_0 keys "NoteFlipWhenUpsideDown" "" n -> s.NoteFlipWhenUpsideDownΔ.[n] <- parseBool value; s
                | P_0 keys "NoteFlipWhenUpsideDown" "H" n -> s.NoteFlipWhenUpsideDownΔH.[n] <- parseBool value; s
                | P_0 keys "NoteFlipWhenUpsideDown" "L" n -> s.NoteFlipWhenUpsideDownΔL.[n] <- parseBool value; s
                | P_0 keys "NoteFlipWhenUpsideDown" "T" n -> s.NoteFlipWhenUpsideDownΔT.[n] <- parseBool value; s
                | "NoteBodyStyle" -> { s with NoteBodyStyle = int value }
                | P_0 keys "NoteBodyStyle" "" n -> s.NoteBodyStyleΔ.[n] <- int value; s
                | P_1 keys "Colour" "" n -> s.ColourΔ.[n] <- parseRGBa value; s
                | P_1 keys "ColourLight" "" n -> s.ColourLightΔ.[n] <- parseRGB value; s
                | "ColourColumnLine" -> { s with ColourColumnLine = parseRGBa value }
                | "ColourBarline" -> { s with ColourBarline = parseRGBa value }
                | "ColourJudgementLine" -> { s with ColourJudgementLine = parseRGB value }
                | "ColourKeyWarning" -> { s with ColourKeyWarning = parseRGB value }
                | "ColourHold" -> { s with ColourHold = parseRGBa value }
                | "ColourBreak" -> { s with ColourBreak = parseRGB value }
                | P_0 keys "KeyImage" "" n -> s.KeyImageΔ.[n] <- value; s
                | P_0 keys "KeyImage" "D" n -> s.KeyImageΔD.[n] <- value; s
                | P_0 keys "NoteImage" "" n -> s.NoteImageΔ.[n] <- value; s
                | P_0 keys "NoteImage" "H" n -> s.NoteImageΔH.[n] <- value; s
                | P_0 keys "NoteImage" "L" n -> s.NoteImageΔL.[n] <- value; s
                | P_0 keys "NoteImage" "T" n -> s.NoteImageΔT.[n] <- value; s
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

    type SkinData = { General: General; Colours: Colours; Fonts: Fonts; Mania: Mania list }

    let skinIniParser =
        tuple5
            (parseHeader "General" .>> spaces)
            (opt (parseHeader "Colours" .>> spaces))
            (opt (parseHeader "Fonts" .>> spaces))
            (opt (parseHeader "CatchTheBeat" .>> spaces))
            (many (parseHeader "Mania" .>> spaces))
        |>> fun (g, c, f, ctb, ms) ->
                { 
                    General = readGeneral g
                    Colours =
                        c
                        |> Option.map readColours
                        |> Option.defaultValue Colours.Default
                    Fonts =
                        f
                        |> Option.map readFonts
                        |> Option.defaultValue Fonts.Default
                    Mania = List.map readMania ms
                }

    let parseSkinINI file =
        match runParserOnFile skinIniParser () file System.Text.Encoding.UTF8 with
        | Success (s, _, _) -> s
        | Failure (e, _, _) -> failwith e
            
    // constructor can throw an exception!

    module Converter =

        let getTextureFilenames(id: string, path: string) : string list =

            let file = Path.Combine(path, id)
            if File.Exists(file + "@2x.png") then [file + "@2x.png"]
            elif File.Exists(file + ".png") then [file + ".png"]
            else
                let rec f i =
                    if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                        file + "-" + i.ToString() + "@2x.png" :: f (i + 1)
                    elif File.Exists(file + "-" + i.ToString() + ".png") then
                        file + "-" + i.ToString() + ".png" :: f (i + 1)
                    else []
                let result = f 0
                if result.IsEmpty then failwithf "could not find texture in skin folder for %A" id
                result

        let grayscale (brightness: float32) (img: Bitmap) : Bitmap =
            let newimg = img.Clone()
            newimg.Mutate(fun i ->  i.Grayscale().Brightness(brightness) |> ignore)
            newimg

        type ColumnTextures = { Note: string; Head: string; Body: string; Tail: string }

        let convert (source: string) (target: string) (keymodes: int list) =
            if keymodes.IsEmpty then failwith "Specify at least 1 keymode to convert"
            
            let ini = parseSkinINI (Path.Combine (source, "skin.ini"))

            if Directory.Exists target then failwith "a folder with this name already exists!"
            Directory.CreateDirectory target |> ignore

            let textures = ResizeArray<ColumnTextures>()
            let colorConfig : ColorConfig = { ColorConfig.Default with Style = ColorScheme.Column; UseGlobalColors = false }

            // Identify textures used by noteskin
            for keys in keymodes do
                let mania = List.tryFind (fun m -> m.Keys = keys) ini.Mania |> Option.defaultValue (Mania.Default keys)

                for k = 0 to (keys - 1) do
                    let tex = { Note = mania.NoteImageΔ.[k]; Head = mania.NoteImageΔH.[k]; Body = mania.NoteImageΔL.[k]; Tail = mania.NoteImageΔT.[k] }
                    if not (textures.Contains tex) then textures.Add tex

            let loadbmp f =
                use s = File.Open(f, FileMode.Open)
                Bitmap.load s

            // Copy all the textures and ensure they are square
            // todo: for ln textures the image should stretch instead of being squared if h < w
            let square_images (name: string) (files: string list list) =
                let animation_frames = List.map List.length files |> List.max
                let colors = List.length files
                let images = files |> List.map (List.map loadbmp)
                let width = images.Head.Head.Width

                // todo: warn if widths don't match

                for row = 0 to (colors - 1) do
                    for column = 0 to (animation_frames - 1) do
                        let image = let r = images.[row] in r[column % r.Length]
                        use squarebmp = new Bitmap(width, width)
                        squarebmp.Mutate(fun img -> img.DrawImage(image, Point(0, (-image.Height + width) / 2), 1.0f) |> ignore)
                        squarebmp.Save(Path.Combine(target, sprintf "%s-%i-%i.png" name row column))

                JSON.ToFile (Path.Combine(target, name + ".json"), false)
                    {
                        Rows = colors
                        Columns = animation_frames
                        Mode = Loose
                    }

            let mutable flipholdtail = false
            let mutable useholdtail = true
            square_images "note" (textures |> Seq.map (fun x -> x.Note) |> Seq.map (fun x -> getTextureFilenames(x, source)) |> List.ofSeq)
            square_images "holdhead" (textures |> Seq.map (fun x -> x.Head) |> Seq.map (fun x -> getTextureFilenames(x, source)) |> List.ofSeq)
            square_images "holdbody" (textures |> Seq.map (fun x -> x.Body) |> Seq.map (fun x -> getTextureFilenames(x, source)) |> List.ofSeq)
            try
                square_images "holdtail" (textures |> Seq.map (fun x -> x.Tail) |> Seq.map (fun x -> getTextureFilenames(x, source)) |> List.ofSeq)
            with err ->
                Logging.Debug("Error in holdtail textures - Using hold head textures for compatibility")
                flipholdtail <- true
                useholdtail <- false

            // Generate receptors
            let receptor_base = getTextureFilenames (textures.[0].Note, source) |> List.head |> loadbmp
            // todo: square these images
            (grayscale 0.5f receptor_base).Save(Path.Combine(target, "receptor-0-0.png"))
            (grayscale 1.0f receptor_base).Save(Path.Combine(target, "receptor-1-0.png"))
            JSON.ToFile (Path.Combine(target, "receptor.json"), false)
                { Rows = 2; Columns = 1; Mode = Loose }
            
            // Point color data at textures correctly
            for keys in keymodes do
                let mania = List.tryFind (fun m -> m.Keys = keys) ini.Mania |> Option.defaultValue (Mania.Default keys)

                let textureIds = Array.zeroCreate 10

                for k = 0 to (keys - 1) do
                    let tex = { Note = mania.NoteImageΔ.[k]; Head = mania.NoteImageΔH.[k]; Body = mania.NoteImageΔL.[k]; Tail = mania.NoteImageΔT.[k] }
                    textureIds.[k] <- byte (textures.IndexOf tex)

                colorConfig.Colors.[keys - 2] <- textureIds

            // Generate noteskin.json
            let primaryKeymode = 
                List.head keymodes
                |> fun keys -> 
                    List.tryFind (fun m -> m.Keys = keys) ini.Mania
                    |> Option.defaultValue (Mania.Default keys)

            let config : NoteskinConfig =
                { NoteskinConfig.Default with
                    Name = ini.General.Name
                    Author = ini.General.Author
                    Version = ini.General.Version
                    NoteColors = colorConfig
                    FlipHoldTail = flipholdtail
                    UseHoldTailTexture = useholdtail
                    HoldNoteTrim = 0.0f
                    PlayfieldColor = primaryKeymode.ColourΔ.[0]
                    ColumnWidth = 1080f / 512f * float32 primaryKeymode.ColumnWidth.[0]
                    AnimationFrameTime = 1000.0/60.0
                }

            JSON.ToFile (Path.Combine(target, "noteskin.json"), false) config