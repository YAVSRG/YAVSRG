namespace Prelude.Charts

open FParsec
open Prelude.Common

//https://osu.ppy.sh/help/wiki/osu!_File_Formats/Osu_(file_format)
//https://osu.ppy.sh/community/forums/topics/1869?start=12468#p12468
(*
  Not currently supported:
    Sliders (parsing or writing)
    Combo colors/starters
    Loops in storyboard (parsing or writing)
    Parsing of storyboard shorthand notation
*)

module osu =

    (*
        Basic .osu file representation
        Headers are just decoded data and then parsed into record structures with the data tags
    *)

    type Header = string * (string * string) list

    let formatHeader (title, entries) =
        let f (key, value) = key + ":" + value
        "[" + title + "]\n" + (String.concat "\n" (List.map f entries))

    type TimingEffect =
        | Kiai = 1
        | OmitFirstBarline = 8

    type SampleSet =
        | None = 0
        | Default = 0
        | Normal = 1
        | Soft = 2
        | Drum = 3

    type TimingPoint =
        | BPM of Time * float32<ms/beat> * int<beat> * (SampleSet * int * int) * TimingEffect
        | SV of Time * float32 * (SampleSet * int * int) * TimingEffect
        override this.ToString() =
            match this with
            | BPM (time, msPerBeat, meter, (set,index,volume), effect) ->
                String.concat "," [string time; string msPerBeat; string meter; set |> int |> string; string index; string volume; "1"; effect |> int |> string]
            | SV (time, value, (set, index, volume), effect) ->
                String.concat "," [string time; string (-100.0f / value); "4"; set |> int |> string; string index; string volume; "0"; effect |> int |> string]

    type Point = float * float

    type SliderShape =
        | Linear
        | Catmull
        | Bezier
        | PerfectCircle
        override this.ToString() =
            match this with
            | Linear -> "L" | Catmull -> "C" | Bezier -> "B" | PerfectCircle -> "P"

    type HitAddition = SampleSet * SampleSet * int * int * string
    let formatHitAddition (s1,s2,index,volume,file) = String.concat ":" [s1 |> int |> string; s2 |> int |> string; string index; string volume; file]

    type HitSound =
        | Normal = 1
        | Whistle = 2
        | Finish = 4
        | Clap = 8

    type HitObject =
        | HitCircle of Point * Time * HitSound * HitAddition
        | HoldNote of Point * Time * Time * HitSound * HitAddition
        | Slider of Point * Time * int * (SliderShape * Point list) * float * HitSound * (HitSound list * (SampleSet * SampleSet) list * HitAddition) option
        | Spinner of Time * Time * HitSound * HitAddition
        override this.ToString() = 
            match this with
            | HitCircle ((x, y), offset, hs, addition) -> String.concat "," [x |> string; y |> string; offset |> string; "1"; hs |> int |> string; formatHitAddition addition]
            | HoldNote ((x, y), start, finish, hs, addition) -> String.concat "," [string x; string y; string start; "128"; hs |> int |> string; (string finish) + ":" + formatHitAddition addition]
            | Slider ((x, y), offset, repeats, points, length, hs, sounds) -> "nyi"
            | Spinner (start, finish, hs, addition) -> String.concat "," ["320"; "240"; string start; "8"; hs |> int |> string; string finish; formatHitAddition addition]

    (*
        Storyboard data types
    *)

    //There's actually 35 of these. todo: maybe include them all?
    //https://osu.ppy.sh/wiki/en/Storyboard_Scripting/Commands
    type Easing =
        | None = 0
        | Decelerate = 1
        | Accelerate = 2

    type Layer =
        | Background = 0
        | Fail = 1
        | Pass = 2
        | Foreground = 3
        | Overlay = 4

    type LoopType =
        | LoopForever = 0
        | LoopOnce = 1

    type TriggerType =
        | HitSoundClap
        | HitSoundFinish
        | HitSoundWhistle
        | Passing
        | Failing

    type SpriteOrigin =
        | TopLeft = 0
        | TopCentre = 1
        | TopRight = 2
        | CentreLeft = 3
        | Centre = 4
        | CentreRight = 5
        | BottomLeft = 6
        | BottomCentre = 7
        | BottomRight = 8

    type SpriteParameter =
        | VerticalFlip
        | HorizontalFlip
        | AdditiveBlendColor

    type StoryboardEvent =
        | Fade of Time * Time * Easing * float * float
        | Move of Time * Time * Easing * Point * Point
        | Move_X of Time * Time * Easing * float * float
        | Move_Y of Time * Time * Easing * float * float
        | Scale of Time * Time * Easing * float * float
        | VectorScale of Time * Time * Easing * Point * Point
        | Rotate of Time * Time * Easing * float * float
        | Color of Time * Time * Easing * (int * int * int) * (int * int * int)
        | Loop of Time * int * StoryboardEvent list
        | Trigger_Loop of Time * Time * TriggerType * StoryboardEvent list
        | Parameter of Time * Time * SpriteParameter
        member this.Format padding =
            padding + (String.concat ","
                (match this with
                | Fade (time1, time2, easing, a, b) -> ["F"; easing |> int |> string; string time1; string time2; string a; string b]
                | Move (time1, time2, easing, (x1,y1), (x2,y2)) -> ["M"; easing |> int |> string; string time1; string time2;
                    string x1; string y1; string x2; string y2]
                | Move_X (time1, time2, easing, a, b) -> ["MX"; easing |> int |> string; string time1; string time2; string a; string b]
                | Move_Y (time1, time2, easing, a, b) -> ["MY"; easing |> int |> string; string time1; string time2; string a; string b]
                | Scale (time1, time2, easing, a, b) -> ["S"; easing |> int |> string; string time1; string time2; string a; string b]
                | VectorScale (time1, time2, easing, (x1,y1), (x2,y2)) -> ["V"; easing |> int |> string; string time1; string time2;
                    string x1; string y1; string x2; string y2]
                | Rotate (time1, time2, easing, a, b) -> ["R"; easing |> int |> string; string time1; string time2; string a; string b]
                | Color (time1, time2, easing, (r1,g1,b1), (r2,g2,b2)) -> ["C"; easing |> int |> string; string time1; string time2;
                        string r1; string g1; string b1; string r2; string g2; string b2]
                | _ -> ["nyi"]
                ))

    type StoryboardObject =
        | Sprite of Layer * SpriteOrigin * string * Point * StoryboardEvent list
        | Animation of Layer * SpriteOrigin * string * Point * int * Time * LoopType * StoryboardEvent list
        | Sample of Time * Layer * string * int
        | Background of string * Point
        | Video of Time * string * Point
        | Break of Time * Time
        | BackgroundColorChange of Time * int * int * int //i have no idea what this is or does
        override this.ToString() = 
            match this with
            | Background (filename, (x, y)) -> String.concat "," ["0"; "0"; "\""+filename+"\""; string x; string y]
            | Video (time, filename, (x, y)) -> String.concat "," ["1"; string time; "\""+filename+"\""; string x; string y];
            | Break (start, finish) -> String.concat "," ["2"; string start; string finish]
            | Sprite (layer, origin, filename, (x,y), events) ->
                String.concat "\n" (
                    (String.concat "," ["Sprite"; layer.ToString("G"); origin.ToString("G"); "\""+filename+"\""; string x; string y])
                    :: (List.map (fun (x : StoryboardEvent) -> x.Format " ") events)
                 )
            | _ -> "nyi"

    (*
        Parsing of .osu and .osb formats
    *)

    let private isLetterOrDigit c = isLetter c || isDigit c
    let private comma = pchar ','
    let private colon = pchar ':'
    let private pipe = pchar '|'
    let private parseNum = pfloat
    let private parseInt = pint64 |>> int
    let private parseName = many1Satisfy isLetterOrDigit
    let private parseQuote =
        between (pchar '"') (pchar '"') (manySatisfy (fun c -> c <> '"')) <|> (many1Satisfy (Text.IsWhitespace >> not))
    let private comment = (anyOf "-/#=" >>% "") >>. restOfLine true

    let private parseKeyValue = parseName .>> spaces .>> colon .>> manyChars (anyOf " \t") .>>. (restOfLine true) .>> spaces
    let private parseHeaderTitle name = pstring ("[" + name + "]") .>> (restOfLine true) .>> spaces >>% name
    
    let parseHeader name : Parser<Header, unit> =
        many comment >>. parseHeaderTitle name .>>. (many comment >>. many (parseKeyValue .>> (many comment))) .>> spaces |>> Header

    let parseTimingPoint: Parser<TimingPoint, unit> =
        (tuple4 (parseNum .>> comma) (parseNum .>> comma) (parseInt .>> comma) (parseInt .>> comma))
        .>>. (tuple4 (parseInt .>> comma) (parseInt .>> comma) (parseInt .>> comma) parseInt) |>>
        fun ((offset, value, meter, sampleSet), (sampleIndex, volume, isBpm, effects)) ->
            if isBpm > 0
            then BPM (toTime offset, toTime value / 1.0f<beat>, meter * 1<beat>, (enum sampleSet, sampleIndex, volume), enum effects)
            else SV (toTime offset, -100.0f / float32 value, (enum sampleSet, sampleIndex, volume), enum effects)

    let parseTimingPoints = pstring "[TimingPoints]" >>. newline >>. many (parseTimingPoint .>> newline)

    let private parsePoint = parseNum .>> comma .>>. parseNum |>> Point
    let private parsePointColon = parseNum .>> colon .>>. parseNum |>> Point
    let private parseAddition =
        tuple5 (parseInt .>> colon) (parseInt .>> colon) (parseInt .>> colon) (parseInt .>> colon) (restOfLine true)
        |>> fun (normal, addition, index, volume, file) -> (enum normal, enum addition, index, volume, file)

    let parseSliderType =
        anyOf "LBCP" |>> fun c ->
            match c with
            | 'L' -> Linear
            | 'B' -> Bezier
            | 'C' -> Catmull
            | 'P' -> PerfectCircle
            | _ -> failwith "impossible"

    let parseSliderSounds =
        (sepBy1 (parseInt |>> enum) pipe) .>> comma
        .>>. (sepBy1 ((parseInt |>> enum) .>> colon .>>. (parseInt |>> enum)) pipe)
    let sliderPoints = parseSliderType .>> pipe .>>. sepBy1 parsePointColon pipe

    let parseHitObject: Parser<HitObject, unit> =
        tuple4 (parsePoint .>> comma) (parseNum .>> comma) (parseInt .>> comma) ((parseInt .>> comma) |>> enum)
        >>= (fun (pos, offset, objType, hitsound) ->
            let offset = toTime offset
            match objType &&& 139 with
            | 1 -> parseAddition |>> fun addition -> HitCircle(pos, offset, hitsound, addition)
            | 2 ->
                tuple4 (sliderPoints .>> comma) (parseInt .>> comma) parseNum
                    (opt (comma >>. (parseSliderSounds .>> comma) .>>. parseAddition) |>> Option.map (fun ((a, b), c) -> (a, b, c)))
                |>> fun (points, slides, length, sounds) ->
                    Slider(pos, offset, slides, points, length, hitsound, sounds)
            | 8 ->
                pipe2 (parseNum .>> comma) parseAddition
                    (fun endTime addition -> Spinner(offset, toTime endTime, hitsound, addition))
            | 128 ->
                pipe2 (parseNum .>> colon) parseAddition
                    (fun endTime addition -> HoldNote(pos, offset, toTime endTime, hitsound, addition))
            | _ -> failwith "Unknown hitobject type")

    let parseHitObjects = pstring "[HitObjects]" >>. newline >>. many parseHitObject

    (*
        Storyboard parsing code
    *)

    //Parsing loops is unsupported!
    let rec private parseSpriteEvent depthSkipper =
        depthSkipper
        >>. 
        (
        //((pchar 'L' >>. comma >>. tuple3 (parseNum .>> comma) (parseInt .>> comma .>> spaces) (many (parseSpriteEvent ((pchar ' ' <|> pchar '_') >>. depthSkipper .>> skipNewline))))
        //|>> (fun (time, repeats, events) -> Loop(time, repeats, events)))
        //<|>
        (tuple4 ((pstring "MX" <|> pstring "MY" <|> (satisfy (isAnyOf "FMSVRCP") |>> string)) .>> comma .>> spaces)
                    (parseInt .>> comma .>> spaces |>> enum) (opt parseNum .>> comma .>> spaces) (opt parseNum .>> comma .>> spaces)
        >>= (fun (eventType, easing, startTime, endTime) ->
            let parsePoint = ((parseNum .>> comma) .>>. parseNum)
            let parseNumsWithShorthand = (parseNum .>>. opt (comma >>. parseNum)) |>> fun (a, b) -> (a, Option.defaultValue a b)
            let startTime, endTime =
                match startTime, endTime with
                | None, None -> failwith "Both start time and end time were left blank"
                | Some x, None -> x, x
                | None, Some x -> x, x
                | Some x, Some y -> x, y
            let startTime, endTime = toTime startTime, toTime endTime
            match eventType with
            | "F" -> parseNumsWithShorthand |>> fun (f1, f2) -> Fade(startTime, endTime, easing, f1, f2)
            | "M" -> parsePoint .>>. opt (comma >>. parsePoint) |>> fun (p1, p2) -> Move(startTime, endTime, easing, p1, Option.defaultValue p1 p2)
            | "MX" -> parseNumsWithShorthand |>> fun (f1, f2) -> Move_X(startTime, endTime, easing, f1, f2)
            | "MY" -> parseNumsWithShorthand |>> fun (f1, f2) -> Move_Y(startTime, endTime, easing, f1, f2)
            | "S" -> parseNumsWithShorthand |>> fun (s1, s2) -> Scale(startTime, endTime, easing, s1, s2)
            | "V" -> parsePoint .>>. opt (comma >>. parsePoint) |>> fun (p1, p2) -> VectorScale(startTime, endTime, easing, p1, Option.defaultValue p1 p2)
            | "R" -> parseNumsWithShorthand |>> fun (f1, f2) -> Rotate(startTime, endTime, easing, f1, f2)
            | "P" ->
                (charReturn 'H' HorizontalFlip <|> charReturn 'V' VerticalFlip <|> charReturn 'A' AdditiveBlendColor)
                |>> fun p -> Parameter(startTime, endTime, p)
            | "C" ->
                (tuple3 (parseInt .>> comma) (parseInt .>> comma) (parseInt))
                .>>. opt (tuple3 (comma >>. parseInt .>> comma) (parseInt .>> comma) parseInt)
                |>> fun (c1, c2) -> Color(startTime, endTime, easing, c1, Option.defaultValue c1 c2)
            | _ -> failwith "Unknown storyboard event")))

    let private parseSpriteEvents depthSkipper = many (parseSpriteEvent depthSkipper .>> skipNewline)

    let parseStoryboardEvent =
        (pstring "Animation" >>. comma
            >>. (tuple3
                (tuple4 (parseName .>> comma |>> Layer.Parse) (parseName .>> comma |>> SpriteOrigin.Parse)
                    (parseQuote .>> comma) (parseNum .>> comma))
                ((tuple4 (parseNum .>> comma) (parseInt .>> comma) (parseNum .>> comma) (parseName |>> LoopType.Parse))
                .>> pchar '\n') (parseSpriteEvents (pchar '_' <|> pchar ' ')))
            |>> fun ((layer, origin, file, x), (y, frames, frameTime, loopType), events) ->
                Animation(layer, origin, file, (x, y), frames, toTime frameTime, loopType, events))

        <|> ((pstring "Sprite" <|> pstring "4") >>. comma
            >>. ((tuple5 (parseName .>> comma |>> Layer.Parse) (parseName .>> comma |>> SpriteOrigin.Parse)
                    (parseQuote .>> comma) (parseNum .>> comma) parseNum) .>> pchar '\n')
            .>>. (parseSpriteEvents (pchar '_' <|> pchar ' '))
            |>> fun ((layer, origin, file, x, y), events) -> Sprite(layer, origin, file, (x, y), events))

        <|> ((pstring "Background" <|> pstring "0") >>. comma
            >>. (tuple3 (parseNum .>> comma) parseQuote (opt (tuple2 (comma >>. parseNum) (comma >>. parseNum)) |>> Option.defaultValue (0.0, 0.0)))
            |>> fun (time, file, (x, y)) -> Background(file, (x, y)))

        <|> ((pstring "Sample") >>. comma
            >>. (tuple4 (parseNum .>> comma) (parseName .>> comma |>> Layer.Parse) (parseQuote .>> comma) parseInt)
            |>> fun (time, layer, file, volume) -> Sample(toTime time, layer, file, volume))
         
        <|> ((pstring "Video" <|> pstring "1") >>. comma
            >>. (tuple3 (parseNum .>> comma) parseQuote (opt (tuple2 (comma >>. parseNum) (comma >>. parseNum)) |>> Option.defaultValue (0.0, 0.0)))
            |>> fun (time, file, (x, y)) -> Video(toTime time, file, (x, y)))
         
        <|> ((pstring "Break" <|> pstring "2") >>. comma
            >>. (parseNum .>> comma) .>>. parseNum
            |>> fun (time1, time2) -> Break (toTime time1, toTime time2))

        //I have no idea what this does and cannot for the life of me find any documentation on it
        <|> (pstring "3" >>. comma
            >>. (tuple4 (parseNum .>> comma) (parseInt .>> comma) (parseInt .>> comma) (parseInt))
            |>> fun (time, r, g, b) -> BackgroundColorChange(toTime time, r, g, b))

    let parseEvents =
        pstring "[Events]" >>. newline >>. many comment
        >>. many ((parseStoryboardEvent .>> spaces) .>> (many comment)) .>> spaces

    (*
        Metadata structures to turn parsed headers into the meaningful metadata
        Can also check here for invalid values/ignore unknown values
    *)

    type GameMode =
        | osu = 0
        | Taiko = 1
        | Catch = 2
        | Mania = 3

    type OverlayPosition =
        | NoChange = 0
        | Below = 1
        | Above = 2

    type General =
        { 
            AudioFilename: string
            AudioLeadIn: int
            PreviewTime: Time
            Countdown: int
            SampleSet: SampleSet
            StackLeniency: float
            Mode: GameMode
            LetterboxInBreaks: bool
            UseSkinSprites: bool
            OverlayPosition: OverlayPosition
            SkinPreference: string
            EpilepsyWarning: bool
            CountdownOffset: int
            SpecialStyle: bool
            WidescreenStoryboard: bool
            SamplesMatchPlaybackRate: bool 
         }
        static member Default =
            { 
                AudioFilename = ""
                AudioLeadIn = 0
                PreviewTime = -1.0f<ms>
                Countdown = 0
                SampleSet = SampleSet.Normal
                StackLeniency = 0.7
                Mode = GameMode.osu
                LetterboxInBreaks = false
                UseSkinSprites = false
                OverlayPosition = OverlayPosition.NoChange
                SkinPreference = ""
                EpilepsyWarning = false
                CountdownOffset = 0
                SpecialStyle = false
                WidescreenStoryboard = false
                SamplesMatchPlaybackRate = false
            }

    let private readGeneral (title, settings) =
        assert (title = "General")
        let readBool = int >> fun x -> x <> 0

        let f s (key, value) =
            match key with
            | "AudioFilename" -> { s with AudioFilename = value }
            | "AudioLeadIn" -> { s with AudioLeadIn = value |> int }
            | "PreviewTime" -> { s with PreviewTime = value |> float |> toTime }
            | "Countdown" -> { s with Countdown = value |> int }
            | "SampleSet" -> { s with SampleSet = value |> SampleSet.Parse }
            | "StackLeniency" -> { s with StackLeniency = value |> float }
            | "Mode" -> { s with Mode = (value |> int |> enum) }
            | "LetterboxInBreaks" -> { s with LetterboxInBreaks = value |> readBool }
            | "UseSkinSprites" -> { s with UseSkinSprites = value |> readBool }
            | "OverlayPosition" -> { s with OverlayPosition = value |> OverlayPosition.Parse }
            | "SkinPreference" -> { s with SkinPreference = value }
            | "EpilepsyWarning" -> { s with EpilepsyWarning = value |> readBool }
            | "CountdownOffset" -> { s with CountdownOffset = value |> int }
            | "SpecialStyle" -> { s with SpecialStyle = value |> readBool }
            | "WidescreenStoryboard" -> { s with WidescreenStoryboard = value |> readBool }
            | "SamplesMatchPlaybackRate" -> { s with SamplesMatchPlaybackRate = value |> readBool }
            | _ -> s
        List.fold f General.Default settings

    let private writeGeneral (data: General) : Header =
        ("General", [("AudioFilename", data.AudioFilename); ("AudioLeadIn", string data.AudioLeadIn); ("PreviewTime", string data.PreviewTime);
            ("Countdown", string data.Countdown); ("SampleSet", string data.SampleSet); ("StackLeniency", string data.StackLeniency);
            ("Mode", data.Mode |> int |> string); ("LetterboxInBreaks", if data.LetterboxInBreaks then "1" else "0");
            ("UseSkinSprites", if data.UseSkinSprites then "1" else "0"); ("OverlayPosition", string data.OverlayPosition);
            ("SkinPreference", data.SkinPreference); ("EpilepsyWarning", if data.EpilepsyWarning then "1" else "0");
            ("CountdownOffset", string data.CountdownOffset); ("SpecialStyle", if data.SpecialStyle then "1" else "0");
            ("WidescreenStoryboard", if data.WidescreenStoryboard then "1" else "0"); ("SamplesMatchPlaybackRate", if data.SamplesMatchPlaybackRate then "1" else "0")])


    type Editor =
        { 
            Bookmarks: Time list
            DistanceSpacing: float
            BeatDivisor: float
            GridSize: int
            TimelineZoom: float
        }
        static member Default =
            {
                Bookmarks = []
                DistanceSpacing = 1.0
                BeatDivisor = 8.0
                GridSize = 4
                TimelineZoom = 1.0
            }

    let private readEditor (title, settings) =
        assert (title = "Editor")
        let f s (key, value) =
            match key with
            | "Bookmarks" ->
                match run (sepBy parseNum comma) value with
                | Success(result, _, _) -> { s with Bookmarks = result |> List.map toTime }
                | Failure(errorMsg, _, _) -> failwith errorMsg
            | "DistanceSpacing" -> { s with DistanceSpacing = value |> float }
            | "BeatDivisor" -> { s with BeatDivisor = value |> float }
            | "GridSize" -> { s with GridSize = value |> int }
            | "TimelineZoom" -> { s with TimelineZoom = value |> float }
            | _ -> s
        List.fold f Editor.Default settings

    let private writeEditor (data: Editor) : Header =
        ("Editor", [("Bookmarks", String.concat "," (List.map string data.Bookmarks)); ("DistanceSpacing", string data.DistanceSpacing);
            ("BeatDivisor", string data.BeatDivisor); ("GridSize", string data.GridSize); ("TimelineZoom", string data.TimelineZoom)])


    type Metadata = 
        { 
            Title: string
            TitleUnicode: string
            Artist: string
            ArtistUnicode: string
            Creator: string
            Version: string
            Source: string
            Tags: string list
            BeatmapID: int
            BeatmapSetID: int
        } 
        static member Default =
            {
                Title = ""
                TitleUnicode = ""
                Artist = ""
                ArtistUnicode = ""
                Creator = ""
                Version = ""
                Source = ""
                Tags = []
                BeatmapID = 0
                BeatmapSetID = -1
            }

    let private readMetadata (title, settings) =
        assert (title = "Metadata")
        let f s (key, value) =
            match key with
            | "Tags" ->
                match run (sepBy (many1Satisfy (Text.IsWhitespace >> not)) (many1 (pchar ' '))) value with
                | Success(result, _, _) -> { s with Tags = result }
                | Failure(errorMsg, _, _) -> failwith (errorMsg + "|" + value + "|")
            | "Title" -> { s with Title = value }
            | "TitleUnicode" -> { s with TitleUnicode = value }
            | "Artist" -> { s with Artist = value }
            | "ArtistUnicode" -> { s with ArtistUnicode = value }
            | "Creator" -> { s with Creator = value }
            | "Version" -> { s with Version = value }
            | "Source" -> { s with Source = value }
            | "BeatmapID" -> { s with BeatmapID = int value}
            | "BeatmapSetID" -> { s with BeatmapSetID = int value }
            | _ -> s
        List.fold f Metadata.Default settings

    let private writeMetadata (data: Metadata) : Header =
        ("Metadata", [("Title", data.Title); ("TitleUnicode", data.TitleUnicode); ("Artist", data.Artist);
            ("ArtistUnicode", data.ArtistUnicode); ("Creator", data.Creator); ("Version", data.Version);
            ("Source", data.Source); ("Tags", String.concat " " data.Tags); ("BeatmapID", string data.BeatmapID);
            ("BeatmapSetID", string data.BeatmapSetID)])


    type Difficulty =
        { 
            HPDrainRate: float
            CircleSize: float
            OverallDifficulty: float
            ApproachRate: float
            SliderMultiplier: float
            SliderTickRate: float
        }
        static member Default = 
            {
                HPDrainRate = 5.0
                CircleSize = 5.0
                OverallDifficulty = 5.0
                ApproachRate = 5.0
                SliderMultiplier = 1.4
                SliderTickRate = 1.0
            }

    let private readDifficulty (title, settings) =
        assert (title = "Difficulty")
        let f s (key, value) =
            match key with
            | "HPDrainRate" -> { s with HPDrainRate = float value }
            | "CircleSize" -> { s with CircleSize = float value }
            | "OverallDifficulty" -> { s with OverallDifficulty = float value }
            | "ApproachRate" -> { s with ApproachRate = float value }
            | "SliderMultiplier" -> { s with SliderMultiplier = float value }
            | "SliderTickRate" -> { s with SliderTickRate = float value }
            | _ -> s
        List.fold f Difficulty.Default settings

    let private writeDifficulty (data: Difficulty) : Header =
        ("Difficulty", [("HPDrainRate", string data.HPDrainRate); ("CircleSize", string data.CircleSize);
            ("OverallDifficulty", string data.OverallDifficulty); ("ApproachRate", string data.ApproachRate);
            ("SliderMultiplier", string data.SliderMultiplier); ("SliderTickRate", string data.SliderTickRate)])

    (*
        Final parsing of .osu file into meaningful data/saving .osu files from internal representation
    *)

    type Beatmap =
        {
            General: General
            Editor: Editor
            Metadata: Metadata
            Difficulty: Difficulty
            Events: StoryboardObject list
            Objects: HitObject list
            Timing: TimingPoint list
        }


    //todo: in future can include variables list too
    type Storyboard = StoryboardObject list

    let getBeatmapFilename beatmap =
        let clean = String.filter (fun c -> System.Char.IsWhiteSpace c || System.Char.IsLetterOrDigit c)
        sprintf "%s - %s (%s) [%s].osu"
            (clean beatmap.Metadata.ArtistUnicode)
            (clean beatmap.Metadata.TitleUnicode)
            (clean beatmap.Metadata.Creator)
            (clean beatmap.Metadata.Version)

    let eventsToString events = "[Events]\n" + String.concat "\n" (List.map (fun o -> o.ToString()) events);

    let beatmapToString beatmap = 
        String.concat "\n\n" [
            "osu file format v14";
            beatmap.General |> writeGeneral |> formatHeader;
            beatmap.Editor |> writeEditor |> formatHeader;
            beatmap.Metadata |> writeMetadata |> formatHeader;
            beatmap.Difficulty |> writeDifficulty |> formatHeader;
            beatmap.Events |> eventsToString;
            "[TimingPoints]\n" + String.concat "\n" (List.map (fun o -> o.ToString()) beatmap.Timing);
            "[HitObjects]\n" + String.concat "\n" (List.map (fun o -> o.ToString()) beatmap.Objects);
        ]

    let parseBeatmap =
        tuple4 (pstring "osu file format v" >>. restOfLine true .>> spaces)
            (parseHeader "General" .>> spaces)  //General
            (parseHeader "Editor" .>> spaces)  //Editor
            (parseHeader "Metadata" .>> spaces)  //Metadata
        .>>.
        tuple5 (parseHeader "Difficulty" .>> spaces) (*Difficulty*) (parseEvents .>> spaces) (parseTimingPoints .>> spaces)
            (optional (parseHeader "Colours" .>> spaces))
            (parseHitObjects .>> spaces)
        |>> fun ((format, general, editor, metadata), (difficulty, events, timingpoints, _, hitobjects)) ->
            {
                General = readGeneral general
                Editor = readEditor editor
                Metadata = readMetadata metadata
                Difficulty = readDifficulty difficulty
                Events = events
                Objects = hitobjects
                Timing = timingpoints
            }

    let loadBeatmapFile path : Beatmap =
        match runParserOnFile parseBeatmap () path System.Text.Encoding.UTF8 with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg

    let saveBeatmapFile path beatmap = 
        System.IO.File.WriteAllText(path, beatmapToString beatmap)
    
    let loadStoryboardFile path : Storyboard =
        match runParserOnFile parseEvents () path System.Text.Encoding.UTF8 with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith errorMsg
    
    let saveStoryboardFile path events = 
        System.IO.File.WriteAllText (path, eventsToString events + "\n\n")