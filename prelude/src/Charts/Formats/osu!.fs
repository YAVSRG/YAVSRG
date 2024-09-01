namespace Prelude.Charts.Formats.osu

open System
open System.Globalization

//https://osu.ppy.sh/wiki/en/Client/File_formats/osu_(file_format)
//https://osu.ppy.sh/community/forums/topics/1869?start=12468#p12468
(*
  Not currently supported:
    Parsing storyboards (only writing a newly created storyboard is available)
    Loops and triggers in storyboards (both reading and writing)
*)

module MapHelpers =

    let string_or (key: string) (default_value: string) (map: Map<string, string>) =
        map.TryFind key |> Option.defaultValue default_value

    let int_or (key: string) (default_value: int) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Int32.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

    let int_opt (key: string) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Int32.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | false, _ -> None
        | None -> None

    let float_or (key: string) (default_value: float) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

    let float_opt (key: string) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> Some v
            | false, _ -> None
        | None -> None

    let enum_or<'T when 'T : enum<int> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType> (key: string) (default_value: 'T) (map: Map<string, string>) =
        match map.TryFind key with
        | Some s ->
            match Enum.TryParse(s, true) with
            | true, v -> v
            | false, _ -> default_value
        | None -> default_value

module CsvHelpers =
    
    let string_or (index: int) (default_value: string) (csv: string array) =
        if index >= csv.Length then 
            default_value 
        else
            csv.[index]

    let int_or (index: int) (default_value: int) (csv: string array) =
        if index >= csv.Length then 
            default_value
        else
            match Int32.TryParse(csv.[index], CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value

    let float_or (index: int) (default_value: float) (csv: string array) =
        if index >= csv.Length then 
            default_value
        else
            match Double.TryParse(csv.[index], CultureInfo.InvariantCulture) with
            | true, v -> v
            | false, _ -> default_value

    let enum_or<'T when 'T : enum<int> and 'T : (new: unit -> 'T) and 'T : struct and 'T :> ValueType> (index: int) (default_value: 'T) (csv: string array) =
        if index >= csv.Length then 
            default_value 
        else
            match Enum.TryParse(csv.[index], true) with
            | true, v -> v
            | false, _ -> default_value
        
type Gamemode =
    | OSU = 0
    | OSU_TAIKO = 1
    | OSU_CATCH = 2
    | OSU_MANIA = 3

type OverlayPosition =
    | NoChange = 0
    | Below = 1
    | Above = 2

type Countdown =
    | None = 0
    | Normal = 1
    | Half = 2
    | Double = 3

type SampleSet =
    | None = 0
    | Default = 0
    | Normal = 1
    | Soft = 2
    | Drum = 3

type General =
    {
        AudioFilename: string
        AudioLeadIn: int
        PreviewTime: int
        Countdown: Countdown
        SampleSet: SampleSet
        StackLeniency: float
        Mode: Gamemode
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
    static member FromMap (properties: Map<string, string>) =
        {
            AudioFilename = MapHelpers.string_or "AudioFilename" "" properties
            AudioLeadIn = MapHelpers.int_or "AudioLeadIn" 0 properties
            PreviewTime = MapHelpers.int_or "PreviewTime" -1 properties
            Countdown = MapHelpers.int_or "Countdown" 1 properties |> enum
            SampleSet = MapHelpers.enum_or "SampleSet" SampleSet.Normal properties
            StackLeniency = MapHelpers.float_or "StackLeniency" 0.7 properties
            Mode = MapHelpers.enum_or "Mode" Gamemode.OSU properties
            LetterboxInBreaks = MapHelpers.int_or "LetterBoxInBreaks" 0 properties <> 0
            UseSkinSprites = MapHelpers.int_or "UseSkinSprites" 0 properties <> 0
            OverlayPosition = MapHelpers.enum_or "OverlayPosition" OverlayPosition.NoChange properties
            SkinPreference = MapHelpers.string_or "SkinPreference" "" properties
            EpilepsyWarning = MapHelpers.int_or "EpilepsyWarning" 0 properties <> 0
            CountdownOffset = MapHelpers.int_or "CountdownOffset" 0 properties
            SpecialStyle = MapHelpers.int_or "SpecialStyle" 0 properties <> 0
            WidescreenStoryboard = MapHelpers.int_or "WidescreenStoryboard" 0 properties <> 0
            SamplesMatchPlaybackRate = MapHelpers.int_or "SamplesMatchPlaybackRate" 0 properties <> 0
        }
    member this.ToMap : (string * string) seq =
        seq {
            yield "AudioFilename", this.AudioFilename
            yield "AudioLeadIn", this.AudioLeadIn.ToString()
            yield "PreviewTime", this.PreviewTime.ToString()
            yield "Countdown", (int this.Countdown).ToString()
            yield "SampleSet", this.SampleSet.ToString()
            yield "StackLeniency", this.StackLeniency.ToString(CultureInfo.InvariantCulture)
            yield "Mode", (int this.Mode).ToString()
            yield "LetterboxInBreaks", if this.LetterboxInBreaks then "1" else "0"
            yield "UseSkinSprites", if this.UseSkinSprites then "1" else "0"
            yield "OverlayPosition", this.OverlayPosition.ToString()
            yield "SkinPreference", this.SkinPreference
            yield "EpilepsyWarning", if this.EpilepsyWarning then "1" else "0"
            yield "CountdownOffset", this.CountdownOffset.ToString()
            yield "SpecialStyle", if this.SpecialStyle then "1" else "0"
            yield "WidescreenStoryboard", if this.WidescreenStoryboard then "1" else "0"
            yield "SamplesMatchPlaybackRate", if this.SamplesMatchPlaybackRate then "1" else "0"
        }

type Editor =
    {
        Bookmarks: int list
        DistanceSpacing: float
        BeatDivisor: int
        GridSize: int
        TimelineZoom: float
    }
    static member Default = 
        {
            Bookmarks = []
            DistanceSpacing = 1.0
            BeatDivisor = 8
            GridSize = 4
            TimelineZoom = 1.0
        }
    static member FromMap (properties: Map<string, string>) =
        {
            Bookmarks = 
                MapHelpers.string_or "Bookmarks" "" properties
                |> fun s -> s.Trim().Trim(',').Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                |> Seq.choose (fun s -> 
                    match Int32.TryParse(s, CultureInfo.InvariantCulture) with
                    | true, v -> Some v
                    | false, _ -> None
                )
                |> List.ofSeq
            DistanceSpacing = MapHelpers.float_or "DistanceSpacing" 1.0 properties
            BeatDivisor = MapHelpers.int_or "BeatDivisor" 8 properties
            GridSize = MapHelpers.int_or "GridSize" 4 properties
            TimelineZoom = MapHelpers.float_or "TimelineZoom" 1.0 properties
        }
    member this.ToMap : (string * string) seq =
        seq {
            yield "Bookmarks", (this.Bookmarks |> Seq.map string |> String.concat ",")
            yield "DistanceSpacing", this.DistanceSpacing.ToString(CultureInfo.InvariantCulture)
            yield "BeatDivisor", this.BeatDivisor.ToString()
            yield "GridSize", this.GridSize.ToString()
            yield "TimelineZoom", this.TimelineZoom.ToString(CultureInfo.InvariantCulture)
        }

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
    static member FromMap (properties: Map<string, string>) =
        {
            Title = MapHelpers.string_or "Title" "" properties
            TitleUnicode = MapHelpers.string_or "TitleUnicode" "" properties
            Artist = MapHelpers.string_or "Artist" "" properties
            ArtistUnicode = MapHelpers.string_or "ArtistUnicode" "" properties
            Creator = MapHelpers.string_or "Creator" "" properties
            Version = MapHelpers.string_or "Version" "" properties
            Source = MapHelpers.string_or "Source" "" properties
            Tags = 
                MapHelpers.string_or "Tags" "" properties
                |> fun s -> s.Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                |> List.ofSeq
            BeatmapID = MapHelpers.int_or "BeatmapID" 0 properties
            BeatmapSetID = MapHelpers.int_or "BeatmapSetID" -1 properties
        }
    member this.ToMap : (string * string) seq =
        seq {
            yield "Title", this.Title
            yield "TitleUnicode", this.TitleUnicode
            yield "Artist", this.Artist
            yield "ArtistUnicode", this.ArtistUnicode
            yield "Creator", this.Creator
            yield "Version", this.Version
            yield "Source", this.Source
            yield "Tags", (this.Tags |> String.concat " ")
            yield "BeatmapID", this.BeatmapID.ToString()
            yield "BeatmapSetID", this.BeatmapSetID.ToString()
        }

type Difficulty =
    {
        HPDrainRate: float
        CircleSize: float
        OverallDifficulty: float
        ApproachRate: float
        SliderMultiplier: float
        SliderTickRate: float
    }
    static member FromMap (properties: Map<string, string>) =
        {
            HPDrainRate = MapHelpers.float_or "HPDrainRate" 5.0 properties
            CircleSize = MapHelpers.float_or "CircleSize" 5.0 properties
            OverallDifficulty = MapHelpers.float_or "OverallDifficulty" 5.0 properties
            ApproachRate = MapHelpers.float_or "ApproachRate" 5.0 properties
            SliderMultiplier = MapHelpers.float_or "SliderMultiplier" 1.4 properties
            SliderTickRate = MapHelpers.float_or "SliderTickRate" 1.0 properties
        }
    member this.ToMap : (string * string) seq =
        seq {
            yield "HPDrainRate", this.HPDrainRate.ToString(CultureInfo.InvariantCulture)
            yield "CircleSize", this.CircleSize.ToString(CultureInfo.InvariantCulture)
            yield "OverallDifficulty", this.OverallDifficulty.ToString(CultureInfo.InvariantCulture)
            yield "ApproachRate", this.ApproachRate.ToString(CultureInfo.InvariantCulture)
            yield "SliderMultiplier", this.SliderMultiplier.ToString(CultureInfo.InvariantCulture)
            yield "SliderTickRate", this.SliderTickRate.ToString(CultureInfo.InvariantCulture)
        }

type HitSample = 
    {
        NormalSet: SampleSet
        AdditionSet: SampleSet
        Index: int
        Volume: int
        Filename: string
    }
    static member Default =
        {
            NormalSet = SampleSet.None
            AdditionSet = SampleSet.None
            Index = 0
            Volume = 0
            Filename = ""
        }
    override this.ToString() =
        sprintf "%i:%i:%i:%i:%s" (int this.NormalSet) (int this.AdditionSet) this.Index this.Volume this.Filename

type HitSound =
    | Default = 0
    | Normal = 1
    | Whistle = 2
    | Finish = 4
    | Clap = 8

type HitCircle =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        HitSample: HitSample
    }
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%O"
            this.X
            this.Y
            this.Time
            (1 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.HitSample

type SliderShape =
    | Linear
    | Catmull
    | Bezier
    | PerfectCircle
    override this.ToString() =
        match this with
        | Linear -> "L"
        | Catmull -> "C"
        | Bezier -> "B"
        | PerfectCircle -> "P"

type Slider =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        CurveType: SliderShape
        CurvePoints: (int * int) list
        Slides: int
        Length: float
        EdgeSounds: HitSound list
        EdgeSets: (SampleSet * SampleSet) list
        HitSample: HitSample
    }
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%O|%s,%i,%s,%s,%s,%O"
            this.X
            this.Y
            this.Time
            (2 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.CurveType
            (this.CurvePoints |> Seq.map (fun (x, y) -> sprintf "%i:%i" x y) |> String.concat "|")
            this.Slides
            (this.Length.ToString(CultureInfo.InvariantCulture))
            (this.EdgeSounds |> Seq.map (int >> sprintf "%i") |> String.concat "|")
            (this.EdgeSets |> Seq.map (fun (normal, addition) -> sprintf "%i:%i" (int normal) (int addition)) |> String.concat "|")
            this.HitSample

type Spinner =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        EndTime: int
        HitSample: HitSample
    }
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%i,%O"
            this.X
            this.Y
            this.Time
            (8 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.EndTime
            this.HitSample

type Hold =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        EndTime: int
        HitSample: HitSample
    }
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%i:%O"
            this.X
            this.Y
            this.Time
            (128 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.EndTime
            this.HitSample

type HitObject =
    | HitCircle of HitCircle
    | HoldNote of Hold
    | Slider of Slider
    | Spinner of Spinner
    member this.Time =
        match this with
        | HitCircle x -> x.Time
        | HoldNote x -> x.Time
        | Slider x -> x.Time
        | Spinner x -> x.Time
    override this.ToString() =
        match this with
        | HitCircle x -> x.ToString()
        | HoldNote x -> x.ToString()
        | Slider x -> x.ToString()
        | Spinner x -> x.ToString()

type TimingEffect =
    | None = 0
    | Kiai = 1
    | OmitFirstBarline = 8

type UninheritedTimingPoint =
    {
        Time: int
        MsPerBeat: float
        Meter: int
        SampleSet: SampleSet
        SampleIndex: int
        Volume: int
        Effects: TimingEffect
    }
    override this.ToString() =
        sprintf "%i,%s,%i,%i,%i,%i,1,%i"
            this.Time
            (this.MsPerBeat.ToString(CultureInfo.InvariantCulture))
            this.Meter
            (int this.SampleSet)
            this.SampleIndex
            this.Volume
            (int this.Effects)

type InheritedTimingPoint =
    {
        Time: int
        Multiplier: float
        METER__UNUSED: int
        SampleSet: SampleSet
        SampleIndex: int
        Volume: int
        Effects: TimingEffect
    }
    override this.ToString() =
        sprintf "%i,%s,%i,%i,%i,%i,0,%i"
            this.Time
            (-100.0 / this.Multiplier |> fun f -> f.ToString(CultureInfo.InvariantCulture))
            this.METER__UNUSED
            (int this.SampleSet)
            this.SampleIndex
            this.Volume
            (int this.Effects)

type TimingPoint =
    | Uninherited of UninheritedTimingPoint
    | Inherited of InheritedTimingPoint
    member this.Time =
        match this with
        | Uninherited x -> x.Time
        | Inherited x -> x.Time
    override this.ToString() =
        match this with
        | Uninherited x -> x.ToString()
        | Inherited x -> x.ToString()

type Easing =
    | Linear = 0
    | EasingOut = 1 // Decelerate
    | EasingIn = 2 // Accelerate
    | QuadIn = 3
    | QuadOut = 4
    | QuadInOut = 5
    | CubicIn = 6
    | CubicOut = 7
    | CubicInOut = 8
    | QuartIn = 9
    | QuartOut = 10
    | QuartInOut = 11
    | QuintIn = 12
    | QuintOut = 13
    | QuintInOut = 14
    | SineIn = 15
    | SineOut = 16
    | SineInOut = 17
    | ExpoIn = 18
    | ExpoOut = 19
    | ExpoInOut = 20
    | CircIn = 21
    | CircOut = 22
    | CircInOut = 23
    | ElasticIn = 24
    | ElasticOut = 25
    | ElasticHalfOut = 26
    | ElasticQuarterOut = 27
    | ElasticInOut = 28
    | BackIn = 29
    | BackOut = 30
    | BackInOut = 31
    | BounceIn = 32
    | BounceOut = 33
    | BounceInOut = 34

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

type StoryboardCommandGuts =
    | Fade of start_opacity: float * end_opacity: float
    | Move of start_pos: (int * int) * end_pos: (int * int)
    | Move_X of start_x: int * end_x: int
    | Move_Y of start_y: int * end_y: int
    | Scale of start_scale: float * end_scale: float
    | VectorScale of start_scale: (float * float) * end_scale: (float * float)
    | Rotate of start_radians: float * end_radians: float
    | Color of start_color: (int * int * int) * end_color: (int * int * int)
    member this.Shorthand =
        match this with
        | Fade _ -> "F"
        | Move _ -> "M"
        | Move_X _ -> "MX"
        | Move_Y _ -> "MY"
        | Scale _ -> "S"
        | VectorScale _ -> "V"
        | Rotate _ -> "R"
        | Color _ -> "C"
    override this.ToString() =
        match this with
        | Fade (a, b) -> sprintf "%f,%f" a b
        | Move ((x1, y1), (x2, y2)) -> sprintf "%i,%i,%i,%i" x1 y1 x2 y2
        | Move_X (a, b) -> sprintf "%i,%i" a b
        | Move_Y (a, b) -> sprintf "%i,%i" a b
        | Scale (a, b) -> sprintf "%f,%f" a b
        | VectorScale ((x1, y1), (x2, y2)) -> sprintf "%f,%f,%f,%f" x1 y1 x2 y2
        | Rotate (a, b) -> sprintf "%f,%f" a b
        | Color ((r1, g1, b1), (r2, g2, b2)) -> sprintf "%i,%i,%i,%i,%i,%i" r1 g1 b1 r2 g2 b2

// todo: normal/trigger/loop DU
type StoryboardCommand =
    {
        Easing: Easing
        StartTime: int
        EndTime: int
        Command: StoryboardCommandGuts
    }
    member this.Format : string seq =
        seq {
            yield sprintf "%s,%i,%i,%i,%O"
                this.Command.Shorthand
                (int this.Easing)
                this.StartTime
                this.EndTime
                this.Command
        }

type Sprite =
    {
        Layer: Layer
        Origin: SpriteOrigin
        File: string
        X: int
        Y: int
        Commands: StoryboardCommand list
    }
    override this.ToString() =
        seq {
            yield sprintf "Sprite,%O,%O,%A,%i,%i"
                this.Layer
                this.Origin
                this.File
                this.X
                this.Y
            for command in this.Commands do
                for f in command.Format do
                    yield " " + f
        }
        |> String.concat "\n"

type Animation =
    {
        Layer: Layer
        Origin: SpriteOrigin
        File: string
        X: int
        Y: int
        FrameCount: int
        MsPerFrame: int
        LoopType: LoopType
        Commands: StoryboardCommand list
    }
    override this.ToString() =
        seq {
            yield sprintf "Animation,%O,%O,%A,%i,%i,%i,%i,%i"
                this.Layer
                this.Origin
                this.File
                this.X
                this.Y
                this.FrameCount
                this.MsPerFrame
                (int this.LoopType)
            for command in this.Commands do
                for f in command.Format do
                    yield " " + f
        }
        |> String.concat "\n"

type StoryboardObject =
    | Sprite of Sprite
    | Animation of Animation
    | Sample of int * Layer * string * int
    | Background of string * int * int
    | Video of int * string * int * int
    | Break of int * int
    override this.ToString() =
        match this with
        | Sprite sprite -> sprite.ToString()
        | Animation animation -> animation.ToString()
        | Sample (time, layer, filename, volume) -> sprintf "Sample,%i,%i,%A,%i" time (int layer) filename volume
        | Background(filename, x, y) -> sprintf "0,%A,%i,%i" filename x y
        | Video(time, filename, x, y) -> sprintf "1,%i,%A,%i,%i" time filename x y
        | Break(start, finish) -> sprintf "2,%i,%i" start finish

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
    member this.Filename =
        let clean =
            String.filter (fun c -> System.Char.IsWhiteSpace c || System.Char.IsLetterOrDigit c)

        sprintf
            "%s - %s (%s) [%s].osu"
            (clean this.Metadata.ArtistUnicode)
            (clean this.Metadata.TitleUnicode)
            (clean this.Metadata.Creator)
            (clean this.Metadata.Version)
    member this.ToLines =
        seq {
            yield "osu file format v14"
            yield ""
            yield "[General]"
            for key, value in this.General.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Editor]"
            for key, value in this.Editor.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Metadata]"
            for key, value in this.Metadata.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Difficulty]"
            for key, value in this.Difficulty.ToMap do
                yield sprintf "%s: %s" key value
            yield ""
            yield "[Events]"
            for object in this.Events do
                yield object.ToString()
            yield ""
            yield "[TimingPoints]"
            for object in this.Timing do
                yield object.ToString()
            yield ""
            // todo: [Colours]
            yield "[HitObjects]"
            for object in this.Objects do
                yield object.ToString()
            yield ""
        }


type Storyboard =
    {
        // todo: Variables
        Events: StoryboardObject list
    }
    member this.ToLines =
        seq {
            yield "[Events]"
            for object in this.Events do
                yield object.ToString()
        }

module private Parser =

    open System.IO

    let parse_failure message line =
        failwithf "osu! parse error: %s\nat: %s" message line

    let parse_storyboard_event (csv: string array) =
        match csv.[0].ToLowerInvariant() with
        | "0"
        | "background" ->   
            Background(
                (CsvHelpers.string_or 2 "" csv).Trim('"'),
                CsvHelpers.int_or 3 0 csv,
                CsvHelpers.int_or 4 0 csv
            )
            |> Some
        | "1"
        | "video" ->
            Video(
                CsvHelpers.int_or 1 0 csv,
                (CsvHelpers.string_or 2 "" csv).Trim('"'),
                CsvHelpers.int_or 3 0 csv,
                CsvHelpers.int_or 4 0 csv
            )
            |> Some
        | "2"
        | "break" ->
            Break(
                CsvHelpers.int_or 1 0 csv,
                CsvHelpers.int_or 2 0 csv
            )
            |> Some
        | "sample" ->
            Sample(
                CsvHelpers.int_or 1 0 csv,
                CsvHelpers.enum_or 2 Layer.Background csv,
                (CsvHelpers.string_or 3 "" csv).Trim('"'),
                CsvHelpers.int_or 4 0 csv
            )
            |> Some
        | "sprite"
        | "animation"
        | _ -> None

    let parse_timing_point (csv: string array) =
        let uninherited = CsvHelpers.int_or 6 1 csv <> 0
        if uninherited then
            Uninherited {
                Time = CsvHelpers.int_or 0 0 csv
                MsPerBeat = CsvHelpers.float_or 1 500.0 csv
                Meter = CsvHelpers.int_or 2 4 csv
                SampleSet = CsvHelpers.enum_or 3 SampleSet.Default csv
                SampleIndex = CsvHelpers.int_or 4 0 csv
                Volume = CsvHelpers.int_or 5 0 csv
                Effects = CsvHelpers.enum_or 7 TimingEffect.None csv
            }
        else
            Inherited {
                Time = CsvHelpers.int_or 0 0 csv
                Multiplier = -100.0 / (CsvHelpers.float_or 1 1.0 csv)
                METER__UNUSED = CsvHelpers.int_or 2 4 csv
                SampleSet = CsvHelpers.enum_or 3 SampleSet.Default csv
                SampleIndex = CsvHelpers.int_or 4 0 csv
                Volume = CsvHelpers.int_or 5 0 csv
                Effects = CsvHelpers.enum_or 7 TimingEffect.None csv
            }

    let parse_hit_sample (colon_separated_values: string array) =
        {
            NormalSet = CsvHelpers.enum_or 0 SampleSet.Default colon_separated_values
            AdditionSet = CsvHelpers.enum_or 1 SampleSet.Default colon_separated_values
            Index = CsvHelpers.int_or 2 0 colon_separated_values
            Volume = CsvHelpers.int_or 3 0 colon_separated_values
            Filename = CsvHelpers.string_or 4 "" colon_separated_values
        }

    let parse_hit_object (line: string) (csv: string array) =
        let x = CsvHelpers.int_or 0 0 csv
        let y = CsvHelpers.int_or 1 0 csv
        let time = CsvHelpers.int_or 2 0 csv
        let obj_type = CsvHelpers.int_or 3 0 csv
        let hitsound = CsvHelpers.enum_or 4 HitSound.Default csv

        let starts_new_combo = obj_type &&& 4 <> 0
        let color_hax = (obj_type >>> 4) &&& 7

        if obj_type &&& 1 > 0 then
            HitCircle {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                HitSample = 
                    CsvHelpers.string_or 5 "" csv
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        elif obj_type &&& 2 > 0 then
            let curve = CsvHelpers.string_or 5 "" csv |> fun s -> s.Split([|'|'|], 2, StringSplitOptions.TrimEntries)
            let curve_shape, curve_points =
                if curve.Length < 2 then
                    parse_failure "Invalid slider curve" line
                else
                    match curve.[0].ToUpperInvariant() with
                    | "B" -> Bezier
                    | "C" -> Catmull
                    | "L" -> Linear
                    | "P" -> PerfectCircle
                    | _ -> Bezier
                    ,
                    curve.[1].Split('|', StringSplitOptions.TrimEntries)
                    |> Seq.map (fun s -> 
                        let xy = s.Split(':', StringSplitOptions.TrimEntries)
                        CsvHelpers.int_or 0 0 xy, CsvHelpers.int_or 1 0 xy
                    )
                    |> List.ofSeq
            Slider {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                CurveType = curve_shape
                CurvePoints = curve_points
                Slides = CsvHelpers.int_or 6 1 csv
                Length = CsvHelpers.float_or 7 100.0 csv
                EdgeSounds = 
                    CsvHelpers.string_or 8 "" csv
                    |> fun s -> s.Split("|", StringSplitOptions.TrimEntries)
                    |> Seq.choose (fun n ->
                        match Int32.TryParse(n, CultureInfo.InvariantCulture) with
                        | true, v -> Some (enum v)
                        | false, _ -> None
                    )
                    |> List.ofSeq
                EdgeSets = 
                    CsvHelpers.string_or 9 "" csv
                    |> fun s -> s.Split("|", StringSplitOptions.TrimEntries)
                    |> Seq.map (fun s -> 
                        let sets = s.Split(':', StringSplitOptions.TrimEntries)
                        CsvHelpers.enum_or 0 SampleSet.None sets, CsvHelpers.enum_or 1 SampleSet.None sets
                    )
                    |> List.ofSeq
                HitSample = 
                    CsvHelpers.string_or 10 "" csv
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        elif obj_type &&& 8 > 0 then
            Spinner {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                EndTime = CsvHelpers.int_or 5 time csv
                HitSample = 
                    CsvHelpers.string_or 6 "" csv
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        elif obj_type &&& 128 > 0 then
            let endtime_and_sample = CsvHelpers.string_or 5 "" csv |> fun s -> s.Split([|':'|], 2, StringSplitOptions.TrimEntries)
            HoldNote {
                X = x
                Y = y
                Time = time
                StartsNewCombo = starts_new_combo
                ColorHax = color_hax
                HitSound = hitsound
                EndTime = CsvHelpers.int_or 0 time endtime_and_sample
                HitSample = 
                    CsvHelpers.string_or 1 "" endtime_and_sample
                    |> fun s -> s.Split(":", StringSplitOptions.TrimEntries)
                    |> parse_hit_sample
            }
        else
            parse_failure "Unrecognised object type" line

    [<Struct>]
    type private ParserState =
        | Nothing
        | Header
        | Events
        | TimingPoints
        | Objects
        | Colors

    let beatmap_from_stream (stream: Stream) : Beatmap =
        use reader = new StreamReader(stream)

        let mutable state = Nothing
        let general = ref Map.empty
        let editor = ref Map.empty
        let metadata = ref Map.empty
        let difficulty = ref Map.empty
        let mutable section_ref = general

        let objects = ResizeArray<HitObject>()
        let timing = ResizeArray<TimingPoint>()
        let events = ResizeArray<StoryboardObject>()

        while reader.Peek() >= 0 do
            let line = reader.ReadLine()
            let trimmed = line.Trim()
            match trimmed with
            | "" -> ()
            | _ when line.StartsWith("//") -> ()
            | "[General]" ->
                state <- Header
                section_ref <- general
            | "[Editor]" ->
                state <- Header
                section_ref <- editor
            | "[Metadata]" ->
                state <- Header
                section_ref <- metadata
            | "[Difficulty]" ->
                state <- Header
                section_ref <- difficulty
            | "[Events]" ->
                state <- Events
            | "[TimingPoints]" ->
                state <- TimingPoints
            | "[HitObjects]" ->
                state <- Objects
            | "[Colours]" -> // todo: support colors header
                state <- Colors
            | _ ->

            match state with
            | Nothing -> ()
            | Header ->
                let parts = trimmed.Split([|':'|], 2, StringSplitOptions.TrimEntries)
                if parts.Length = 2 then
                    section_ref.Value <- Map.add parts.[0] parts.[1] section_ref.Value
            | Events ->
                let csv = trimmed.Split(',', StringSplitOptions.TrimEntries)
                if csv.Length > 1 then
                    match parse_storyboard_event csv with
                    | Some event -> events.Add event
                    | None -> ()
            | TimingPoints ->
                let csv = trimmed.Split(',', StringSplitOptions.TrimEntries)
                if csv.Length > 1 && csv.Length < 8 then
                    parse_failure "Failed to parse timing point" line
                elif csv.Length >= 8 then
                    parse_timing_point csv |> timing.Add
            | Objects ->
                let csv = trimmed.Split(',', StringSplitOptions.TrimEntries)
                if csv.Length > 1 && csv.Length < 5 then
                    parse_failure "Failed to parse hit object" line
                elif csv.Length >= 5 then
                    parse_hit_object line csv |> objects.Add
            | Colors -> ()

        {
            General = General.FromMap general.Value
            Editor = Editor.FromMap editor.Value
            Metadata = Metadata.FromMap metadata.Value
            Difficulty = Difficulty.FromMap difficulty.Value
            Events = List.ofSeq events
            Objects = List.ofSeq objects
            Timing = List.ofSeq timing
        }

open System.IO
open System.Text

type Beatmap with
    static member FromFile(path: string) =
        try
            use stream = File.OpenRead(path)
            Ok (Parser.beatmap_from_stream stream)
        with err ->
            printfn "%s" err.StackTrace
            Error err.Message
    member this.ToFile(path: string) =
        this.ToLines |> String.concat "\n" |> fun contents -> File.WriteAllText(path, contents, Encoding.UTF8)
    member this.ToStream(stream: Stream) =
        use writer = new StreamWriter(stream, Encoding.UTF8)
        this.ToLines |> Seq.iter writer.WriteLine

type Storyboard with
    // todo: there is currently no support for reading a storyboard file, only generating one
    member this.ToFile(path: string) =
        this.ToLines |> String.concat "\n" |> fun contents -> File.WriteAllText(path, contents)