namespace Prelude.Formats.Osu

open System
open System.Globalization

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