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

    static member FromMap(properties: Map<string, string>) : General =
        {
            AudioFilename = properties.StringOrDefault("AudioFilename", "")
            AudioLeadIn = properties.IntOrDefault("AudioLeadIn", 0)
            PreviewTime = properties.IntOrDefault("PreviewTime", -1)
            Countdown = properties.IntOrDefault("Countdown", 1) |> enum
            SampleSet = properties.EnumOrDefault("SampleSet", SampleSet.Soft)
            StackLeniency = properties.DoubleOrDefault("StackLeniency", 0.7)
            Mode = properties.EnumOrDefault("Mode", Gamemode.OSU)
            LetterboxInBreaks = properties.IntOrDefault("LetterBoxInBreaks", 0) <> 0
            UseSkinSprites = properties.IntOrDefault("UseSkinSprites", 0) <> 0
            OverlayPosition = properties.EnumOrDefault("OverlayPosition", OverlayPosition.NoChange)
            SkinPreference = properties.StringOrDefault("SkinPreference", "")
            EpilepsyWarning = properties.IntOrDefault("EpilepsyWarning", 0) <> 0
            CountdownOffset = properties.IntOrDefault("CountdownOffset", 0)
            SpecialStyle = properties.IntOrDefault("SpecialStyle", 0) <> 0
            WidescreenStoryboard = properties.IntOrDefault("WidescreenStoryboard", 0) <> 0
            SamplesMatchPlaybackRate = properties.IntOrDefault("SamplesMatchPlaybackRate", 0) <> 0
        }

    member this.ToMap: (string * string) seq =
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
            DistanceSpacing = 0.8
            BeatDivisor = 4
            GridSize = 4
            TimelineZoom = 1.0
        }

    static member FromMap(properties: Map<string, string>) : Editor =
        {
            Bookmarks =
                properties
                    .StringOrDefault("Bookmarks", "")
                    .Trim()
                    .Trim(',')
                    .Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                |> Seq.choose(fun s ->
                    match Int32.TryParse(s, CultureInfo.InvariantCulture) with
                    | true, v -> Some v
                    | false, _ -> None
                )
                |> List.ofSeq
            DistanceSpacing = properties.DoubleOrDefault("DistanceSpacing", 1.0)
            BeatDivisor = properties.IntOrDefault("BeatDivisor", 8)
            GridSize = properties.IntOrDefault("GridSize", 4)
            TimelineZoom = properties.DoubleOrDefault("TimelineZoom", 1.0)
        }

    member this.ToMap: (string * string) seq =
        seq {
            yield "Bookmarks", this.Bookmarks |> Seq.map _.ToString() |> String.concat ","
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

    static member FromMap(properties: Map<string, string>) : Metadata =
        {
            Title = properties.StringOrDefault("Title", "")
            TitleUnicode = properties.StringOrDefault("TitleUnicode", "")
            Artist = properties.StringOrDefault("Artist", "")
            ArtistUnicode = properties.StringOrDefault("ArtistUnicode", "")
            Creator = properties.StringOrDefault("Creator", "")
            Version = properties.StringOrDefault("Version", "")
            Source = properties.StringOrDefault("Source", "")
            Tags =
                properties
                    .StringOrDefault("Tags", "")
                    .Split(' ', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
                |> List.ofSeq
            BeatmapID = properties.IntOrDefault("BeatmapID", 0)
            BeatmapSetID = properties.IntOrDefault("BeatmapSetID", -1)
        }

    member this.ToMap: (string * string) seq =
        seq {
            yield "Title", this.Title
            yield "TitleUnicode", this.TitleUnicode
            yield "Artist", this.Artist
            yield "ArtistUnicode", this.ArtistUnicode
            yield "Creator", this.Creator
            yield "Version", this.Version
            yield "Source", this.Source
            yield "Tags", this.Tags |> String.concat " "
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

    static member FromMap(properties: Map<string, string>) : Difficulty =
        {
            HPDrainRate = properties.DoubleOrDefault("HPDrainRate", 8.0)
            CircleSize = properties.DoubleOrDefault("CircleSize", 4.0)
            OverallDifficulty = properties.DoubleOrDefault("OverallDifficulty", 8.0)
            ApproachRate = properties.DoubleOrDefault("ApproachRate", 5.0)
            SliderMultiplier = properties.DoubleOrDefault("SliderMultiplier", 1.4)
            SliderTickRate = properties.DoubleOrDefault("SliderTickRate", 1.0)
        }

    member this.ToMap: (string * string) seq =
        seq {
            yield "HPDrainRate", this.HPDrainRate.ToString(CultureInfo.InvariantCulture)
            yield "CircleSize", this.CircleSize.ToString(CultureInfo.InvariantCulture)
            yield "OverallDifficulty", this.OverallDifficulty.ToString(CultureInfo.InvariantCulture)
            yield "ApproachRate", this.ApproachRate.ToString(CultureInfo.InvariantCulture)
            yield "SliderMultiplier", this.SliderMultiplier.ToString(CultureInfo.InvariantCulture)
            yield "SliderTickRate", this.SliderTickRate.ToString(CultureInfo.InvariantCulture)
        }
