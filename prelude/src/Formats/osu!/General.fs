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

