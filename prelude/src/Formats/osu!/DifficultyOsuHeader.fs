namespace Prelude.Formats.Osu

open System.Globalization

type DifficultyOsuHeader =
    {
        HPDrainRate: float
        CircleSize: float
        OverallDifficulty: float
        ApproachRate: float
        SliderMultiplier: float
        SliderTickRate: float
    }

    static member FromMap(properties: Map<string, string>) : DifficultyOsuHeader =
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
