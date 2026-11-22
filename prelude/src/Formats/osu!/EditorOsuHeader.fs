namespace Prelude.Formats.Osu

open System
open System.Globalization

type EditorOsuHeader =
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

    static member FromMap(properties: Map<string, string>) : EditorOsuHeader =
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
