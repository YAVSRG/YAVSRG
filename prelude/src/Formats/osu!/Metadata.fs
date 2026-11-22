namespace Prelude.Formats.Osu

open System
open System.Globalization

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

