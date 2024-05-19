namespace Prelude.Backbeat.Archive

open System
open System.Collections.Generic
open Percyqaz.Data
open Prelude
open Prelude.Charts

type ArtistName = string

[<Json.AutoCodec>]
type VerifiedArtist =
    {
        Alternatives: string list
        IsJapaneseFullName: bool
    }
// store of artists that are confirmed to exist, with the correct STYLiSATION of their name
[<Json.AutoCodec>]
type VerifiedArtists =
    {
        Artists: Dictionary<string, VerifiedArtist>
        Aliases: Dictionary<string, string list>
    }
    // all-lowercase string map to correct stylised artist name, if exists
    member this.FixerMapping() =
        let d = Dictionary<string, string>()

        for a in this.Artists.Keys do
            let v = this.Artists.[a]
            d.Add(a.ToLower(), a)

            for alt in v.Alternatives do
                d.Add(alt.ToLower(), a)

            if v.IsJapaneseFullName then
                let split = a.Split(' ', 2)
                d.Add((split.[1] + " " + split.[0]).ToLower(), a)

        d
    // case-sensitive mapping of what strings map to what parsed artist
    member this.ParserMapping() =
        let d = Dictionary<string, string>()

        for a in this.Artists.Keys do
            let v = this.Artists.[a]
            d.Add(a, a)

            for alt in v.Alternatives do
                d.Add(alt, a)

            if v.IsJapaneseFullName then
                let split = a.Split(' ', 2)
                d.Add((split.[1] + " " + split.[0]), a)

        for alias in this.Aliases.Keys do
            d.Add(alias, alias)

        d

type SongId = string

[<Json.AutoCodec>]
type Song =
    {
        /// Non-empty list of main artists/arrangers/composers
        Artists: ArtistName list
        /// List of additional features/performers
        OtherArtists: ArtistName list
        /// List of additional remixers/cover artists
        Remixers: ArtistName list
        /// Song title, prefer something romanised / ASCII printable
        Title: string
        /// Alternative song titles, no romanisation requirements
        AlternativeTitles: string list
        /// None for unknown, for original album/place of release
        Source: string option
        /// Various user-defined tags
        Tags: string list
    }
    member this.FormattedArtists =
        String.concat ", " this.Artists
        + if this.OtherArtists <> [] then
              " ft. " + String.concat ", " this.OtherArtists
          else
              ""

    member this.FormattedTitle = this.FormattedArtists + " - " + this.Title

    member this.MergeWithIncoming(song: Song) =
        { this with
            Source = match this.Source with Some existing -> Some existing | None -> song.Source
            Tags = List.distinct (this.Tags @ song.Tags)
        }

type StepmaniaPackId = int

[<Json.AutoCodec>]
type StepmaniaPack =
    {
        Title: string
        Mirrors: string list
        Size: int64
    }

[<Json.AutoCodec>]
type ChartSource =
    | Osu of {| BeatmapId: int; BeatmapSetId: int |}
    | Stepmania of id: int
    | CommunityPack of id: string

type ChartHash = string

[<Json.AutoCodec>]
type Chart =
    {
        Creators: string list
        DifficultyName: string
        Subtitle: string option
        Tags: string list
        Duration: Time
        PreviewTime: Time
        Notecount: int
        Keys: int
        BPM: (float32<ms / beat> * float32<ms / beat>)
        BackgroundHash: string
        AudioHash: string
        Sources: ChartSource list
    }
    member this.FormattedCreators = String.concat ", " this.Creators

    member this.MergeWithIncoming(chart: Chart) =
        { this with
            Subtitle = match this.Subtitle with Some existing -> Some existing | None -> chart.Subtitle
            Tags = List.distinct (this.Tags @ chart.Tags)
            Sources = List.distinct (this.Sources @ chart.Sources)
            BackgroundHash = chart.BackgroundHash
            AudioHash = chart.AudioHash
        }

module Archive =

    let make_chart_header (chart: Chart, song: Song) : ChartHeader =
        {
            Title = song.Title
            TitleNative = None
            Artist = song.FormattedArtists
            ArtistNative = None
            Creator = chart.Creators |> String.concat ", "
            DiffName = chart.DifficultyName
            Subtitle = chart.Subtitle
            Source = song.Source
            Tags = chart.Tags
            PreviewTime = chart.PreviewTime
            BackgroundFile = Asset chart.BackgroundHash
            AudioFile = Asset chart.AudioHash
            ChartSource =
                match chart.Sources with
                | Osu d :: _ -> Origin.Osu(d.BeatmapSetId, d.BeatmapId)
                | Stepmania d :: _ -> Origin.Stepmania d
                | _ -> Origin.Unknown
        }

    module DownloadUrl =

        let into_base64 (str: string) =
            str |> Text.Encoding.UTF8.GetBytes |> Convert.ToBase64String

        let from_base64 (str: string) =
            str |> Convert.FromBase64String |> Text.Encoding.UTF8.GetString

        let create (str: string) =
            str.Replace("https://", "").Replace("http://", "") |> into_base64

        let unpickle (str: string) =
            "https://" + Uri.EscapeDataString(from_base64 str)
