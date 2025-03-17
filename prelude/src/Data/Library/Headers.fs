namespace Prelude.Data.Library

open System.IO
open Percyqaz.Data
open Prelude
open Prelude.Charts
open Prelude.Calculator
open Prelude.Calculator.Patterns

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type AssetPath =
    | Absolute of string
    | Hash of string
    | Missing
    member this.Path : string option =
        match this with
        | Absolute p -> Some p
        | Hash h -> Path.Combine(get_game_folder "Songs", ".assets", h.Substring(0, 2), h) |> Some
        | Missing -> None

type ChartMeta =
    {
        Hash: string

        Title: string
        TitleNative: string option
        Artist: string
        ArtistNative: string option
        DifficultyName: string
        Subtitle: string option
        Source: string option
        Creator: string
        Tags: string list

        Background: AssetPath
        Audio: AssetPath
        PreviewTime: Time

        Packs: Set<string>
        Origins: Set<ChartOrigin>

        Keys: int
        Length: Time
        BPM: int
        DateAdded: int64
        Rating: float32
        Patterns: PatternReport
    }

    member this.OriginString =
        match Seq.tryHead this.Packs with
        | Some pack -> pack
        | None ->
            match Seq.tryHead this.Origins with
            | Some o -> o.ToString()
            | None -> "Unknown"

    member this.MergeWithExisting (existing: ChartMeta) : ChartMeta * bool =
        // Tiebreaker rule when importing data over the top of an existing chart
        // Etterna metadata takes priority over osu, etc
        let existing_data_priority =
            not (Set.isEmpty existing.Origins) && existing.Origins < this.Origins

        if existing_data_priority then
            { existing with
                TitleNative = existing.TitleNative |> Option.orElse this.TitleNative
                ArtistNative = existing.ArtistNative |> Option.orElse this.ArtistNative
                Subtitle = existing.Subtitle |> Option.orElse this.Subtitle
                Source = existing.Source |> Option.orElse this.Source
                Packs = Set.union this.Packs existing.Packs
                Origins = Set.union this.Origins existing.Origins
                Background = this.Background
            },
            false
        else
            { this with
                TitleNative = this.TitleNative |> Option.orElse existing.TitleNative
                ArtistNative = this.ArtistNative |> Option.orElse existing.ArtistNative
                Subtitle = this.Subtitle |> Option.orElse existing.Subtitle
                Source = this.Source |> Option.orElse existing.Source
                Packs = Set.union this.Packs existing.Packs
                Origins = Set.union this.Origins existing.Origins
            },
            true

    static member FromImport (timestamp: int64) (handle_asset: ImportAsset -> AssetPath) (import_chart: ImportChart) : ChartMeta =
        let source_folder_path = Path.GetDirectoryName(import_chart.LoadedFromPath)
        let chart = import_chart.Chart
        let difficulty = Difficulty.calculate(1.0f<rate>, chart.Notes)

        {
            Hash = Chart.hash chart

            Title = import_chart.Header.Title
            TitleNative = import_chart.Header.TitleNative
            Artist = import_chart.Header.Artist
            ArtistNative = import_chart.Header.ArtistNative
            DifficultyName = import_chart.Header.DiffName
            Subtitle = import_chart.Header.Subtitle
            Source = import_chart.Header.Source
            Creator = import_chart.Header.Creator
            Tags = import_chart.Header.Tags

            Background = handle_asset import_chart.Header.BackgroundFile
            Audio = handle_asset import_chart.Header.AudioFile
            PreviewTime = import_chart.Header.PreviewTime

            Packs = Set.singleton import_chart.PackName
            Origins = import_chart.Header.Origins

            Keys = chart.Keys
            Length = chart.LastNote - chart.FirstNote
            BPM =
                let mspb = Chart.find_most_common_bpm chart
                let bpm = 60000.0f<ms/minute> / mspb |> float32
                if System.Single.IsFinite(bpm) then
                    bpm |> round |> int
                else 0
            DateAdded = timestamp
            Rating = difficulty.Overall
            Patterns = (PatternReport.from_chart(difficulty, chart))
        }