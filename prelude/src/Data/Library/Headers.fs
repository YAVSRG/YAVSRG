namespace Prelude.Data.Library

open System.IO
open Percyqaz.Data
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing.Difficulty
open Prelude.Charts.Processing.Patterns

[<Json.AutoCodec>]
type AssetPath =
    | Absolute of string
    | Hash of string
    | Missing
    member this.Path : string option =
        match this with
        | Absolute p -> Some p
        | Hash h -> Path.Combine(get_game_folder "Songs", ".assets", h.Substring(0, 2), h) |> Some
        | Missing -> None

[<Json.AutoCodec>]
type ChartOrigin =
    | Osu of beatmapsetid: int * beatmapid: int
    | Quaver of mapsetid: int * mapid: int
    | Etterna of pack_name: string
    | Unknown

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

        Folders: Set<string>
        Origin: ChartOrigin

        Keys: int
        Length: Time
        BPM: int
        DateAdded: int64
        Rating: float32
        Patterns: PatternSummary.Info
    }
    static member FromImport (timestamp: int64) (import_chart: ImportChart) =
        let source_folder_path = Path.GetDirectoryName(import_chart.LoadedFromPath)
        let source_folder_name = Path.GetDirectoryName(import_chart.LoadedFromPath) |> Path.GetFileName
        let chart = import_chart.Chart
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

            Background =
                match import_chart.Header.BackgroundFile with
                | ChartImportAssetPath.Asset s -> AssetPath.Hash s
                | ChartImportAssetPath.Relative f -> AssetPath.Absolute (Path.Combine(source_folder_path, f))
                | ChartImportAssetPath.Absolute p -> AssetPath.Absolute p
                | ChartImportAssetPath.Missing -> AssetPath.Missing
            Audio =
                match import_chart.Header.AudioFile with
                | ChartImportAssetPath.Asset s -> AssetPath.Hash s
                | ChartImportAssetPath.Relative f -> AssetPath.Absolute (Path.Combine(source_folder_path, f))
                | ChartImportAssetPath.Absolute p -> AssetPath.Absolute p
                | ChartImportAssetPath.Missing -> AssetPath.Missing
            PreviewTime = import_chart.Header.PreviewTime

            Folders = Set.singleton source_folder_name
            Origin = 
                match import_chart.Header.ChartSource with
                | ChartImportOrigin.Osu (set, map) -> ChartOrigin.Osu(set, map)
                | ChartImportOrigin.Quaver (set, map) -> ChartOrigin.Quaver(set, map)
                | ChartImportOrigin.Etterna pack -> ChartOrigin.Etterna pack
                | ChartImportOrigin.Stepmania _ -> ChartOrigin.Etterna source_folder_name
                | ChartImportOrigin.Unknown -> ChartOrigin.Unknown

            Keys = chart.Keys
            Length = chart.LastNote - chart.FirstNote
            BPM = 
                let mspb = Chart.find_most_common_bpm chart
                let bpm = 60000.0f<ms/minute> / mspb |> float32
                if System.Single.IsFinite(bpm) then 
                    bpm |> round |> int 
                else 0
            DateAdded = timestamp
            Rating = (DifficultyRating.calculate 1.0f chart.Notes).Physical |> float32
            Patterns = (PatternSummary.generate_pattern_data_uncached 1.0f chart)
        }