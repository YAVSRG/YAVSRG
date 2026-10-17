namespace Prelude.Charts

open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude

(*
    These headers are used during the process of importing a chart
    Once a chart has been successfully imported into the game's database it's stored as a different object with pre-cached info about its patterns

    Sources for headers:
    - Some kind of future .yav file format that allows charts to be imported and exported as files
    - Generated when converting a different chart file format
    - Generated when downloading a chart from the cdn
*)

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type ImportAsset =
    | Copy of absolute_path: string
    | Link of absolute_path: string
    | Download of hash: string
    | Missing

[<Json.AutoCodec(false)>]
[<StructuredFormatDisplay("{InfoString}")>]
type ChartImportHeader =
    {
        Title: string
        TitleNative: string option
        Artist: string
        ArtistNative: string option
        Creator: string
        DiffName: string
        Subtitle: string option
        Source: string option
        Tags: string list

        PreviewTime: Time
        BackgroundFile: ImportAsset
        AudioFile: ImportAsset

        mutable Origins: Set<ChartOrigin>
    }
    static member Default =
        {
            Title = "Untitled Chart"
            TitleNative = None
            Artist = ""
            ArtistNative = None
            Creator = ""
            DiffName = ""
            Subtitle = None
            Source = None
            Tags = []

            PreviewTime = 0.0f<ms>
            BackgroundFile = ImportAsset.Missing
            AudioFile = ImportAsset.Missing

            Origins = Set.empty
        }
        
    member this.InfoString = sprintf "%s - %s (%s) [%s] %s" this.Artist this.Title this.Creator this.DiffName (this.Origins |> Seq.map _.InfoString |> String.concat ", ")

type ImportChart =
    {
        Header: ChartImportHeader
        LoadedFromPath: string
        PackName: string
        Chart: Chart
    }

module ImportChart =

    // this unused code used to be for reading .yav files, which no longer exist
    // todo: think clearly what this should be and do or remove it
    let legacy_from_yav_file (pack_name: string) (path: string) : Result<ImportChart, string> =
        try
            use fs = new FileStream(path, FileMode.Open)
            use br = new BinaryReader(fs)
            let keys = br.ReadByte() |> int

            let header =
                match JSON.FromString(br.ReadString()) with
                | Ok v -> v
                | Error err ->
                    Logging.Error "%O" err
                    raise err

            match Chart.ReadFromStreamHeadless(keys, br) with
            | Ok chart ->
                Ok {
                    PackName = pack_name
                    Header = header
                    LoadedFromPath = path
                    Chart = chart
                }
            | Error reason -> Error reason

        with err -> Error err.Message