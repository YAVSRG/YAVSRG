namespace Prelude.Charts

open System
open System.IO
open Percyqaz.Common
open Percyqaz.Data
open Prelude

[<Json.AutoCodec>]
type OsuOrigin =
    {
        Md5: string
        BeatmapSetId: int
        BeatmapId: int
        SourceRate: Rate
        SourceOD: float32
        FirstNoteOffset: Time
    }

[<Json.AutoCodec>]
type QuaverOrigin =
    {
        Md5: string
        MapSetId: int
        MapId: int
        FirstNoteOffset: Time
    }

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
[<CustomComparison>]
[<CustomEquality>]
type ChartOrigin =
    | Osu of OsuOrigin
    | Quaver of QuaverOrigin
    | Etterna of pack_name: string

    override this.ToString() =
        match this with
        | Osu _ -> "osu!"
        | Quaver _ -> "Quaver"
        | Etterna pack -> pack

    member this.InfoString : string =
        match this with
        | Osu osu -> sprintf "osu!|%s|%i|%i" osu.Md5 osu.BeatmapSetId osu.BeatmapId
        | Quaver quaver -> sprintf "Quaver|%s|%i|%i" quaver.Md5 quaver.MapSetId quaver.MapId
        | Etterna pack -> sprintf "Etterna|%s" pack

    member this.SuitableForUpload : bool =
        match this with
        | Osu osu -> osu.BeatmapSetId <> -1 && osu.BeatmapId <> 0
        | Quaver quaver -> quaver.MapSetId <> -1 && quaver.MapId <> 0
        | Etterna _ -> true

    interface IComparable with
        override this.CompareTo(other) : int =
            match other with
            | :? ChartOrigin as origin ->
                match this, origin with
                | Etterna p1, Etterna p2 -> p1.CompareTo p2
                | Osu osu1, Osu osu2 ->
                    if osu1.SourceRate = osu2.SourceRate then
                        osu2.BeatmapId.CompareTo osu1.BeatmapId
                    else
                        (float32 osu1.SourceRate - 1.0f |> abs).CompareTo (float32 osu2.SourceRate - 1.0f |> abs)
                | Quaver quaver1, Quaver quaver2 -> quaver2.MapId.CompareTo quaver1.MapId
                | Etterna _, _ -> -1
                | _, Etterna _ -> 1
                | Osu _, Quaver _ -> -1
                | Quaver _, Osu _ -> 1
            | _ -> -1

    override this.Equals (other: obj) : bool =
        match other with
            | :? ChartOrigin as origin ->
                match this, origin with
                | Etterna p1, Etterna p2 -> p1 = p2
                | Osu osu1, Osu osu2 -> osu1 = osu2
                | Quaver quaver1, Quaver quaver2 -> quaver1 = quaver2
                | _ -> false
            | _ -> false

    override this.GetHashCode (): int =
        match this with
        | Etterna p -> p.GetHashCode()
        | Osu osu -> osu.GetHashCode()
        | Quaver quaver -> quaver.GetHashCode()

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

type ImportChart =
    {
        Header: ChartImportHeader
        LoadedFromPath: string
        PackName: string
        Chart: Chart
    }

module ImportChart =

    let from_file (pack_name: string) (path: string) : Result<ImportChart, string> =
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

            match Chart.read_headless keys br with
            | Ok chart ->
                Ok {
                    PackName = pack_name
                    Header = header
                    LoadedFromPath = path
                    Chart = chart
                }
            | Error reason -> Error reason

        with err -> Error err.Message