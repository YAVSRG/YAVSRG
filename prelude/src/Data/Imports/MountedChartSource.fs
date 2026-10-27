namespace Prelude.Data.Imports

open System
open System.IO
open Percyqaz.Data
open Percyqaz.Common
open Prelude.Formats.Conversions
open Prelude.Data
open Prelude.Data.ChartDatabase
open Prelude.Data.Library

[<Json.AutoCodec>]
type MountedChartSourceType =
    | Pack of name: string
    | Library

[<Json.AutoCodec(false)>]
type MountedChartSource =
    {
        SourceFolder: string
        Type: MountedChartSourceType
        ImportOnStartup: bool
        CopyAssetFiles: bool
        mutable LastImported: DateTime option
    }

    static member Create
        (source_type: MountedChartSourceType, path: string, import_on_startup: bool, copy_assets: bool)
        : MountedChartSource =
        {
            SourceFolder = path
            Type = source_type
            ImportOnStartup = import_on_startup
            CopyAssetFiles = copy_assets
            LastImported = None
        }

    member this.FindLinkedCharts(chart_db: ChartDatabase) : ChartMeta seq =
        seq {
            for chart_meta in chart_db.Entries |> Array.ofSeq do
                match chart_meta.Audio with
                | AssetLocation.Absolute path when path.StartsWith(this.SourceFolder) -> yield chart_meta
                | _ -> ()
        }

    member this.DeleteLinkedCharts(chart_db: ChartDatabase) : unit =
        let charts = this.FindLinkedCharts(chart_db)
        chart_db.Delete(charts)
