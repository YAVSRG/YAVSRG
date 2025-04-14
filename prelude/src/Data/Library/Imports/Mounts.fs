namespace Prelude.Data.Library.Imports

open System
open System.IO
open Percyqaz.Data
open Percyqaz.Common
open Prelude.Formats
open Prelude.Data
open Prelude.Data.Library
open Prelude.Data.User

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

    static member Create(source_type: MountedChartSourceType, path: string, import_on_startup: bool, copy_assets: bool) : MountedChartSource =
        {
            SourceFolder = path
            Type = source_type
            ImportOnStartup = import_on_startup
            CopyAssetFiles = copy_assets
            LastImported = None
        }

module Mount =

    let import_new (source: MountedChartSource, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<Result<ConversionResult, string>> =
        async {
            try
                match source.Type with
                | Pack packname ->
                    Logging.Info "Importing songs path %s as '%s'" source.SourceFolder packname
                    match source.LastImported with
                    | Some date -> Logging.Info "Last import was %s, only importing song folders modified since then" (date.ToString("yyyy-MM-dd HH:mm:ss"))
                    | None -> ()
                    let config = ConversionOptions.Pack(packname, source.LastImported, if source.CopyAssetFiles then CopyAssetFiles else LinkAssetFiles)
                    let! result = Imports.convert_pack_folder(source.SourceFolder, config, chart_db, user_db, progress)

                    log_conversion result
                    source.LastImported <- Some DateTime.UtcNow
                    progress Complete
                    return Ok result

                | Library ->
                    Logging.Info "Importing songs library %s" source.SourceFolder
                    match source.LastImported with
                    | Some date -> Logging.Info "Last import was %s, only importing song folders modified since then" (date.ToString("yyyy-MM-dd HH:mm:ss"))
                    | None -> ()
                    let mutable results = ConversionResult.Empty
                    let pack_folders = Directory.GetDirectories source.SourceFolder

                    for i, pack_folder in Array.indexed pack_folders do
                        let pack_name = Path.GetFileName pack_folder

                        let! result =
                            Imports.convert_pack_folder(
                                pack_folder,
                                ConversionOptions.Pack(pack_name, source.LastImported, if source.CopyAssetFiles then CopyAssetFiles else LinkAssetFiles),
                                chart_db,
                                user_db,
                                (fun p -> Nested (pack_name, i + 1, pack_folders.Length, p)) >> progress
                            )
                        results <- ConversionResult.Combine result results

                    log_conversion results
                    source.LastImported <- Some DateTime.UtcNow
                    progress Complete
                    return Ok results
            with err ->
                Logging.Error "Unexpected exception while importing '%s': %O" source.SourceFolder err
                progress Faulted
                return Error err.Message
        }

    let import_all (source: MountedChartSource, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<Result<ConversionResult, string>> =
        async {
            source.LastImported <- None
            return! import_new(source, chart_db, user_db, progress)
        }

    let find_linked_charts (source: MountedChartSource, chart_db: ChartDatabase) : ChartMeta seq =
        seq {
            for chart_meta in chart_db.Entries |> Array.ofSeq do
                match chart_meta.Audio with
                | AssetPath.Absolute path when path.StartsWith(source.SourceFolder) ->
                    yield chart_meta
                | _ -> ()
        }

    let delete_linked_charts (source: MountedChartSource, chart_db: ChartDatabase) : unit =
        let charts = find_linked_charts (source, chart_db)
        ChartDatabase.delete_many charts chart_db