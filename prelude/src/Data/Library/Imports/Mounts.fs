namespace Prelude.Data.Library.Imports

open System
open System.IO
open Percyqaz.Data
open Percyqaz.Common
open Prelude.Charts
open Prelude.Charts.Conversions
open Prelude.Data.Library

[<Json.AutoCodec>]
type MountedChartSourceType =
    | Pack of name: string
    | Library

[<Json.AutoCodec>]
type MountedChartSource =
    {
        SourceFolder: string
        mutable LastImported: DateTime option
        Type: MountedChartSourceType
        ImportOnStartup: bool
    }
    static member Pack(name: string, path: string) =
        {
            SourceFolder = path
            LastImported = None
            Type = Pack name
            ImportOnStartup = false
        }

    static member Library(path: string) =
        {
            SourceFolder = path
            LastImported = None
            Type = Library
            ImportOnStartup = false
        }

module Mount =

    let import_service =
        { new Async.Service<MountedChartSource * Library, ConversionResult>() with
            override this.Handle((source, library)) =
                async {
                    match source.Type with
                    | Pack packname ->
                        Logging.Info "Importing songs path %s as '%s'" source.SourceFolder packname
                        match source.LastImported with
                        | Some date -> Logging.Info "Last import was %s, only importing song folders modified since then" (date.ToString("yyyy-MM-dd HH:mm:ss"))
                        | None -> ()
                        let config =
                            { ConversionOptions.Default with
                                MoveAssets = false
                                ChangedAfter = source.LastImported
                                PackName = packname
                            }

                        let! result = Imports.convert_pack_folder.RequestAsync(source.SourceFolder, config, library)
                        source.LastImported <- Some DateTime.UtcNow
                        log_conversion result
                        return result
                    | Library ->
                        Logging.Info "Importing songs library %s" source.SourceFolder
                        match source.LastImported with
                        | Some date -> Logging.Info "Last import was %s, only importing song folders modified since then" (date.ToString("yyyy-MM-dd HH:mm:ss"))
                        | None -> ()
                        let mutable results = ConversionResult.Empty
                        for pack_folder in Directory.EnumerateDirectories source.SourceFolder do
                            let! result =
                                Imports.convert_pack_folder.RequestAsync(
                                    pack_folder,
                                    { ConversionOptions.Default with
                                        MoveAssets = false
                                        ChangedAfter = source.LastImported
                                        PackName = Path.GetFileName pack_folder
                                    },
                                    library
                                )
                            results <- ConversionResult.Combine result results

                        log_conversion results
                        source.LastImported <- Some DateTime.UtcNow
                        return results
                }
        }