namespace Prelude.Data.Imports

open System
open System.IO
open System.Runtime.CompilerServices
open Percyqaz.Common
open Prelude.Formats.Conversions
open Prelude.Data
open Prelude.Data.Library

type MountImportTask(source: MountedChartSource, library: Library, since: DateTime option) =
    inherit BackgroundTask<ConversionResult>()

    override this.Run(ctx: ITaskContext) : Async<ConversionResult> =
        async {
            match source.Type with
            | Pack packname ->
                Logging.Info "Importing songs path %s as '%s'" source.SourceFolder packname

                let config =
                    ConversionOptions.Pack(packname, since, if source.CopyAssetFiles then Copy else Link)

                let! result =
                    FileSystemImports.convert_pack_folder(source.SourceFolder, config, library, ctx.set_Progress)

                log_conversion result
                source.LastImported <- Some DateTime.UtcNow
                return result

            | Library ->
                Logging.Info "Importing songs library %s" source.SourceFolder
                let mutable results = ConversionResult.Empty
                let pack_folders = Directory.GetDirectories(source.SourceFolder)

                for i, pack_folder in Array.indexed pack_folders do
                    let pack_name = Path.GetFileName(pack_folder)

                    let! result =
                        FileSystemImports.convert_pack_folder(
                            pack_folder,
                            ConversionOptions.Pack(pack_name, since, if source.CopyAssetFiles then Copy else Link),
                            library,
                            ctx.Subtask(pack_name, i + 1, pack_folders.Length).set_Progress
                        )

                    results <- results.CombineWith(result)

                log_conversion results
                source.LastImported <- Some DateTime.UtcNow
                return results
        }

    [<Extension>]
    static member ImportNewCharts(source: MountedChartSource, library: Library) : BackgroundTask<ConversionResult> =
        match source.LastImported with
        | Some date ->
            Logging.Info
                "Last import was %s, only importing song folders modified since then"
                (date.ToString("yyyy-MM-dd HH:mm:ss"))
        | None -> ()

        MountImportTask(source, library, source.LastImported)

    [<Extension>]
    static member ImportAllCharts(source: MountedChartSource, library: Library) : BackgroundTask<ConversionResult> =
        MountImportTask(source, library, None)
