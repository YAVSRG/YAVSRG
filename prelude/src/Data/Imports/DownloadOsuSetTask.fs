namespace Prelude.Data.Imports

open System
open System.IO
open Prelude
open Prelude.Formats.Conversions
open Prelude.Data.Imports
open Prelude.Data.Library

type DownloadOsuSetTask(url: string, library: Library) =
    inherit BackgroundTask<ConversionResult>()

    override this.Run(ctx: ITaskContext) : Async<ConversionResult> =
        async {
            let target =
                Path.Combine(get_game_folder "Downloads", Guid.NewGuid().ToString() + ".osz")

            match! WebServices.download_file.RequestAsync((url, target, ctx.DownloadProgress)) with
            | false -> return failwith "Download failure"
            | true ->
                match! FileSystemImportTask(target, library).Run(ctx) with
                | Some result ->
                    FileSystemServices.delete_file.Request(target, ignore)
                    return result
                | None ->
                    FileSystemServices.delete_file.Request(target, ignore)
                    return failwithf "Downloaded .osz was not importable!"
        }
