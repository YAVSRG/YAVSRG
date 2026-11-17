namespace Prelude.Data.Imports

open System
open System.IO
open Prelude
open Prelude.Formats.Conversions
open Prelude.Data.Imports
open Prelude.Data.Library

type DownloadEtternaPackTask(name: string, url: string, library: Library) =
    inherit BackgroundTask<ConversionResult>()

    override this.Run(ctx: ITaskContext) : Async<ConversionResult> =
        async {
            let target =
                Path.Combine(get_game_folder "Downloads", Guid.NewGuid().ToString() + ".zip")

            match! WebServices.download_file.RequestAsync((url, target, ctx.DownloadProgress)) with
            | false -> return failwith "Download failure"
            | true ->
                let! result = FileSystemImports.convert_stepmania_pack_zip(target, name, library, ctx)
                FileSystemServices.delete_file.Request(target, ignore)
                return result
        }

    static member GetDownloadUrl(pack_name: string) : Async<Result<string, string>> =
        async {
            let url =
                "https://api.etternaonline.com/api/packs?page=1&limit=1&sort=name&filter[search]="
                + Uri.EscapeDataString(pack_name)

            match! WebServices.download_json_async<_> url with
            | WebResult.Ok(response: {| data: {| download: string |} array |}) ->
                match response.data |> Array.tryExactlyOne with
                | Some d -> return Ok d.download
                | None -> return Error "No matching packs"
            | WebResult.HttpError i -> return Error(sprintf "Http error code %i" i)
            | WebResult.Exception err -> return Error err.Message
        }
