namespace Prelude.Data.Library.Imports

open System
open System.IO
open Prelude
open Prelude.Charts
open Prelude.Formats
open Prelude.Data
open Prelude.Data.Library
open Prelude.Data.User

module OnlineImports =

    let etterna_download_url (pack_name: string) : Async<Result<string, string>> =
        async {
            let url = "https://api.etternaonline.com/api/packs?page=1&limit=1&sort=name&filter[search]=" + Uri.EscapeDataString(pack_name)
            match! WebServices.download_json_async<_> url with
            | WebResult.Ok (response: {| data: {| download: string |} array |}) ->
                match response.data |> Array.tryExactlyOne with
                | Some d -> return Ok d.download
                | None -> return Error "No matching packs"
            | WebResult.HttpError i -> return Error (sprintf "Http error code %i" i)
            | WebResult.Exception err -> return Error err.Message
        }

    let private download_etterna_pack (name: string, url: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: float32 -> unit) : Async<Result<ConversionResult, string>> =
        async {
            let target = Path.Combine(get_game_folder "Downloads", Guid.NewGuid().ToString() + ".zip")
            match! WebServices.download_file.RequestAsync((url, target, progress)) with
            | false -> return Error "Download failure"
            | true ->
                match! Imports.convert_stepmania_pack_zip(target, name, chart_db, user_db) with
                | Some result ->
                    progress 1.0f
                    Imports.delete_file.Request(target, ignore)
                    return Ok result
                | None ->
                    Imports.delete_file.Request(target, ignore)
                    return Error "Error extracting/converting zip"
        }

    let private download_osu_set (url: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: float32 -> unit) : Async<Result<ConversionResult, string>> =
        async {
            let target = Path.Combine(get_game_folder "Downloads", Guid.NewGuid().ToString() + ".osz")
            match! WebServices.download_file.RequestAsync((url, target, progress)) with
            | false -> return Error "Download failure"
            | true ->
                match! Imports.auto_convert.RequestAsync(target, true, chart_db, user_db) with
                | Some result ->
                    progress 1.0f
                    Imports.delete_file.Request(target, ignore)
                    return Ok result
                | None ->
                    Imports.delete_file.Request(target, ignore)
                    return Error "Error extracting/converting osz"
        }

    let get_from_origin (origin: ChartOrigin, chart_db: ChartDatabase, user_db: UserDatabase, progress: float32 -> unit) =
        async {
            match origin with

            | ChartOrigin.Osu osu ->
                // todo: download via md5 if available
                let url = sprintf "https://catboy.best/d/%in" osu.BeatmapSetId
                return! download_osu_set (url, chart_db, user_db, progress)

            | ChartOrigin.Quaver quaver ->
                return Error "No mirror exists for Quaver"

            | ChartOrigin.Etterna (pack: string) ->
                match! etterna_download_url pack with
                | Error reason ->
                    return Error (sprintf "Error getting URL from EtternaOnline for '%s': %s" pack reason)
                | Ok url ->
                    return! download_etterna_pack (pack, url, chart_db, user_db, progress)
        }