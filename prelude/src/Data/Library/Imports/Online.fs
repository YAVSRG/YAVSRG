namespace Prelude.Data.Library.Imports

open System
open System.IO
open Percyqaz.Common
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

    let download_etterna_pack (name: string, url: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<Result<ConversionResult, string>> =
        async {
            try
                let target = Path.Combine(get_game_folder "Downloads", Guid.NewGuid().ToString() + ".zip")
                match! WebServices.download_file.RequestAsync((url, target, Downloading >> progress)) with
                | false ->
                    progress Faulted
                    return Error "Download failure"
                | true ->
                    match! Imports.convert_stepmania_pack_zip(target, name, chart_db, user_db, progress) with
                    | Ok result ->
                        Imports.delete_file.Request(target, ignore)
                        progress Complete
                        return Ok result
                    | Error reason ->
                        Imports.delete_file.Request(target, ignore)
                        progress Faulted
                        return Error reason
            with err ->
                Logging.Error "Unexpected exception while downloading '%s': %O" name err
                progress Faulted
                return Error err.Message
        }

    let download_osu_set (url: string, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) : Async<Result<ConversionResult, string>> =
        async {
            try
                let target = Path.Combine(get_game_folder "Downloads", Guid.NewGuid().ToString() + ".osz")
                match! WebServices.download_file.RequestAsync((url, target, Downloading >> progress)) with
                | false ->
                    progress Faulted
                    return Error "Download failure"
                | true ->
                    match! Imports.auto_detect_import(target, chart_db, user_db, progress) with
                    | Ok result ->
                        Imports.delete_file.Request(target, ignore)
                        progress Complete
                        return Ok result
                    | Error reason ->
                        Imports.delete_file.Request(target, ignore)
                        progress Faulted
                        return Error reason
            with err ->
                Logging.Error "Unexpected exception while downloading '%s': %O" url err
                progress Faulted
                return Error err.Message
        }

    let download_by_origin (origin: ChartOrigin, chart_db: ChartDatabase, user_db: UserDatabase, progress: ProgressCallback) =
        async {
            match origin with

            | ChartOrigin.Osu osu ->
                // todo: download via md5 if available
                let url = sprintf "https://catboy.best/d/%in" osu.BeatmapSetId
                return! download_osu_set (url, chart_db, user_db, progress)

            | ChartOrigin.Quaver quaver ->
                progress Faulted
                return Error "No mirror exists for Quaver"

            | ChartOrigin.Etterna (pack: string) ->
                match! etterna_download_url pack with
                | Error reason ->
                    progress Faulted
                    return Error (sprintf "Error getting URL from EtternaOnline for '%s': %s" pack reason)
                | Ok url ->
                    return! download_etterna_pack (pack, url, chart_db, user_db, progress)
        }

    open Prelude.Backbeat.Archive
    open Prelude.Data.WebServices
    open System.Net.Http

    let private httpclient = new HttpClient()

    // todo: ought to be internal due to its design
    let cdn_install (folder: string, hash: string, chart: Chart, song: Song, chart_db: ChartDatabase) : Async<Result<unit, string>> =
        async {
            let header = Archive.make_chart_header (chart, song)

            try
                let! response = httpclient.GetAsync("https://cdn.yavsrg.net/" + hash) |> Async.AwaitTask

                if not response.IsSuccessStatusCode then
                    return Error "Chart notes not found on server"
                else

                    use! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                    use br = new BinaryReader(stream)

                    match Chart.read_headless chart.Keys br with
                    | Error reason ->
                        return Error (sprintf "Malformed chart: %s" reason)
                    | Ok chart_data ->

                    let actual_hash = Chart.hash chart_data

                    if actual_hash <> hash then
                        return Error (sprintf "Hash mismatch: '%s' expected vs '%s' actual" hash actual_hash)
                    else

                    if File.Exists(ChartDatabase.asset_path chart.BackgroundHash chart_db) |> not then

                        let bg_path = Path.Combine(get_game_folder "Downloads", chart.BackgroundHash)

                        let! success =
                            download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.BackgroundHash,
                                bg_path,
                                ignore
                            )

                        if not success then failwithf "Error downloading background '%s'" chart.BackgroundHash

                        let actual_bg_hash = ChartDatabase.hash_asset bg_path chart_db

                        if chart.BackgroundHash <> actual_bg_hash then
                            failwithf
                                "Downloaded background hash was '%s', expected '%s'"
                                actual_bg_hash
                                chart.BackgroundHash

                    if File.Exists(ChartDatabase.asset_path chart.AudioHash chart_db) |> not then

                        let audio_path = Path.Combine(get_game_folder "Downloads", chart.AudioHash)

                        let! success =
                            download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.AudioHash,
                                audio_path,
                                ignore
                            )

                        if not success then failwithf "Error downloading audio '%s'" chart.BackgroundHash

                        let actual_audio_hash = ChartDatabase.hash_asset audio_path chart_db

                        if chart.AudioHash <> actual_audio_hash then
                            failwithf "Downloaded audio hash was '%s', expected '%s'" actual_audio_hash chart.AudioHash

                    ChartDatabase.import [{ PackName = folder; LoadedFromPath = ""; Header = header; Chart = chart_data }] chart_db
                    return Ok()
            with err ->
                // todo: specific exception type for asset errors, handle uncaught exceptions differently
                return Error err.Message
        }