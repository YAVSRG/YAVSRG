namespace Prelude.Data.Imports

open System.IO
open System.Net.Http
open Prelude
open Prelude.Charts
open Prelude.Formats.Conversions
open Prelude.Data
open Prelude.Data.ChartDatabase
open Prelude.Data.Library
open Prelude.Backbeat.Archive

module OnlineImports =

    let download_by_origin
        (origin: ChartOrigin, library: Library, ctx: ITaskContext)
        : Async<Result<ConversionResult, string>> =
        async {
            match origin with

            | ChartOrigin.Osu osu ->
                // todo: download via md5 if available
                let url = sprintf "https://catboy.best/d/%in" osu.BeatmapSetId
                let! result = DownloadOsuSetTask(url, library).Run(ctx)
                return Ok(result)

            | ChartOrigin.Quaver _ -> return Error "No mirror exists for Quaver"

            | ChartOrigin.Etterna(pack: string) ->
                match! DownloadEtternaPackTask.GetDownloadUrl(pack) with
                | Error reason -> return Error(sprintf "Error getting URL from EtternaOnline for '%s': %s" pack reason)
                | Ok url ->
                    let! result = DownloadEtternaPackTask(pack, url, library).Run(ctx)
                    return Ok(result)
        }

    let private httpclient = new HttpClient()

    // todo: ought to be internal due to its design
    let cdn_install
        (folder: string, hash: string, chart: Chart, song: Song, chart_db: ChartDatabase)
        : Async<Result<unit, string>> =
        async {
            let header = Archive.make_chart_header(chart, song)

            try
                let! response = httpclient.GetAsync("https://cdn.yavsrg.net/" + hash) |> Async.AwaitTask

                if not response.IsSuccessStatusCode then
                    return Error "Chart notes not found on server"
                else

                    use! stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask
                    use br = new BinaryReader(stream)

                    match Chart.ReadFromStreamHeadless(chart.Keys, br) with
                    | Error reason -> return Error(sprintf "Malformed chart: %s" reason)
                    | Ok chart_data ->

                    let actual_hash = chart_data.Hash()

                    if actual_hash <> hash then
                        return Error(sprintf "Hash mismatch: '%s' expected vs '%s' actual" hash actual_hash)
                    else

                    if not(chart_db.AssetStorage.Contains(chart.BackgroundHash)) then

                        let bg_path = Path.Combine(get_game_folder "Downloads", chart.BackgroundHash)

                        let! success =
                            WebServices.download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.BackgroundHash,
                                bg_path,
                                ignore
                            )

                        if not success then
                            failwithf "Error downloading background '%s'" chart.BackgroundHash

                        let actual_bg_hash = chart_db.AssetStorage.Add(bg_path)

                        if chart.BackgroundHash <> actual_bg_hash then
                            failwithf
                                "Downloaded background hash was '%s', expected '%s'"
                                actual_bg_hash
                                chart.BackgroundHash

                    if not(chart_db.AssetStorage.Contains(chart.AudioHash)) then

                        let audio_path = Path.Combine(get_game_folder "Downloads", chart.AudioHash)

                        let! success =
                            WebServices.download_file.RequestAsync(
                                "https://cdn.yavsrg.net/assets/" + chart.AudioHash,
                                audio_path,
                                ignore
                            )

                        if not success then
                            failwithf "Error downloading audio '%s'" chart.BackgroundHash

                        let actual_audio_hash = chart_db.AssetStorage.Add(audio_path)

                        if chart.AudioHash <> actual_audio_hash then
                            failwithf "Downloaded audio hash was '%s', expected '%s'" actual_audio_hash chart.AudioHash

                    chart_db.Import(
                        [
                            {
                                PackName = folder
                                LoadedFromPath = ""
                                Header = header
                                Chart = chart_data
                            }
                        ]
                    )
                    |> ignore

                    return Ok()
            with err ->
                // todo: specific exception type for asset errors, handle uncaught exceptions differently
                return Error err.Message
        }
