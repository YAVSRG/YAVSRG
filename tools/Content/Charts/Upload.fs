namespace YAVSRG.CLI.Features.Backbeat

open System.IO
open System.Net.Http
open Percyqaz.Common
open Prelude.Charts
open Prelude.Data.Library.Caching
open Prelude
open Bytewizer.Backblaze.Client

type BackbeatChart = Backbeat.Archive.Chart
type Chart = Charts.Chart

module Upload =

    let private BUCKET_ID = "c44023fe407a500583900717"

    let private backblaze_client =
        let c = new BackblazeClient() in
        c.Connect(backbeat_config.S3ApiKeyID, backbeat_config.S3ApiKey) |> ignore
        c

    let private cdn_httpclient = new HttpClient()
    let check_cdn_file_exists(file_name: string) =
        let req = new HttpRequestMessage(HttpMethod.Head, "https://cdn.yavsrg.net/" + file_name)
        let reply = cdn_httpclient.Send(req)
        reply.IsSuccessStatusCode

    let private upload_chart_internal (chart: Chart) : Async<Result<unit, string>> =
        match Chart.check chart with
        | Error msg -> async { return Error (sprintf "Chart is invalid: %s" msg) }
        | Ok() ->

        match chart.Header.BackgroundFile, chart.Header.AudioFile with
        | Asset background_hash, Asset audio_hash ->
            let upload_notes =
                task {
                        let file_name = Chart.hash chart

                        let exists = check_cdn_file_exists file_name

                        if not exists then
                            use ms = new MemoryStream()
                            use bw = new BinaryWriter(ms)

                            Chart.write_headless chart bw
                            bw.Flush()

                            let! response = backblaze_client.UploadAsync(BUCKET_ID, file_name, ms)

                            response.EnsureSuccessStatusCode() |> ignore
                    }
            let upload_audio =
                task {
                        let file_name = "assets/" + audio_hash

                        let exists = check_cdn_file_exists file_name

                        if not exists then
                            let! response =
                                backblaze_client.UploadAsync(
                                    BUCKET_ID,
                                    file_name,
                                    File.OpenRead (Cache.audio_path chart interlude_chart_cache).Value
                                )

                            response.EnsureSuccessStatusCode() |> ignore

                    }
            let upload_bg =
                task {
                        let file_name = "assets/" + background_hash

                        let exists = check_cdn_file_exists file_name

                        if not exists then
                            let! response =
                                backblaze_client.UploadAsync(
                                    BUCKET_ID,
                                    file_name,
                                    File.OpenRead (Cache.background_path chart interlude_chart_cache).Value
                                )

                            response.EnsureSuccessStatusCode() |> ignore
                    }

            async {
                match! 
                    Async.Parallel
                        [
                            upload_notes |> Async.AwaitTask
                            upload_audio |> Async.AwaitTask
                            upload_bg |> Async.AwaitTask
                        ]
                   |> Async.Catch
                with
                | Choice1Of2 ok -> return Ok ()
                | Choice2Of2 exn -> return Error exn.Message
            }
        | _ ->  async { return Error "Chart is not part of cache/not using hashed assets mode" }

    let UPLOAD_POOL_CONCURRENCY = 10
    let private mass_upload_pooler =
        let sub_services =
            Array.init
                UPLOAD_POOL_CONCURRENCY
                (fun i ->
                    { new Async.Service<Chart, Result<unit, string>>() with
                        override this.Handle(chart) =
                            async {
                                return! upload_chart_internal chart
                            }
                    }
                )

        let mutable i = 0

        { new Async.Service<Chart, Result<unit, string>>() with
            override this.Handle(r) =
                async {
                    let! result = sub_services.[i].RequestAsync(r)
                    i <- (i + 1) % sub_services.Length
                    return result
                }
        }

    /// A sequence of these should be called using Async.Parallel (... sequence ...) UPLOAD_POOL_CONCURRENCY
    let upload_chart_to_cdn (chart: Chart) : Async<Result<unit, string>> = mass_upload_pooler.RequestAsync(chart)

    let create_backbeat_data (chart: Chart) : Result<BackbeatChart, string> =
        if chart.Header.AudioFile = Missing then
            Error "Missing audio file"
        elif chart.Header.BackgroundFile = Missing then
            Error "Missing background image"
        else

        match chart.Header.AudioFile, chart.Header.BackgroundFile with
        | Asset audio_hash, Asset background_hash ->
            let duration = chart.LastNote - chart.FirstNote
            if duration < 30000.0f<ms> then
                Error "Chart is too short"
            else

            Ok {
                Creators = [ chart.Header.Creator ]
                Keys = chart.Keys
                DifficultyName = chart.Header.DiffName
                Subtitle = chart.Header.Subtitle
                Tags = chart.Header.Tags
                Duration = duration
                Notecount =
                    let mutable count = 0

                    for row in chart.Notes do
                        for k = 0 to chart.Keys - 1 do
                            if row.Data.[k] = NoteType.NORMAL || row.Data.[k] = NoteType.HOLDHEAD then
                                count <- count + 1

                    count
                BPM = Chart.find_min_max_bpm chart
                Sources =
                    match chart.Header.ChartSource with
                    | Osu(-1, 0) -> []
                    | Osu(set, id) -> [  Backbeat.Archive.ChartSource.Osu {| BeatmapSetId = set; BeatmapId = id |} ]
                    | Stepmania(id) -> [  Backbeat.Archive.ChartSource.Stepmania id ]
                    | Unknown -> []
                PreviewTime = chart.Header.PreviewTime
                BackgroundHash = background_hash
                AudioHash = audio_hash
            }

        | _ -> Error "Chart should be cached/use assets format"