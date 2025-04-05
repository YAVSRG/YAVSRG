namespace YAVSRG.CLI.Features.Backbeat

open System.IO
open System.Net.Http
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Data
open Prelude.Data.Library
open Prelude.Data.Library.Imports
open Interlude.Web.Shared
open Bytewizer.Backblaze.Client

type BackbeatChart = Backbeat.Archive.Chart
type BackbeatSong = Backbeat.Archive.Song
type Chart = Charts.Chart

module Upload =

    let UPLOAD_POOL_CONCURRENCY = 10
    let private BUCKET_ID = "c44023fe407a500583900717"

    //let sw = System.Diagnostics.Stopwatch.StartNew()
    //let tic () = sw.Elapsed.TotalMilliseconds
    //let toc (label: string) (start: float) = printfn "%s took %.0fms" label (tic() - start)

    let private backblaze_client =
        try
            let c = new BackblazeClient() in
            c.Connect(backbeat_config.S3ApiKeyID, backbeat_config.S3ApiKey) |> ignore
            c
        with err ->
            printfn "%O\nProbably missing the S3 credentials to upload (only Percyqaz has access to this currently)" err
            failwith "S3 credentials missing"

    let private cdn_httpclient = new HttpClient()
    let check_cdn_file_exists(file_name: string) =
        let req = new HttpRequestMessage(HttpMethod.Head, "https://cdn.yavsrg.net/" + file_name)
        let reply = cdn_httpclient.Send(req)
        reply.IsSuccessStatusCode

    let private upload_chart_to_cdn (chart_meta: ChartMeta) (chart: Chart) : Async<Result<string * int, string>> =
        match Chart.check chart with
        | Error msg -> async { return Error (sprintf "Chart is invalid: %s" msg) }
        | Ok chart ->

        let chart_hash = Chart.hash chart

        match chart_meta.Background, chart_meta.Audio with
        | AssetPath.Hash background_hash, AssetPath.Hash audio_hash ->
            let upload_notes =
                task {
                        let file_name = chart_hash

                        let exists = check_cdn_file_exists file_name

                        if not exists then
                            use ms = new MemoryStream()
                            use bw = new BinaryWriter(ms)

                            Chart.write_headless chart bw
                            bw.Flush()

                            let! response = backblaze_client.UploadAsync(BUCKET_ID, file_name, ms)

                            response.EnsureSuccessStatusCode() |> ignore

                            return 1
                        else return 0
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
                                    File.OpenRead (chart_meta.Audio.Path).Value
                                )

                            response.EnsureSuccessStatusCode() |> ignore
                            return 1
                        else return 0

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
                                    File.OpenRead (chart_meta.Background.Path).Value
                                )

                            response.EnsureSuccessStatusCode() |> ignore
                            return 1
                        else return 0
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
                | Choice1Of2 new_files -> return Ok (chart_hash, Array.sum new_files)
                | Choice2Of2 exn -> return Error exn.Message
            }
        | _ ->  async { return Error "Chart is not part of cache/not using hashed assets mode" }

    let create_backbeat_data (chart_meta: ChartMeta) (chart: Chart) : Result<BackbeatChart * BackbeatSong, string> =
        if chart_meta.Audio = AssetPath.Missing then
            Error "Missing audio file"
        elif chart_meta.Background = AssetPath.Missing then
            Error "Missing background image"
        else

        match chart_meta.Audio, chart_meta.Background with
        | AssetPath.Hash audio_hash, AssetPath.Hash background_hash ->
            if chart_meta.Length < 30000.0f<ms> then
                Error "Chart is too short"
            else

            if chart_meta.Origins |> Set.exists _.SuitableForUpload |> not then
                Error (sprintf "Chart has no valid sources (%A)" chart_meta.Origins)
            else

            Ok (
                {
                    Creators = [ chart_meta.Creator ]
                    Keys = chart_meta.Keys
                    DifficultyName = chart_meta.DifficultyName
                    Subtitle = chart_meta.Subtitle
                    Tags = chart_meta.Tags
                    Duration = chart_meta.Length
                    Notecount =
                        let mutable count = 0

                        for row in chart.Notes do
                            for k = 0 to chart.Keys - 1 do
                                if row.Data.[k] = NoteType.NORMAL || row.Data.[k] = NoteType.HOLDHEAD then
                                    count <- count + 1

                        count
                    BPM = (60000.0f<ms/beat> / float32 chart_meta.BPM, 60000.0f<ms/beat> / float32 chart_meta.BPM)
                    Origins = chart_meta.Origins |> Set.filter _.SuitableForUpload
                    PreviewTime = chart_meta.PreviewTime
                    BackgroundHash = background_hash
                    AudioHash = audio_hash
                },
                {
                    Artists = [ chart_meta.Artist.Trim() ]
                    OtherArtists = []
                    Remixers = []
                    Title = Metadata.prune_song_title chart_meta.Title
                    AlternativeTitles =
                        match chart_meta.TitleNative with
                        | Some x -> [ Metadata.prune_song_title x ]
                        | None -> []
                    Source = chart_meta.Source
                    Tags = Metadata.prune_tags chart_meta.Tags
                }
            )
        | _ -> Error "Chart should be cached/use assets format"

    let private upload_chart_to_backbeat (chart_hash: string, chart: BackbeatChart, song: BackbeatSong) : Async<Result<unit, string>> =
        async {
            let mutable result = Ok()
            do! Requests.Charts.Add.post_async(
                { ChartId = chart_hash; Chart = chart; Song = song },
                function
                | Some true -> ()
                | Some false -> result <- Error "Server replied with error"
                | None -> result <- Error "No reply from server or unauthenticated"
            )
            return result
        }

    let private upload_chart (chart_meta: ChartMeta) (chart: Chart) : Async<Result<unit, string>> =
        async {
            match create_backbeat_data chart_meta chart with
            | Error reason -> return Error reason
            | Ok (bb_chart, bb_song) ->

            match! upload_chart_to_cdn chart_meta chart with
            | Error reason -> return Error reason
            | Ok (hash, files_changed) ->

            if files_changed > 0 then
                Logging.Debug "Uploaded %i new files for '%s'" files_changed chart_meta.Title

            match! upload_chart_to_backbeat (hash, bb_chart, bb_song) with
            | Error reason -> return Error reason
            | Ok () -> return Ok ()
        }

    let has_folder (folder_name: string) =
        interlude_chart_db.Entries
        |> Seq.where (fun meta -> meta.Packs.Contains folder_name)
        |> Seq.map (fun meta -> meta.Origins |> Set.exists _.SuitableForUpload && match meta.Audio with AssetPath.Hash _ -> true | _ -> false)
        |> fun s -> not (Seq.isEmpty s) && Seq.forall id s

    let upload_folder (folder_name: string) =
        seq {
            for chart_meta in interlude_chart_db.Entries |> Seq.where (fun meta -> meta.Packs.Contains folder_name) do
                async {
                    match ChartDatabase.get_chart chart_meta.Hash interlude_chart_db with
                    | Ok chart ->
                        match! upload_chart chart_meta chart with
                        | Ok () -> Logging.Debug "Uploaded '%s'" chart_meta.Title
                        | Error reason -> Logging.Warn "Upload of '%s' failed: %s" chart_meta.Title reason
                    | Error reason -> Logging.Error "Loading '%s' from disk failed: %s" chart_meta.Title reason
                }
        }
        |> fun upload_tasks -> Async.Parallel(upload_tasks, UPLOAD_POOL_CONCURRENCY)
        |> Async.Ignore
        |> Async.RunSynchronously
        Logging.Info "Uploading '%s' complete!" folder_name

    let get_etterna_pack (pack_name: string) =
        OnlineImports.download_by_origin (ChartOrigin.Etterna pack_name, interlude_library.Charts, interlude_scores_db, TaskProgress.log_progress_bar pack_name)
        |> Async.RunSynchronously

    let etterna_pack_aio (pack_name: string) =
        if has_folder pack_name then
            Logging.Info "You already have '%s'" pack_name
            upload_folder pack_name
        else
            Logging.Info "'%s' is not installed or needs a re-download" pack_name
            match get_etterna_pack pack_name with
            | Error reason ->
                Logging.Error "Error installing '%s': %s" pack_name reason
            | Ok result ->
                Logging.Info "Installed '%s': %i succeeded and %i skipped" pack_name result.ConvertedCharts result.SkippedCharts.Length
                upload_folder pack_name

    let get_etterna_pack_list () =
        let on_file =
            File.ReadAllLines PACK_LIST_PATH
            |> Set.ofSeq
        let top_50 =
            match WebServices.download_json_async<_> "https://api.etternaonline.com/api/packs?page=1&limit=50&sort=-popularity" |> Async.RunSynchronously with
            | WebResult.Ok (response: {| data: {| name: string |} array |}) -> response.data |> Array.map _.name
            | WebResult.HttpError i -> [||]
            | WebResult.Exception err -> [||]
            |> Set.ofArray
        let new_list = Set.union on_file top_50
        File.WriteAllLines(PACK_LIST_PATH, new_list)
        new_list

    let bulk_etterna_pack_aio () =
        get_etterna_pack_list () |> Seq.iter etterna_pack_aio