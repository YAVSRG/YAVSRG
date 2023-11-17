namespace Backbeat.Features.Archive

open System.IO
open Percyqaz.Common
open Prelude.Charts.Formats
open Prelude.Data.Charts.Caching
open Prelude.Backbeat.Archive
open Backbeat.Utils
open System.Collections.Concurrent
open Bytewizer.Backblaze.Client
open Bytewizer.Backblaze.Models

[<AutoOpen>]
module Upload =

    let client =
        let c = new BackblazeClient() in
        c.Connect(config.S3ApiKeyID, config.S3ApiKey) |> ignore
        c

    let existing_files =
        let d = ConcurrentDictionary<string, bool>()

        match Queue.get "uploaded" with
        | [] ->
            Logging.Debug "No uploaded list, fetching from bucket"

            task {
                // todo: stuff when file count goes over 10k
                let request = ListFileNamesRequest("c44023fe407a500583900717", MaxFileCount = 10000)
                let! files = client.Files.ListNamesAsync(request)

                for f in files.Response.Files do
                    d.[f.FileName] <- true
                    Queue.append "uploaded" f.FileName
            }
            |> Async.AwaitTask
            |> Async.RunSynchronously

            Logging.Debug(sprintf "Found %i files in bucket" d.Count)
        | xs ->
            for file in xs do
                d.[file] <- true

        d

    let add_existing_file (s: string) =
        existing_files.[s] <- true
        Queue.append "uploaded" s

    let private upload_chart_internal (chart: Interlude.Chart) (chart_info: Chart) =
        match Interlude.Chart.check chart with
        | Error msg -> failwithf "Invalid chart to upload: %s" msg
        | Ok() ->

        Async.Parallel
            [
                task {
                    let hash = Interlude.Chart.hash chart

                    let exists = existing_files.ContainsKey hash

                    if not exists then
                        use ms = new MemoryStream()
                        use bw = new BinaryWriter(ms)

                        Interlude.Chart.write_headless chart bw
                        bw.Flush()

                        let! response = client.UploadAsync("c44023fe407a500583900717", hash, ms)
                        response.EnsureSuccessStatusCode() |> ignore
                        Logging.Info(sprintf "Uploaded %s" chart.Header.Title)

                        add_existing_file hash
                }
                |> Async.AwaitTask

                task {
                    let audio_hash = "assets/" + chart_info.AudioFile

                    let exists = existing_files.ContainsKey audio_hash

                    if not exists then
                        let! response =
                            client.UploadAsync(
                                "c44023fe407a500583900717",
                                audio_hash,
                                File.OpenRead (Cache.audio_path chart backbeat_cache).Value
                            )

                        response.EnsureSuccessStatusCode() |> ignore

                        Logging.Info(
                            sprintf
                                "Uploaded %s (%.1fMB)"
                                audio_hash
                                (float response.Response.ContentLength / 1000000.0)
                        )

                        add_existing_file audio_hash
                }
                |> Async.AwaitTask

                task {
                    let bg_hash = "assets/" + chart_info.BackgroundFile

                    let exists = existing_files.ContainsKey bg_hash

                    if not exists then
                        let! response =
                            client.UploadAsync(
                                "c44023fe407a500583900717",
                                bg_hash,
                                File.OpenRead (Cache.background_path chart backbeat_cache).Value
                            )

                        response.EnsureSuccessStatusCode() |> ignore

                        Logging.Info(
                            sprintf "Uploaded %s (%.1fMB)" bg_hash (float response.Response.ContentLength / 1000000.0)
                        )

                        add_existing_file bg_hash
                }
                |> Async.AwaitTask
            ]
        |> Async.Ignore

    let private mass_upload_service =
        let sub_services =
            Array.init
                24
                (fun i ->
                    { new Async.Service<(Interlude.Chart * Chart), unit>() with
                        override this.Handle((chart, chart_info)) =
                            async {
                                do! upload_chart_internal chart chart_info

                                if i = 0 && this.Status = Async.ServiceStatus.Working then
                                    Logging.Debug(sprintf "Worker #%i completed its last upload" i)
                            }
                    }
                )

        let mutable i = 0

        { new Async.Service<(Interlude.Chart * Chart), unit>() with
            override this.Handle(r) =
                async {
                    sub_services.[i].Request(r, ignore)
                    i <- (i + 1) % sub_services.Length
                    return ()
                }
        }

    let upload_chart (chart: Interlude.Chart) (chart_info: Chart) =
        mass_upload_service.Request((chart, chart_info), ignore)
