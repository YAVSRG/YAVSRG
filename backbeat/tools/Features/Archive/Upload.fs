namespace Backbeat.Features.Archive

open System.IO
open Percyqaz.Common
open Prelude.Charts.Formats
open Prelude.Data.Charts.Caching
open Prelude.Backbeat.Archive
open Backbeat.Utils
open System.Collections.Concurrent
open Bytewizer.Backblaze.Client

[<AutoOpen>]
module Upload =

    let client = 
        let c = new BackblazeClient() in c.Connect(config.S3ApiKeyID, config.S3ApiKey) |> ignore; c

    let existingFiles = 
        let d = ConcurrentDictionary<string, bool>()
        task {
            let! files = client.Files.ListNamesAsync("c44023fe407a500583900717")
            for f in files.Response.Files do
                printfn "%s" f.FileName
                d.[f.FileName] <- true
        } |> Async.AwaitTask |> Async.RunSynchronously
        d
    
    let private upload_chart_internal (chart: Interlude.Chart) (chart_info: Chart) =
        Async.Parallel [
            task {
                let hash = Interlude.Chart.hash chart

                let exists = existingFiles.ContainsKey hash
                if not exists then

                    existingFiles.[hash] <- false

                    use ms = new MemoryStream()
                    use bw = new BinaryWriter(ms)

                    Interlude.Chart.writeHeadless chart bw
                    bw.Flush()

                    let! response = client.UploadAsync("c44023fe407a500583900717", hash, ms)
                    response.EnsureSuccessStatusCode() |> ignore
                    Logging.Info(sprintf "Uploaded %s (%.1fKB)" hash (float response.Response.ContentLength / 1000.0))

                    existingFiles.[hash] <- true
                else Logging.Info(sprintf "Skipping %s" hash)
            } |> Async.AwaitTask

            task {
                let audio_hash = "assets/" + chart_info.AudioFile

                let exists = existingFiles.ContainsKey audio_hash
                if not exists then
                    existingFiles.[audio_hash] <- false

                    let! response = client.UploadAsync("c44023fe407a500583900717", audio_hash, File.OpenRead (Cache.audio_path chart backbeat_cache).Value)
                    response.EnsureSuccessStatusCode() |> ignore
                    Logging.Info(sprintf "Uploaded %s (%.1fMB)" audio_hash (float response.Response.ContentLength / 1000000.0))

                    existingFiles.[audio_hash] <- true
                else Logging.Info(sprintf "Skipping %s" audio_hash)
            } |> Async.AwaitTask
                
            task {
                let bg_hash = "assets/" + chart_info.BackgroundFile
            
                let exists = existingFiles.ContainsKey bg_hash
                if not exists then
                    existingFiles.[bg_hash] <- false
            
                    let! response = client.UploadAsync("c44023fe407a500583900717", bg_hash, File.OpenRead (Cache.background_path chart backbeat_cache).Value)
                    response.EnsureSuccessStatusCode() |> ignore
                    Logging.Info(sprintf "Uploaded %s (%.1fMB)" bg_hash (float response.Response.ContentLength / 1000000.0))

                    existingFiles.[bg_hash] <- true
                else Logging.Info(sprintf "Skipping %s" bg_hash)
            } |> Async.AwaitTask
        ] |> Async.Ignore

    let private mass_upload_service =
        let sub_services =
            Array.init 24
                (fun i ->
                    { new Async.Service<(Interlude.Chart * Chart), unit>() with
                        override this.Handle((chart, chart_info)) = 
                            async {
                                do! upload_chart_internal chart chart_info
                                if i = 0 && this.Status = Async.ServiceStatus.Working then Logging.Debug(sprintf "Worker #%i completed its last upload" i)
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

    let upload_chart (chart: Interlude.Chart) (chart_info: Chart) = mass_upload_service.Request((chart, chart_info), ignore)