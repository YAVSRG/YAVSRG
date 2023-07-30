namespace Backbeat.Features.Archive

open System.IO
open Percyqaz.Common
open Prelude.Charts.Formats
open Prelude.Data.Charts.Caching
open Prelude.Backbeat.Archive
open Backbeat.Utils
open Bytewizer.Backblaze.Client

[<AutoOpen>]
module Upload =

    let client = 
        let c = new BackblazeClient() in c.Connect(config.S3ApiKeyID, config.S3ApiKey) |> ignore; c
    
    let upload_chart (chart: Interlude.Chart) (chart_info: Chart) =

        task {


            let hash = Interlude.Chart.hash chart

            let! exists = client.DownloadAsync("yavsrg-backbeat", hash, null)
            if not exists.IsSuccessStatusCode then

                use ms = new MemoryStream()
                use bw = new BinaryWriter(ms)

                Interlude.Chart.writeHeadless chart bw
                bw.Flush()

                let! response = client.UploadAsync("c44023fe407a500583900717", hash, ms)
                response.EnsureSuccessStatusCode() |> ignore
                Logging.Info(sprintf "Uploaded %s (%.1fKB)" hash (float response.Response.ContentLength / 1000.0))
            else Logging.Info(sprintf "Skipping %s" hash)

            let audio_hash = "assets/" + chart_info.AudioFile

            let! exists = client.DownloadAsync("yavsrg-backbeat", audio_hash, null)
            if not exists.IsSuccessStatusCode then

                let! response = client.UploadAsync("c44023fe407a500583900717", audio_hash, File.OpenRead (Cache.audio_path chart backbeat_cache).Value)
                response.EnsureSuccessStatusCode() |> ignore
                Logging.Info(sprintf "Uploaded %s (%.1fMB)" audio_hash (float response.Response.ContentLength / 1000000.0))
            else Logging.Info(sprintf "Skipping %s" audio_hash)
                
            let bg_hash = "assets/" + chart_info.BackgroundFile
            
            let! exists = client.DownloadAsync("yavsrg-backbeat", bg_hash, null)
            if not exists.IsSuccessStatusCode then
            
                let! response = client.UploadAsync("c44023fe407a500583900717", bg_hash, File.OpenRead (Cache.background_path chart backbeat_cache).Value)
                response.EnsureSuccessStatusCode() |> ignore
                Logging.Info(sprintf "Uploaded %s (%.1fMB)" bg_hash (float response.Response.ContentLength / 1000000.0))
            else Logging.Info(sprintf "Skipping %s" bg_hash)
        }