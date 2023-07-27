namespace Backbeat.Features.Archive

open System.IO
open Percyqaz.Common
open Prelude.Charts.Formats
open Prelude.Backbeat.Archive
open Backbeat.Utils
open Bytewizer.Backblaze.Client

[<AutoOpen>]
module Upload =
    
    let upload_chart (chart: Interlude.Chart) (chart_info: Chart) =

        let client = new BackblazeClient()

        task {

            let _ = client.Connect(config.S3ApiKeyID, config.S3ApiKey)

            let hash = Interlude.Chart.hash chart

            let! exists = client.DownloadAsync("c44023fe407a500583900717", hash, null)
            if not exists.IsSuccessStatusCode then

                use ms = new MemoryStream()
                use bw = new BinaryWriter(ms)

                Interlude.Chart.writeHeadless chart bw
                bw.Flush()

                let! response = client.UploadAsync("c44023fe407a500583900717", hash, ms)
                response.EnsureSuccessStatusCode() |> ignore
                Logging.Info(sprintf "Uploaded chart file %s" hash)

            let audio_hash = chart_info.AudioFile

            let! exists = client.DownloadAsync("c44023fe407a500583900717", audio_hash, null)
            if not exists.IsSuccessStatusCode then

                let! response = client.UploadAsync("c44023fe407a500583900717", audio_hash, File.OpenRead chart.AudioPath)
                response.EnsureSuccessStatusCode() |> ignore
                Logging.Info(sprintf "Uploaded audio file %s" audio_hash)
                
            let bg_hash = chart_info.BackgroundFile
            
            let! exists = client.DownloadAsync("c44023fe407a500583900717", bg_hash, null)
            if not exists.IsSuccessStatusCode then
            
                let! response = client.UploadAsync("c44023fe407a500583900717", bg_hash, File.OpenRead chart.BackgroundPath)
                response.EnsureSuccessStatusCode() |> ignore
                Logging.Info(sprintf "Uploaded background file %s" bg_hash)

        }