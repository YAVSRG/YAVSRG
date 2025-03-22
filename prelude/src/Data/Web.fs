namespace Prelude.Data

open System.IO
open System.Net
open System.Net.Http
open Percyqaz.Common
open Prelude

[<RequireQualifiedAccess>]
type WebResult<'T> =
    | Ok of 'T
    | HttpError of int
    | Exception of exn

module WebServices =

    let download_string =

        let download_string_client =
            let handler = new HttpClientHandler()
            handler.AutomaticDecompression <- DecompressionMethods.Deflate ||| DecompressionMethods.GZip
            let client = new HttpClient(handler)
            client.DefaultRequestHeaders.Add("User-Agent", "Interlude")
            client.DefaultRequestHeaders.Add("Accept-Encoding", "gzip, deflate")
            client

        { new Async.Queue<string, WebResult<string>>() with
            override this.Handle(url: string) =
                async {
                    match! download_string_client.GetAsync(url) |> Async.AwaitTask |> Async.Catch with
                    | Choice1Of2 http_response ->
                        if http_response.StatusCode = HttpStatusCode.OK then
                            let! text = http_response.Content.ReadAsStringAsync() |> Async.AwaitTask
                            return WebResult.Ok text
                        else
                            return WebResult.HttpError (int http_response.StatusCode)
                    | Choice2Of2 err ->
                        return WebResult.Exception err
                }
        }

    let download_image =

        let download_image_client = new HttpClient()

        { new Async.Queue<string, Bitmap option>() with
            override this.Handle(url: string) =
                async {
                    match! Async.AwaitTask(download_image_client.GetStreamAsync url) |> Async.Catch with
                    | Choice1Of2 stream -> return! Bitmap.from_stream_async true stream
                    | Choice2Of2 exn -> return None
                }
        }

    let download_file =

        let download_file_client = new HttpClient()
        download_file_client.DefaultRequestHeaders.Add("User-Agent", "Interlude")

        { new Async.Queue<string * string * (float32 -> unit), bool>() with
            override this.Handle((url: string, target: string, progress: float32 -> unit)) : Async<bool> =
                async {
                    let intermediate_file = target + ".download"

                    try
                        use! response = download_file_client.GetAsync(url, HttpCompletionOption.ResponseHeadersRead) |> Async.AwaitTask
                        if not response.IsSuccessStatusCode then
                            Logging.Error "Download from %s failed (%O)" url response.StatusCode
                            return false
                        else

                        let total_bytes = response.Content.Headers.ContentLength.GetValueOrDefault -1L

                        use! content_stream = response.Content.ReadAsStreamAsync() |> Async.AwaitTask

                        let BUFFER_SIZE = 8192
                        let buffer : byte array = Array.zeroCreate BUFFER_SIZE
                        let mutable bytes_read = 0L
                        let mutable total_bytes_read = 0L

                        if File.Exists intermediate_file then
                            File.Delete intermediate_file

                        let file_stream = new FileStream(intermediate_file, FileMode.Create, FileAccess.Write, FileShare.None, bufferSize = 8192, useAsync = true)

                        let read() = async {
                            let! r = content_stream.ReadAsync(buffer, 0, buffer.Length) |> Async.AwaitTask
                            bytes_read <- r
                            return bytes_read > 0
                        }

                        while! read() do
                            do! file_stream.WriteAsync(buffer, 0, int bytes_read) |> Async.AwaitTask
                            total_bytes_read <- total_bytes_read + bytes_read

                            let percent_progress = if total_bytes > 0 then float32 total_bytes_read / float32 total_bytes else 0.0f
                            progress percent_progress

                        do! file_stream.FlushAsync() |> Async.AwaitTask
                        file_stream.Dispose()

                        if File.Exists target then
                            File.Delete target

                        File.Move(intermediate_file, target)
                        return true

                    with err ->
                        Logging.Error "Failed to download file from '%s': %O" url err
                        return false
                }
        }

    let download_json<'T> (url: string, callback: WebResult<'T> -> unit) =
        download_string.Request(
            url,
            function
            | WebResult.Ok s ->
                match JSON.FromString<'T> s with
                | Ok parsed -> callback (WebResult.Ok parsed)
                | Error err -> callback (WebResult.Exception err)
            | WebResult.HttpError code -> callback (WebResult.HttpError code)
            | WebResult.Exception err -> callback (WebResult.Exception err)
        )

    let download_json_async<'T> (url: string) : Async<WebResult<'T>> =
        async {
            match! download_string.RequestAsync(url) with
            | WebResult.Ok s ->
                match JSON.FromString<'T> s with
                | Ok parsed -> return WebResult.Ok parsed
                | Error err -> return WebResult.Exception err
            | WebResult.HttpError code -> return WebResult.HttpError code
            | WebResult.Exception err -> return WebResult.Exception err
        }