namespace Prelude.Data

open System.IO
open System.Net
open System.Net.Http
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Common

module WebServices =

    // todo: stop logging errors from these services. instead return appropriate result types and it's the caller's job to do the logging
    let download_string =

        let download_string_client =
            let handler = new HttpClientHandler()
            handler.AutomaticDecompression <- DecompressionMethods.Deflate ||| DecompressionMethods.GZip
            let client = new HttpClient(handler)
            client.DefaultRequestHeaders.Add("User-Agent", "Interlude")
            client.DefaultRequestHeaders.Add("Accept-Encoding", "gzip, deflate")
            client

        { new Async.Service<string, string option>() with
            override this.Handle(url: string) =
                async {
                    try
                        let! s = download_string_client.GetStringAsync(url) |> Async.AwaitTask
                        return Some s
                    with err ->
                        Logging.Error(sprintf "Could not reach %s" url, err)
                        return None
                }
        }


    let download_image =

        let download_image_client = new HttpClient()

        { new Async.Service<string, Bitmap option>() with
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

        { new Async.Service<string * string * (float32 -> unit), bool>() with
            override this.Handle((url: string, target: string, progress: float32 -> unit)) : Async<bool> =
                async {
                    let intermediate_file = target + ".download"
                    
                    try
                        use! response = download_file_client.GetAsync(url, HttpCompletionOption.ResponseHeadersRead) |> Async.AwaitTask
                        if not response.IsSuccessStatusCode then
                            Logging.Error(sprintf "Download from %s failed (%O)" url response.StatusCode)
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
                        Logging.Error("Failed to download file from " + url, err)
                        return false
                }
        }

    let download_json<'T> (url: string, callback: 'T option -> unit) =
        download_string.Request(
            url,
            function
            | Some s ->
                match JSON.FromString<'T> s with
                | Ok s -> callback (Some s)
                | Error err ->
                    Logging.Error("Failed to parse json data from " + url, err)
                    callback None
            | None -> callback None // appropriate error already logged by string service
        )

    let download_json_async<'T> (url: string) : Async<'T option> =
        async {
            match! download_string.RequestAsync(url) with
            | Some s ->
                match JSON.FromString<'T> s with
                | Ok s -> return (Some s)
                | Error err ->
                    Logging.Error("Failed to parse json data from " + url, err)
                    return None
            | None -> return None // appropriate error already logged by string service
        }

//let download_file_v2 =
//    let http_client = new HttpClient()
//    http_client.DefaultRequestHeaders.UserAgent.Add(Headers.ProductInfoHeaderValue("Interlude", "1.0"))
//    { new Async.Service<string * string * (float32 -> unit), bool>() with
//        override this.Handle((url: string, target: string, progress: float32 -> unit)) : Async<bool> =
//            task {
//                let intermediate_file = target + ".download"

//                let mutable bytes_written = 0L

//                Logging.Debug(sprintf "Requested download of %s" url)

//                let! response = http_client.SendAsync(new HttpRequestMessage(HttpMethod.Head, url))
//                if not response.IsSuccessStatusCode then return false else
//                let accepts_resume_bytes = response.Headers.AcceptRanges.Contains("bytes")
//                let total_length = response.Content.Headers.ContentLength
//                let total_length = if total_length.HasValue then Some total_length.Value else None

//                Logging.Debug(sprintf "Received size %A, accepts resuming: %b" total_length accepts_resume_bytes)

//                match!
//                    task {
//                        if (File.Exists intermediate_file && accepts_resume_bytes) then
//                            bytes_written <- FileInfo(intermediate_file).Length
//                            Logging.Debug(sprintf "Resuming partial download from %i bytes in" bytes_written)
//                            let request = new HttpRequestMessage(HttpMethod.Get, url)
//                            request.Headers.Range <- Headers.RangeHeaderValue(Nullable(bytes_written), Nullable())
//                            let! response = http_client.SendAsync(request)
//                            if not response.IsSuccessStatusCode then
//                                Logging.Error(sprintf "%A" response.ReasonPhrase)
//                                return None
//                            else
//                            let! stream = response.Content.ReadAsStreamAsync()
//                            return Some stream
//                        else
//                            Logging.Debug(sprintf "Starting download from beginning")
//                            if File.Exists(intermediate_file) then File.Delete(intermediate_file)
//                            let! response = http_client.SendAsync(new HttpRequestMessage(HttpMethod.Get, url))
//                            if not response.IsSuccessStatusCode then
//                                Logging.Error(sprintf "%A" response.ReasonPhrase)
//                                return None
//                            else
//                            let! stream = response.Content.ReadAsStreamAsync()
//                            return Some stream
//                    }
//                with
//                | None -> return false
//                | Some download_stream ->

//                Logging.Debug(sprintf "Download stream acquired, reading...")
//                let file_stream = new FileStream(intermediate_file, FileMode.Append)

//                let buffer : byte array = Array.zeroCreate 10000
//                let mutable bytes_read = download_stream.Read(buffer, 0, buffer.Length)

//                while bytes_read > 0 do
//                    file_stream.Write(buffer, 0, bytes_read)
//                    bytes_written <- bytes_written + int64 bytes_read
//                    match total_length with Some l -> progress (float32 bytes_written / float32 l) | None -> ()
//                    bytes_read <- download_stream.Read(buffer, 0, buffer.Length)

//                Logging.Debug(sprintf "Asserting that bytes read (%i) = total bytes (%A)" bytes_written total_length)

//                do! download_stream.DisposeAsync()
//                do! file_stream.FlushAsync()
//                do! file_stream.DisposeAsync()

//                if File.Exists(target) then File.Delete(target)
//                File.Move(intermediate_file, target)

//                return true
//            } |> Async.AwaitTask
//    }
