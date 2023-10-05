namespace Prelude.Data

open System
open System.IO
open System.Net
open System.Net.Http
open System.Threading.Tasks
open System.ComponentModel
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude.Common

module WebServices =

    let private download_string_client =
        let w = new HttpClient()
        w.DefaultRequestHeaders.Add("User-Agent", "Interlude"); w
    let download_string = 
        { new Async.Service<string, string option>() with
            override this.Handle(url: string) = async {
                try
                    let! s = download_string_client.GetStringAsync(url) |> Async.AwaitTask
                    return Some s
                with err -> 
                    Logging.Error(sprintf "Could not reach %s" url, err)
                    return None
            }
        }
        
    let private download_image_client = new HttpClient()
    let download_image =
        { new Async.Service<string, Bitmap>() with
            override this.Handle(url: string) =
                async {
                    use! stream = Async.AwaitTask (download_image_client.GetStreamAsync url)
                    use! img = Async.AwaitTask (Bitmap.LoadAsync stream)
                    return img.CloneAs<PixelFormats.Rgba32>()
                }
        }

    let download_file =
        { new Async.Service<string * string * (float32 -> unit), bool>() with
            override this.Handle((url: string, target: string, progress: float32 -> unit)) : Async<bool> =
                async {
                    let intermediate_file = target + ".download"
                    use client = new WebClient()
                    client.Headers.Add("User-Agent", "Interlude")
                    let tcs = new TaskCompletionSource<unit>(url)
                    let completed =
                        new AsyncCompletedEventHandler(fun cs ce ->
                            if ce.UserState = (tcs :> obj) then
                                if ce.Error <> null then tcs.TrySetException ce.Error |> ignore
                                elif ce.Cancelled then tcs.TrySetCanceled() |> ignore
                                else tcs.TrySetResult () |> ignore)
                    let prog = new DownloadProgressChangedEventHandler(fun _ e -> progress(float32 e.ProgressPercentage / 100.0f))
                    client.DownloadFileCompleted.AddHandler completed
                    client.DownloadProgressChanged.AddHandler prog

                    try 
                        if File.Exists intermediate_file then File.Delete intermediate_file
                        client.DownloadFileAsync(new Uri(url), intermediate_file, tcs)
                        do! tcs.Task |> Async.AwaitTask

                        if isNull tcs.Task.Exception then 
                            if File.Exists target then File.Delete target
                            File.Move(intermediate_file, target)
                            return true
                        else
                            Logging.Error("Failed to download file from " + url, tcs.Task.Exception)
                            return false

                    with err ->
                        Logging.Error("Failed to download file from " + url, err)
                        return false
                }
        }

    let download_json<'T> (url: string, callback: 'T option -> unit) =
        download_string.Request(url,
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

// todo: place in IO services file + have zip extractor/creator service
module ImageServices =

    open SixLabors.ImageSharp.Formats.Png
    
    let save_image =
        { new Async.Service<Bitmap * string, unit>() with
            override this.Handle((image, path)) = image.SaveAsPngAsync(path, PngEncoder(ColorType = PngColorType.Palette)) |> Async.AwaitTask
        }

    let get_cached_image =
        { new Async.Service<string, Bitmap>() with
            override this.Handle(url: string) =
                async {
                    let cachedFileName =
                        let uri = new Uri(url)
                        let name = Uri.UnescapeDataString(uri.Segments[uri.Segments.Length - 1])
                        Path.Combine(getDataPath("Downloads"), name)

                    if File.Exists(cachedFileName) then
                        return! Image.LoadAsync<PixelFormats.Rgba32>(cachedFileName) |> Async.AwaitTask
                    else
                        let! image = WebServices.download_image.RequestAsync(url)
                        save_image.Request((image, cachedFileName), ignore)
                        return image
                }
        }