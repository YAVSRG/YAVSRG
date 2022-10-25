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
        { new Async.Service<string, string>() with
            override this.Handle(url: string) =
                download_string_client.GetStringAsync(url) |> Async.AwaitTask
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
                        client.DownloadFileAsync(new Uri(url), target, tcs)
                        do! tcs.Task |> Async.AwaitTask

                        if isNull tcs.Task.Exception then return true
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
            fun s ->
                match JSON.FromString<'T> s with
                | Ok s -> callback (Some s)
                | Error err -> 
                    Logging.Error("Failed to parse json data from " + url, err)
                    callback None
        )
    
    let download_json_async<'T> (url: string) : Async<'T option> =
        async {
            let! s = download_string.RequestAsync(url)
            match JSON.FromString<'T> s with
            | Ok s -> return (Some s)
            | Error err ->
                Logging.Error("Failed to parse json data from " + url, err)
                return None
        }

// todo: place in IO services file + have zip extractor/creator service
module ImageServices =
    
    let save_image =
        { new Async.Service<Bitmap * string, unit>() with
            override this.Handle((image, path)) = image.SaveAsPngAsync(path) |> Async.AwaitTask
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