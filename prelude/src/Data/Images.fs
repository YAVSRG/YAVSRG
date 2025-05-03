namespace Prelude.Data

open System
open System.IO
open SixLabors.ImageSharp
open Percyqaz.Common
open Prelude

module ImageServices =

    open SixLabors.ImageSharp.Formats.Jpeg

    let save_image_jpg =
        { new Async.Queue<Bitmap * string, unit>() with
            override this.Handle((image, path)) =
                image.SaveAsJpegAsync(path, JpegEncoder(Quality = 90)) |> Async.AwaitTask
        }

    let save_image_png =
        { new Async.Queue<Bitmap * string, unit>() with
            override this.Handle((image, path)) =
                image.SaveAsPngAsync(path) |> Async.AwaitTask
        }

    let get_cached_image =
        { new Async.Queue<string, Bitmap option>() with
            override this.Handle(url: string) =
                async {
                    let cached_file_name =
                        let uri = new Uri(url)
                        let name = Uri.UnescapeDataString(uri.Segments[uri.Segments.Length - 1])
                        Path.Combine(get_game_folder ("Downloads"), name)

                    if File.Exists(cached_file_name) then
                        let stream = File.Open(cached_file_name, FileMode.Open)
                        return! Bitmap.from_stream_async true stream
                    else
                        match! WebServices.download_image.RequestAsync(url) with
                        | Some image ->
                            let clone = image.Clone()
                            save_image_png.Request((image, cached_file_name), image.Dispose)
                            return Some clone
                        | None -> return None
                }
        }