namespace Prelude.Data

open System
open System.IO
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing
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

    type BannerInfo = { BaseColor: Color; Emoji: string }

    let generate_banner (info: BannerInfo) : Bitmap =
        let banner =
            new Bitmap(1200, 480, PixelFormats.Rgba32(info.BaseColor.R, info.BaseColor.G, info.BaseColor.B))

        let emoji =
            get_cached_image.RequestAsync(
                sprintf "https://emoji.aranja.com/static/emoji-data/img-twitter-72/%s.png" info.Emoji
            )
            |> Async.RunSynchronously
            |> Option.get

        banner.Mutate(fun ctx ->
            let rows = 4
            let columns = 9

            let space_x = (1200 - emoji.Width - 40) / columns
            let space_y = (480 - emoji.Height - 40) / rows

            for y = 0 to rows do
                if y % 2 = 1 then
                    for x = 0 to columns + 1 do
                        ctx.DrawImage(emoji, new Point(20 - space_x / 2 + x * space_x, 20 + y * space_y), 1.0f)
                        |> ignore
                else
                    for x = 0 to columns do
                        ctx.DrawImage(emoji, new Point(20 + x * space_x, 20 + y * space_y), 1.0f)
                        |> ignore
        )

        banner