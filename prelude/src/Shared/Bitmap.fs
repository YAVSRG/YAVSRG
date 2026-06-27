namespace Prelude

open System.IO
open SixLabors.ImageSharp

type Bitmap = Image<PixelFormats.Rgba32>
module Bitmap =

    // todo: use to ensure correctness when upgrading to later versions of ImageSharp
    //do Configuration.Default.PreferContiguousImageBuffers <- true

    let from_stream (close_stream: bool) (stream: Stream) : Bitmap option =
        let img =
            try
                Some(Bitmap.Load<PixelFormats.Rgba32> stream)
            with
            | :? UnknownImageFormatException -> None
            | :? InvalidImageContentException -> None

        if close_stream then
            stream.Dispose()

        img

    let from_stream_async (close_stream: bool) (stream: Stream) : Async<Bitmap option> =
        async {
            match! Bitmap.LoadAsync<PixelFormats.Rgba32> stream |> Async.AwaitTask |> Async.Catch with
            | Choice1Of2 success ->
                if close_stream then
                    stream.Dispose()

                return Some success
            | Choice2Of2 exn ->
                if close_stream then
                    stream.Dispose()

                return
                    match exn with
                    | :? UnknownImageFormatException -> None
                    | :? InvalidImageContentException -> None
                    | _ -> raise exn
        }