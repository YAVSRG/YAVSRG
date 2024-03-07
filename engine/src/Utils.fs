namespace Percyqaz.Flux

open System.IO
open System.Reflection
open System.Threading
open SixLabors.ImageSharp

module Utils =

    let get_resource_stream name =
        Assembly
            .GetCallingAssembly()
            .GetManifestResourceStream("Percyqaz.Flux.Resources." + name)

    let get_resource_text name =
        use s = get_resource_stream name
        use tr = new StreamReader(s)
        tr.ReadToEnd()

    type Bitmap = Image<PixelFormats.Rgba32>

    //do Configuration.Default.PreferContiguousImageBuffers <- true

    let lerp x a b : float32 = (b - a) * x + a

    let mutable internal UI_THREAD = -1

    let is_ui_thread () =
        UI_THREAD < 0 || Thread.CurrentThread.ManagedThreadId = UI_THREAD

    let inline require_ui_thread () = assert (is_ui_thread ())
