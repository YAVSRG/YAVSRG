namespace Percyqaz.Flux

open System.IO
open System.Reflection
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

    // todo: move to percyqaz.common
    let lerp x a b : float32 = (b - a) * x + a
