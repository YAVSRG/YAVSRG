namespace Percyqaz.Flux

open System
open System.IO
open System.Reflection
open System.Threading
open SixLabors.ImageSharp

module Utils =

    let getResourceStream name =
        Assembly.GetCallingAssembly().GetManifestResourceStream("Percyqaz.Flux.Resources." + name)

    let getResourceText name =
        use s = getResourceStream name
        use tr = new StreamReader(s)
        tr.ReadToEnd()

    type Bitmap = Image<PixelFormats.Rgba32>
    module Bitmap =
        let load (stream: Stream) : Bitmap = Bitmap.Load<PixelFormats.Rgba32> stream
    type Color = Drawing.Color

    let lerp x a b : float32 = (b - a) * x + a

    let mutable internal UITHREAD = 0

    let isUiThread() = Thread.CurrentThread.ManagedThreadId = UITHREAD