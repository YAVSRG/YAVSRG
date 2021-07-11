namespace Prelude.Test

open System.Threading
open System.Threading.Tasks
open Prelude.Common
open Prelude.Data.ChartManager

module Imports =
    
    let cache = new Cache()

    let main() =
        Logging.Info "Converting osu songs in background..."
        let t = BackgroundTask.Create TaskFlags.HIDDEN "Convert osu! songs" (cache.ConvertPackFolder osuSongFolder "osu!")
        while t.Status <> TaskStatus.RanToCompletion do
            Thread.Sleep(5000)
        cache.Save()
        Logging.Info "Conversion complete!"

    let getRandomChart() =
        // not yet a random chart
        Prelude.Charts.Interlude.Chart.fromFile @"C:\Users\percy\Desktop\Source\YAVSRG\Prelude\Prelude-Test\bin\Debug\netcoreapp3.1\Songs\\osu!\Hanatan_-_WAVE\\WAVE [Cut Ver.].yav"
        |> Option.get