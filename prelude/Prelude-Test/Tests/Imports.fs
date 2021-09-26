namespace Prelude.Test

open System
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.ChartFormats.Conversions
open Prelude.Data.Charts

module Imports =
    
    let mount = Library.Imports.MountedChartSource.Pack ("osu!", Library.Imports.osuSongFolder)

    let main() =

        //mount.LastImported <- DateTime.Today.AddDays(-1.0)
        Logging.Subscribe (fun (level, message, details) -> if details <> "" then printfn "%s" details)

        Logging.Info "Converting osu! songs ..."
        let t = BackgroundTask.Create TaskFlags.HIDDEN "Convert osu! songs" (Library.Imports.importMountedSource mount)
        t.Wait()
        Library.save()
        Logging.Info "Conversion complete!"

        Logging.Info "Checking osu! imports for defects ..."
        for id in Library.charts.Keys do
            match Chart.fromFile id with
            | Some chart -> Chart.check chart
            | None -> ()
        Logging.Info "Check complete!"

    let getRandomChart() =
        // not yet a random chart
        Prelude.ChartFormats.Interlude.Chart.fromFile @"C:\Users\percy\Desktop\Source\YAVSRG\Prelude\Prelude-Test\bin\Debug\netcoreapp3.1\Songs\\osu!\Hanatan_-_WAVE\\WAVE [Cut Ver.].yav"
        |> Option.get