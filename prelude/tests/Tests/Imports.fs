namespace Prelude.Test

open Percyqaz.Common
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Data.Charts

module Imports =
    
    let mount = Library.Imports.MountedChartSource.Pack ("osu!", Library.Imports.osuSongFolder)

    let main() =

        //mount.LastImported <- DateTime.Today.AddDays(-1.0)

        Reporter.func <- Reports.minor
        Logging.Info "Converting osu! songs ..."
        let t = BackgroundTask.Create TaskFlags.NONE "Convert osu! songs" (Library.Imports.importMountedSource mount)
        t.Wait()
        Library.save()
        Logging.Info "Conversion complete!"
        
        Reporter.func <- Reports.major
        Logging.Info "Checking osu! imports for defects ..."
        for id in Library.charts.Keys do
            match Chart.fromFile id with
            | Some chart -> Chart.check chart
            | None -> ()
        Logging.Info "Check complete!"

    let getRandomChart() =
        // not yet a random chart
        Chart.fromFile @"C:\Users\percy\Desktop\Source\YAVSRG\Interlude\bin\Debug\netcoreapp3.1\Songs\Compulsive Chordjack Collection 2\Day Dreamer (XingRen)\Dance_Single 23 [0].yav"
        |> Option.get