module Imports

open System.IO
open Percyqaz.Common
open Prelude.Charts
open Prelude.Formats.Conversions
open Prelude.Data.Imports

let main () =

    Logging.Info "Running converter on all osu! songs ..."

    for song in Directory.EnumerateDirectories MountedChartSource.DEFAULT_OSU_SONGS_LOCATION do
        for file in Directory.EnumerateFiles song do
            if file.ToLower().EndsWith(".osu") then
                for converted in
                    FileSystemImports.convert_chart_file
                        {
                            OriginalPath = file
                            Options = ConversionOptions.Pack("osu!", None, Link)
                        } do
                    match converted with
                    | Ok { Chart = chart } ->
                        match chart.CheckForErrors() with
                        | Error msg -> Logging.Error "%s" msg
                        | Ok _ -> ()
                    | Error skipped_reason -> printfn "%A" skipped_reason

    Logging.Info "Complete!"