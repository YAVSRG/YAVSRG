module Imports

open System.IO
open Percyqaz.Common
open Prelude.Charts
open Prelude.Formats
open Prelude.Data.Library.Imports

let main () =

    Logging.Info "Running converter on all osu! songs ..."

    for song in Directory.EnumerateDirectories OSU_SONG_FOLDER do
        for file in Directory.EnumerateFiles song do
            if file.ToLower().EndsWith(".osu") then
                for converted in
                    convert_chart_file
                        {
                            Source = file
                            Config = ConversionOptions.Pack("osu!", None, LinkAssetFiles)
                        } do
                    match converted with
                    | Ok { Chart = chart } ->
                        match Chart.check chart with
                        | Error msg -> Logging.Error "%s" msg
                        | Ok chart -> ()
                    | Error skipped_reason -> printfn "%A" skipped_reason

    Logging.Info "Complete!"