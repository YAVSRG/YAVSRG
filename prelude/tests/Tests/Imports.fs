namespace Prelude.Test

open System.IO
open Percyqaz.Common
open Prelude.Charts
open Prelude.Charts.Conversions
open Prelude.Data.Charts

module Imports =

    let main() =

        Logging.Info "Running converter on all osu! songs ..."
        for song in Directory.EnumerateDirectories Library.Imports.OSU_SONG_FOLDER do
            for file in Directory.EnumerateFiles song do
                if file.ToLower().EndsWith(".osu") then
                    for chart in convert_chart_file { Source = file; Config = ConversionOptions.Default } do
                        match Chart.check chart with Error msg -> Logging.Error msg | Ok() -> ()
        Logging.Info "Complete!"