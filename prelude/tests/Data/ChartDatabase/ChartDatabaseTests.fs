namespace Prelude.Tests.Data.ChartDatabase

open NUnit.Framework
open Percyqaz.Common
open Prelude.Charts
open Prelude.Formats.Osu
open Prelude.Formats.Conversions
open Prelude.Data.ChartDatabase
open Prelude.Tests.Helpers
open Prelude.Tests.Data

module ChartDatabaseTests =

    let TEST_OSU_FILE_PATH =
        "./test_data/osu_parity/Hachi - DONUT HOLE (Raveille) [Filling].osu"

    let TEST_OSU_FILE_HASH = Beatmap.HashFromFile(TEST_OSU_FILE_PATH) |> expect
    let TEST_OSU_FILE = Beatmap.TryReadFromFile(TEST_OSU_FILE_PATH) |> expect

    let TEST_IMPORT =
        (OsuConverter.convert
            TEST_OSU_FILE
            { Options = ConversionOptions.Pack("osu!", None, Link); OriginalPath = TEST_OSU_FILE_PATH }
         |> expect)

    [<Test>]
    let BasicRoundTrip () =
        let db, conn = InMemoryDatabase.Create()

        let chart_db =
            ChartDatabase.CreateLazyLoaded(db, VirtualAssetStorage("mock_data_path.dat"))

        let inline ensure_import_succeeded () : ChartMeta =
            chart_db.Import([ TEST_IMPORT ])

            let charts_in_database = DbCharts.fast_load(db) |> Array.ofSeq
            Assert.AreEqual(1, chart_db.Entries.Count)
            Assert.AreEqual(1, charts_in_database.Length)

            charts_in_database.[0]

        let inline ensure_delete_succeeded (chart_meta: ChartMeta) : unit =
            chart_db.Delete(chart_meta)

            let charts_in_database = DbCharts.fast_load(db) |> Array.ofSeq
            Assert.AreEqual(0, chart_db.Entries.Count)
            Assert.AreEqual(0, charts_in_database.Length)

        let imported_chart = ensure_import_succeeded()
        ensure_import_succeeded() |> ignore

        ensure_delete_succeeded(imported_chart)
        ensure_delete_succeeded(imported_chart)

        conn.Dispose()

    [<Test>]
    let DeleteBackground_DeletesSharedAssetReferences () =
        let db, conn = InMemoryDatabase.Create()

        let chart_db =
            ChartDatabase.CreateLazyLoaded(db, VirtualAssetStorage("mock_data_path.dat"))

        let background_hash = String.replicate 64 "A"

        let chart_with_background (chart: Chart) : ImportChart =
            { TEST_IMPORT with
                Chart = chart
                Header =
                    { TEST_IMPORT.Header with
                        BackgroundFile = ImportAsset.Download(background_hash)
                        AudioFile = ImportAsset.Missing
                    }
            }

        let first_chart = TEST_IMPORT.Chart
        let last_note = Array.last first_chart.Notes

        let second_chart =
            { first_chart with
                Notes = Array.append first_chart.Notes [| { last_note with Time = last_note.Time + 1.0f<ms> } |]
            }

        chart_db.Import([ chart_with_background first_chart; chart_with_background second_chart ])

        let imported_charts = DbCharts.fast_load db |> Array.ofSeq
        Assert.AreEqual(2, imported_charts.Length)

        chart_db.DeleteBackground(imported_charts.[0])

        let deleted_backgrounds =
            DbCharts.fast_load db |> Array.ofSeq |> Array.forall(fun meta -> meta.Background = AssetLocation.Deleted)

        Assert.True(deleted_backgrounds)

        chart_db.Import([ chart_with_background first_chart ])

        match chart_db.GetChartMeta(first_chart.Hash()) with
        | Some chart_meta -> Assert.AreEqual(AssetLocation.Deleted, chart_meta.Background)
        | None -> Assert.Fail("Chart disappeared after reimport")

        conn.Dispose()
