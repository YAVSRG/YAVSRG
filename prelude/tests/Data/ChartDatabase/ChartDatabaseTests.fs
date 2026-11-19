namespace Prelude.Tests.Data.ChartDatabase

open System.Linq
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
        OsuConverter.convert
            TEST_OSU_FILE
            { Options = ConversionOptions.Pack("osu!", None, Link); OriginalPath = TEST_OSU_FILE_PATH }
        |> expect

    [<Test>]
    let BasicRoundTrip () =
        let db, conn = InMemoryDatabase.Create()

        let chart_db =
            ChartDatabase.CreateLazyLoaded(db, VirtualAssetStorage("mock_data_path.dat"))

        let inline ensure_import_succeeded () : ChartMeta =
            chart_db.Import([ TEST_IMPORT ]) |> ignore

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
    let DeleteBackground_PreservedAfterReimport () =
        let db, conn = InMemoryDatabase.CreateIsolated()

        let chart_db =
            ChartDatabase.CreateFullyLoaded(db, VirtualAssetStorage("mock_data_path.dat"))

        // First import: Background hash present, audio not
        chart_db.Import(
            [
                { TEST_IMPORT with
                    Header.BackgroundFile = ImportAsset.Download("BACKGROUND")
                    Header.AudioFile = ImportAsset.Missing
                }
            ]
        )
        |> ignore

        Assert.AreEqual(1, chart_db.Entries.Count)
        let entry = chart_db.Entries.First()
        Assert.True(entry.Background.IsHash)
        Assert.True(entry.Audio.IsMissing)

        // Delete: Audio missing, background deleted
        chart_db.DeleteBackground(entry)

        Assert.AreEqual(1, chart_db.Entries.Count)
        let entry = chart_db.Entries.First()
        Assert.True(entry.Background.IsDeleted)
        Assert.True(entry.Audio.IsMissing)

        // Reimport: Background and audio both present; Background stays deleted; Audio accepts new non-missing asset
        chart_db.Import(
            [
                { TEST_IMPORT with
                    Header.BackgroundFile = ImportAsset.Download("BACKGROUND")
                    Header.AudioFile = ImportAsset.Download("AUDIO")
                }
            ]
        )
        |> ignore

        Assert.AreEqual(1, chart_db.Entries.Count)
        let entry = chart_db.Entries.First()
        Assert.True(entry.Background.IsDeleted)
        Assert.True(entry.Audio.IsHash)

        conn.Dispose()
