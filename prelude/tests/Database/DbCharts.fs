namespace Prelude.Tests.Database

open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Calculator.Patterns
open Prelude.Data.Library

module DbCharts =

    let TEST_CHART : Chart =
        {
            Keys = 4
            Notes = [| { Time = 1000.0f<ms>; Data = Array.create 4 NoteType.NORMAL } |]
            BPM = [| { Time = 0.0f<ms>; Data = { Meter = 4<beat>; MsPerBeat = 500.0f<ms/beat> } } |]
            SV = [||]
        }

    let TEST_CHART_META : ChartMeta =
        {
            Hash = Chart.hash TEST_CHART
            Title = "EDM Jumpers ({E+H}DM Reboot)"
            TitleNative = None
            Artist = "Nanahira"
            ArtistNative = None
            DifficultyName = "Jump, crowd!"
            Subtitle = None
            Source = Some "“Guest Tracks” Summary & VIPs 01 (2017)"
            Creator = "Percyqaz"
            Tags = []
            Background = AssetPath.Missing
            Audio = AssetPath.Missing
            PreviewTime = 1000.0f<ms>
            Packs = Set.singleton "Singles"
            Origins = Set.singleton (ChartOrigin.Etterna "Bangers and Mash")
            Keys = 4
            Length = 0.0f<ms>
            BPM = 120
            DateAdded = Timestamp.now()
            Rating = 5.00f
            Patterns = PatternReport.Default
        }

    let TEST_CHART_META_ALT : ChartMeta =
        {
            Hash = Chart.hash TEST_CHART
            Title = "EDM Jumpers"
            TitleNative = Some "§"
            Artist = "Camellia ft. Nanahira"
            ArtistNative = Some "§"
            DifficultyName = "Jump, crowd!"
            Subtitle = Some "Are you ready?"
            Source = None
            Creator = "Klaius"
            Tags = ["dump"; "jumpstream"]
            Background = AssetPath.Absolute "C:/path/to/bg.png"
            Audio = AssetPath.Absolute "C:/path/to/audio.mp3"
            PreviewTime = 2000.0f<ms>
            Packs = Set.singleton "Nanahira Minipack"
            Origins = Set.singleton (ChartOrigin.Etterna "Nanahira Minipack")
            Keys = 4
            Length = 1.0f<ms>
            BPM = 121
            DateAdded = Timestamp.now() - 100L
            Rating = 10.00f
            Patterns = PatternReport.Default
        }

    let TEST_CHART_META_NAN : ChartMeta =
        { TEST_CHART_META_ALT with
            PreviewTime = System.Single.NegativeInfinity * 1.0f<ms>
            Length = System.Single.NaN * 1.0f<ms>
            Rating = System.Single.PositiveInfinity
        }

    let TEST_CHART_META_NAN_FIXED : ChartMeta =
        { TEST_CHART_META_ALT with
            PreviewTime = 0.0f<ms>
            Length = 0.0f<ms>
            Rating = 0.0f
        }

    [<Test>]
    let Get_Meta_DoesntExist () =
        let db, conn = in_memory ()

        let result = DbCharts.get_meta "doesntexist" db
        Assert.AreEqual(None, result)

        conn.Dispose()

    [<Test>]
    let RoundTrip_Meta() =
        let db, conn = in_memory ()

        DbCharts.delete TEST_CHART_META.Hash db |> ignore

        DbCharts.save TEST_CHART_META TEST_CHART db
        let result = DbCharts.get_meta TEST_CHART_META.Hash db
        Assert.AreEqual(Some TEST_CHART_META, result)

        Assert.AreEqual(None, DbCharts.get_meta "doesntexist" db)

    [<Test>]
    let RoundTrip_Meta_With_NaN() =
        let db, conn = in_memory ()

        DbCharts.delete TEST_CHART_META.Hash db |> ignore

        DbCharts.save TEST_CHART_META_NAN TEST_CHART db
        let result = DbCharts.get_meta TEST_CHART_META.Hash db
        Assert.AreEqual(Some TEST_CHART_META_NAN_FIXED, result)

    [<Test>]
    let RoundTrip_Chart() =
        let db, conn = in_memory ()

        DbCharts.save TEST_CHART_META TEST_CHART db
        let result = DbCharts.get_chart TEST_CHART_META.Hash db
        match result with
        | Ok chart ->
            Assert.AreEqual(TEST_CHART_META.Hash, Chart.hash chart)
        | Error reason -> Assert.Fail(reason)

    [<Test>]
    let Chart_DoesntExist () =
        let db, conn = in_memory ()

        let result = DbCharts.get_chart "doesntexist" db
        match result with
        | Ok _ -> Assert.Fail()
        | Error reason -> Assert.Pass(reason)

        conn.Dispose()

    [<Test>]
    let Chart_Delete () =
        let db, conn = in_memory ()

        DbCharts.save TEST_CHART_META TEST_CHART db
        let result = DbCharts.get_meta TEST_CHART_META.Hash db

        Assert.True(result.IsSome)

        Assert.True(DbCharts.delete TEST_CHART_META.Hash db)

        Assert.AreEqual(None, DbCharts.get_meta TEST_CHART_META.Hash db)

        Assert.False(DbCharts.delete TEST_CHART_META.Hash db)

        conn.Dispose()

    [<Test>]
    let Chart_Batch_Delete () =
        let db, conn = in_memory ()

        DbCharts.save TEST_CHART_META TEST_CHART db
        let result = DbCharts.get_meta TEST_CHART_META.Hash db

        Assert.True(result.IsSome)

        Assert.AreEqual(1, DbCharts.delete_batch [TEST_CHART_META.Hash] db)

        Assert.AreEqual(None, DbCharts.get_meta TEST_CHART_META.Hash db)

        Assert.AreEqual(0, DbCharts.delete_batch [TEST_CHART_META.Hash] db)

        conn.Dispose()

    [<Test>]
    let RoundTrip_Chart_Overwriting() =
        let db, conn = in_memory ()

        DbCharts.delete TEST_CHART_META.Hash db |> ignore

        DbCharts.save TEST_CHART_META TEST_CHART db
        Assert.AreEqual(Some TEST_CHART_META, DbCharts.get_meta TEST_CHART_META.Hash db)

        DbCharts.save TEST_CHART_META_ALT TEST_CHART db
        let with_merged_data =
            { TEST_CHART_META_ALT with
                Packs = Set.union TEST_CHART_META.Packs TEST_CHART_META_ALT.Packs
                Origins = Set.union TEST_CHART_META.Origins TEST_CHART_META_ALT.Origins
            }
        Assert.AreEqual(Some with_merged_data, DbCharts.get_meta TEST_CHART_META.Hash db)

        conn.Dispose()

    [<Test>]
    let RoundTrip_Chart_Overwriting_EmptyOrigins() =
        let db, conn = in_memory ()

        DbCharts.delete TEST_CHART_META.Hash db |> ignore

        DbCharts.save { TEST_CHART_META with Origins = Set.empty } TEST_CHART db

        DbCharts.save { TEST_CHART_META_ALT with Origins = Set.empty } TEST_CHART db
        let with_merged_data =
            { TEST_CHART_META_ALT with
                Packs = Set.union TEST_CHART_META.Packs TEST_CHART_META_ALT.Packs
                Origins = Set.empty
            }
        Assert.AreEqual(Some with_merged_data, DbCharts.get_meta TEST_CHART_META.Hash db)

        conn.Dispose()