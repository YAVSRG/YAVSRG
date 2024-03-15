namespace Interlude.Web.Tests.Domain.Backbeat

open NUnit.Framework

open Prelude
open Interlude.Web.Server.Domain.Backbeat

module Songs =

    let private random = System.Random()

    let private random_text (length: int) =
        System.Guid.NewGuid().ToString("N").Substring(0, length)

    let generate_test_song () =
        {
            Artists = [ random_text 5; random_text 5 ]
            OtherArtists = [ random_text 5; random_text 5 ]
            Remixers = [ random_text 5; random_text 5 ]
            Title = random_text 10
            AlternativeTitles = [ random_text 10 ]
            Source = if random.Next 2 = 1 then Some(random_text 10) else None
            Tags = [ random_text 4; random_text 4; random_text 4 ]
        }

    let generate_test_chart () =
        {
            Creators = [ random_text 5; random_text 5 ]
            DifficultyName = random_text 10
            Subtitle = if random.Next 2 = 1 then Some(random_text 10) else None
            Tags = [ random_text 4; random_text 4; random_text 4 ]
            Duration = random.NextSingle() * 180000.0f<ms>
            PreviewTime = random.NextSingle() * 180000.0f<ms>
            Notecount = random.Next 10000
            Keys = random.Next(3, 11)
            BPM = random.NextSingle() * 1000.0f<ms / beat>, random.NextSingle() * 1000.0f<ms / beat>
            BackgroundHash = random_text 15
            AudioHash = random_text 15
            Sources = [ Stepmania(random.Next 10000); CommunityPack(random_text 5) ]
        }

    let TEST_SONG: Song =
        {
            Artists = [ "Camellia" ]
            OtherArtists = [ "Nanahira" ]
            Remixers = []
            Title = "EDM Jumpers"
            AlternativeTitles = []
            Source = Some "EDM Extreme (2015)"
            Tags = [ "DanceDanceRevolution" ]
        }

    let TEST_CHART_HASH =
        "3769432CBF00E56035035D1D0FAE74DA7313E56E443F32A1C457B0E781E42B6F"

    let TEST_CHART: Chart =
        {
            Creators = [ "Klaius" ]
            DifficultyName = "4K Challenge 26"
            Subtitle = Some "EDM Extreme (2015)"
            Tags = []
            Duration = 277862.28f<ms>
            PreviewTime = 92100.0f<ms>
            Notecount = 4382
            Keys = 4
            BPM = 413.7931f<ms / beat>, 413.7931f<ms / beat>
            BackgroundHash = "823436D6ED4350C2ED3ED6CC8502B69207F2670E60C6B5EAF6AB1A01744BD750"
            AudioHash = "D0AC559C92AE400A3A2C95EA0ED0E9034798634E1AF60ACAD99C6FA272631B89"
            Sources = [ Stepmania 13813 ]
        }

    [<Test>]
    let Chart_Song_RoundTrip () =
        let song_id = Songs.add_chart_song TEST_CHART_HASH TEST_CHART TEST_SONG

        match Songs.chart_by_id TEST_CHART_HASH with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual((song_id, TEST_CHART), result)
        | None -> Assert.Fail()

        match Songs.song_by_id song_id with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual(TEST_SONG, result)
        | None -> Assert.Fail()

        match Songs.song_by_chart_id TEST_CHART_HASH with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual((song_id, TEST_SONG), result)
        | None -> Assert.Fail()

        match Songs.chart_and_song_by_id TEST_CHART_HASH with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual((song_id, TEST_CHART, TEST_SONG), result)
        | None -> Assert.Fail()

    [<Test>]
    let Chart_Song_RoundTrip_FillerData () =
        Songs.add_chart_song "chartsongroundtripfillerdata1" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "chartsongroundtripfillerdata2" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "chartsongroundtripfillerdata3" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        let chart_id = "chartsongroundtripfillerdata"
        let chart = generate_test_chart ()
        let song = generate_test_song ()

        let song_id = Songs.add_chart_song chart_id chart song

        match Songs.chart_by_id chart_id with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual((song_id, chart), result)
        | None -> Assert.Fail()

        match Songs.song_by_id song_id with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual(song, result)
        | None -> Assert.Fail()

        match Songs.song_by_chart_id chart_id with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual((song_id, song), result)
        | None -> Assert.Fail()

        match Songs.chart_and_song_by_id chart_id with
        | Some result ->
            printfn "%A" result
            Assert.AreEqual((song_id, chart, song), result)
        | None -> Assert.Fail()

    [<Test>]
    let UpdateSong () =
        Songs.add_chart_song "updatesongfillerdata1" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatesongfillerdata2" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatesongfillerdata3" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        let chart_id = "updatesong"
        let chart = generate_test_chart ()
        let old_song = generate_test_song ()

        let song_id = Songs.add_chart_song chart_id chart old_song

        let new_song = generate_test_song ()

        Assert.True(Songs.update_song song_id new_song)
        Assert.AreEqual(Some new_song, Songs.song_by_id song_id)
        Assert.AreEqual(Some(song_id, new_song), Songs.song_by_chart_id "updatesong")

    [<Test>]
    let UpdateSong_DoesntExist () =
        Songs.add_chart_song "updatesongdoesntexist1" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatesongdoesntexist2" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatesongdoesntexist3" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        let new_song = generate_test_song ()
        Assert.False(Songs.update_song 9999L new_song)

    [<Test>]
    let UpdateChart () =
        Songs.add_chart_song "updatechartfillerdata1" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatechartfillerdata2" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatechartfillerdata3" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        let chart_id = "updatechart"
        let chart = generate_test_chart ()
        let old_song = generate_test_song ()

        let song_id = Songs.add_chart_song chart_id chart old_song

        let new_chart = generate_test_chart ()

        Assert.True(Songs.update_chart "updatechart" new_chart)
        Assert.AreEqual(Some(song_id, new_chart), Songs.chart_by_id "updatechart")

    [<Test>]
    let UpdateChart_DoesntExist () =
        Songs.add_chart_song "updatechartdoesntexist1" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatechartdoesntexist2" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        Songs.add_chart_song "updatechartdoesntexist3" (generate_test_chart ()) (generate_test_song ())
        |> ignore

        let new_chart = generate_test_chart ()
        Assert.False(Songs.update_chart "doesntexist" new_chart)

    [<Test>]
    let Search () =
        Songs.add_chart_song "search" TEST_CHART TEST_SONG |> printfn "%A"

        let camellia_results = Songs.search_songs "Camellia"
        printfn "%A" camellia_results
        Assert.Positive(camellia_results.Length)
        Assert.Positive((Songs.search_songs "{{''Nanahira}}").Length)
        Assert.Zero((Songs.search_songs "{{{{").Length)

    [<Test>]
    let Search_TrigramMatch () =
        Songs.add_chart_song "search_trigram" TEST_CHART TEST_SONG |> printfn "%A"
        Assert.Positive((Songs.search_songs "ana").Length)

    [<Test>]
    let UpdateChartSongId () =
        let chart_id_1 = "updatechartsongid1"
        let chart_1 = generate_test_chart ()
        let song_1 = generate_test_song ()

        let chart_id_2 = "updatechartsongid2"
        let chart_2 = generate_test_chart ()
        let song_2 = generate_test_song ()

        let song_id_1 = Songs.add_chart_song chart_id_1 chart_1 song_1
        let song_id_2 = Songs.add_chart_song chart_id_2 chart_2 song_2

        Assert.AreNotEqual(None, Songs.song_by_id song_id_1)
        Assert.True(Songs.update_chart_song_id chart_id_1 song_id_2)
        Assert.AreEqual(None, Songs.song_by_id song_id_1)
        Assert.AreEqual(Some(song_id_2, song_2), Songs.song_by_chart_id chart_id_2)

    [<Test>]
    let UpdateChartSongId_OldSongInUse () =
        let chart_id_1 = "updatechartsongidoldsonginuse1"
        let chart_1 = generate_test_chart ()
        let song_1 = generate_test_song ()

        let chart_id_2 = "updatechartsongidoldsonginuse2"
        let chart_2 = generate_test_chart ()
        let song_2 = generate_test_song ()

        let song_id_1 = Songs.add_chart_song chart_id_1 chart_1 song_1

        Songs.add_chart "updatechartsongidoldsonginuse3" (generate_test_chart ()) song_id_1

        let song_id_2 = Songs.add_chart_song chart_id_2 chart_2 song_2

        Assert.True(Songs.update_chart_song_id chart_id_1 song_id_2)
        Assert.AreNotEqual(None, Songs.song_by_id song_id_1)
        Assert.AreEqual(Some(song_id_2, song_2), Songs.song_by_chart_id chart_id_1)

    [<Test>]
    let UpdateChartSongId_DoesntExist () =
        let chart_id = "updatechartsongiddoesntexist"
        let chart = generate_test_chart ()
        let song = generate_test_song ()

        let song_id = Songs.add_chart_song chart_id chart song

        Assert.False(Songs.update_chart_song_id "doesntexist" song_id)
        Assert.False(Songs.update_chart_song_id "doesntexist" 99999L)

        try
            let result = Songs.update_chart_song_id chart_id 99999L
            Assert.Fail("Expected an exception", result)
        with e ->
            printfn "%O" e
            Assert.Pass()

    [<Test>]
    let MergeSongs () =
        let chart_id_1 = "mergesongs1"
        let chart_1 = generate_test_chart ()
        let song_1 = generate_test_song ()

        let chart_id_2 = "mergesongs2"
        let chart_2 = generate_test_chart ()
        let song_2 = generate_test_song ()

        let song_id_1 = Songs.add_chart_song chart_id_1 chart_1 song_1
        let song_id_2 = Songs.add_chart_song chart_id_2 chart_2 song_2

        Assert.AreNotEqual(None, Songs.song_by_id song_id_1)
        Assert.True(Songs.merge_songs song_id_1 song_id_2)
        Assert.AreEqual(None, Songs.song_by_id song_id_1)
        Assert.AreEqual(Some(song_id_2, song_2), Songs.song_by_chart_id chart_id_1)
        Assert.AreEqual(Some(song_id_2, song_2), Songs.song_by_chart_id chart_id_2)
