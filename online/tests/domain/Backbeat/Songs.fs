namespace Interlude.Web.Tests.Domain.Backbeat

open NUnit.Framework

open Percyqaz.Common
open Prelude
open Interlude.Web.Server.Domain.Backbeat

module Songs =

    let test_song : Song =
        {
            Artists = ["Camellia"]
            OtherArtists = ["Nanahira"]
            Remixers = []
            Title = "EDM Jumpers"
            AlternativeTitles = []
            Source = Some "EDM Extreme (2015)"
            Tags = ["DanceDanceRevolution"]
        }
    let test_chart_hash = "3769432CBF00E56035035D1D0FAE74DA7313E56E443F32A1C457B0E781E42B6F"
    let test_chart : Chart =
        {
            Creators = ["Klaius"]
            DifficultyName = "4K Challenge 26"
            Subtitle = Some "EDM Extreme (2015)"
            Tags = []
            Duration = 277862.28f<ms>
            PreviewTime = 92100.0f<ms>
            Notecount = 4382
            Keys = 4
            BPM = 413.7931f<ms/beat>, 413.7931f<ms/beat>
            BackgroundHash = "823436D6ED4350C2ED3ED6CC8502B69207F2670E60C6B5EAF6AB1A01744BD750"
            AudioHash = "D0AC559C92AE400A3A2C95EA0ED0E9034798634E1AF60ACAD99C6FA272631B89"
            Sources = [Stepmania 13813]
        }

    [<Test>]
    let Chart_Song_RoundTrip () =
        let song_id = Songs.add_chart_song test_chart_hash test_chart test_song

        match Songs.chart_by_id test_chart_hash with
        | Some result -> 
            printfn "%A" result
            Assert.AreEqual((song_id, test_chart), result)
        | None -> Assert.Fail()
        
        match Songs.song_by_id song_id with
        | Some result -> 
            printfn "%A" result
            Assert.AreEqual(test_song, result)
        | None -> Assert.Fail()
        
        match Songs.song_by_chart_id test_chart_hash with
        | Some result -> 
            printfn "%A" result
            Assert.AreEqual((song_id, test_song), result)
        | None -> Assert.Fail()
        
        match Songs.chart_and_song_by_id test_chart_hash with
        | Some result -> 
            printfn "%A" result
            Assert.AreEqual((song_id, test_chart, test_song), result)
        | None -> Assert.Fail()