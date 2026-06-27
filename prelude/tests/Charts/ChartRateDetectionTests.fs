namespace Prelude.Tests.Charts

open System.IO.Compression
open NUnit.Framework
open Prelude
open Prelude.Charts
open Prelude.Data.Library.Imports
open Prelude.Formats
open Prelude.Formats.Osu
open Prelude.Tests.Helpers

module ChartRateDetectionTests =

    [<TestCase("[4K] Test [1.1x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test[1.1x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test [1.10x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test[01.10]", 1.1f<rate>)>]
    [<TestCase("[4K] Test [x1.10x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test (x2.5)", 2.5f<rate>)>]
    [<TestCase("[4K] Test (2.5x)", 2.5f<rate>)>]
    [<TestCase("[4K] Test 1.1", 1.1f<rate>)>]
    [<TestCase("[4K] Test1.1x", 1.1f<rate>)>]
    [<TestCase("[4K] Test [1.1x] [1.2x]", 1.1f<rate>)>]
    [<TestCase("[4K] Test 0.8 0.9 1.0 1.1 1.2", 0.8f<rate>)>]
    [<TestCase("[4K] Test 0,8x 0.9x", 0.8f<rate>)>]
    [<TestCase("[4K] Test 3.0x", 3.0f<rate>)>]
    [<TestCase("[4K] Test 0.5x", 0.5f<rate>)>]
    [<TestCase("3.00x", 3.0f<rate>)>]
    [<TestCase("0.50x", 0.5f<rate>)>]
    let RateDetection_Regex_ExpectedValue (difficulty_name: string, expected_value: float32<rate>) =

        let result = detect_rate_mod difficulty_name
        match result with
        | Some value -> Assert.AreEqual(expected_value, value)
        | None -> Assert.Fail(sprintf "Expected %f to be detected rate" expected_value)

    [<TestCase("[4K] Test1.1")>]
    [<TestCase("[4K] Test [1.0x]")>]
    [<TestCase("[4K] Test [x1.0]")>]
    [<TestCase("[4K] Test 1.0")>]
    [<TestCase("[4K] Words1.1words")>]
    [<TestCase("[4K] 1.1words")>]
    [<TestCase("Words only")>]
    [<TestCase("3.01x")>]
    [<TestCase("3.10x")>]
    [<TestCase("x0.4")>]
    [<TestCase("0.49x")>]
    let RateDetection_Regex_ExpectNoRate (difficulty_name: string) =

        let result = detect_rate_mod difficulty_name
        match result with
        | Some value -> Assert.Fail(sprintf "%f" value)
        | None -> Assert.Pass()

    let FUZZ_REFERENCE_CHART = ChartFuzzer.Generate(7, 1234567)
    let simulate_normal_import (rate: Rate) : ImportChart =
        {
            Header = {
                Artist = "An artist"
                ArtistNative = None
                Title = "A title"
                TitleNative = None
                Creator = "Percyqaz"
                DiffName = sprintf "Example [%.2fx]" rate
                Subtitle = None
                Source = None
                Tags = []

                PreviewTime = 1000.0f<ms>
                BackgroundFile = ImportAsset.Missing
                AudioFile = ImportAsset.Missing

                Origins = Set.singleton (ChartOrigin.Osu {
                    Md5 = sprintf "h_%.2f" rate
                    BeatmapSetId = 0
                    BeatmapId = -1
                    SourceRate = rate
                    SourceOD = 8.0f
                    FirstNoteOffset = 10.0f<ms>
                })
            }
            LoadedFromPath = "C:/file.osu"
            PackName = "osu!"
            Chart = Chart.scale (1.0f / rate) FUZZ_REFERENCE_CHART
        }

    let simulate_mislabelled_rate (rate: Rate) : ImportChart =
        { simulate_normal_import rate with
            Chart = Chart.scale (1.0f / (rate - 0.2f<rate>)) FUZZ_REFERENCE_CHART
        }

    [<Test>]
    let RateFiltering_BasicCase() =

        let example_data =
            [
                Ok(simulate_normal_import 1.0f<rate>)
                Ok(simulate_normal_import 1.1f<rate>)
                Ok(simulate_normal_import 1.2f<rate>)
            ]

        let results = filter_rates example_data |> List.choose Result.toOption
        printfn "%A" results
        Assert.AreEqual(1, results.Length)
        let single_result = List.head results
        Assert.AreEqual(3, single_result.Header.Origins.Count)
        let origins =
            single_result.Header.Origins
            |> Seq.map (function ChartOrigin.Osu o -> o.Md5, o.SourceRate | _ -> failwith "")
            |> Array.ofSeq
        Assert.AreEqual(1.0f<rate>, snd origins.[0])
        Assert.AreEqual(1.1f<rate>, snd origins.[1])
        Assert.AreEqual(1.2f<rate>, snd origins.[2])

    [<Test>]
    let RateFiltering_OddOneOut() =

        let example_data =
            [
                Ok(simulate_normal_import 1.0f<rate>)
                Ok(simulate_normal_import 1.5f<rate>)
                Ok(simulate_mislabelled_rate 1.3f<rate>)
            ]

        let results = filter_rates example_data |> List.choose Result.toOption
        printfn "%A" results
        Assert.AreEqual(2, results.Length)

    [<Test>]
    let RateFiltering_RealCase() =

        let data = ZipFile.OpenRead("./Data/2089086 The Living Tombstone - My Ordinary Life (Speed up Ver.).osz")
        let song_rates =
            data.Entries
            |> Seq.map (fun (entry : ZipArchiveEntry) ->
                use stream = entry.Open()
                OsuParser.beatmap_from_stream stream
            )
            |> Seq.map (fun beatmap ->
                Osu_To_Interlude.convert
                    beatmap
                    {
                        Config = ConversionOptions.Pack("osu!", None, LinkAssetFiles)
                        Source = "./Data/2089086 The Living Tombstone - My Ordinary Life (Speed up Ver.).osz"
                    }
            )
            |> List.ofSeq

        let results = filter_rates song_rates |> List.choose Result.toOption
        printfn "%A" results
        Assert.AreEqual(1, results.Length)
        Assert.AreEqual(song_rates.Length, results.Head.Header.Origins.Count)

    [<Test>]
    let RateFiltering_RealCase2() =

        let data = ZipFile.OpenRead("./Data/989392 DJ Sharpnel - World Sound.osz")
        let song_rates =
            data.Entries
            |> Seq.map (fun (entry : ZipArchiveEntry) ->
                use stream = entry.Open()
                OsuParser.beatmap_from_stream stream
            )
            |> Seq.map (fun beatmap ->
                Osu_To_Interlude.convert
                    beatmap
                    {
                        Config = ConversionOptions.Pack("osu!", None, LinkAssetFiles)
                        Source = "./Data/989392 DJ Sharpnel - World Sound.osz"
                    }
            )
            |> List.ofSeq

        let results = filter_rates song_rates |> List.choose Result.toOption
        printfn "%A" results
        Assert.AreEqual(2, results.Length)