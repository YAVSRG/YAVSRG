module OsuReplayReader

open System
open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Conversions
open Prelude.Charts.Formats.osu
open Prelude.Data.OsuClientInterop
open Prelude.Data.Library
open Prelude.Gameplay
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.RulesetsV2
open Prelude.Gameplay.ScoringV2
open SevenZip.Compression

let compare_interlude_implementation_to_osu (chart: Chart, header: ChartImportHeader, replay: ReplayData, rate: Rate, chart_od: float, score_data: OsuScoreDatabase_Score) =

    let ruleset_mod = 
        if score_data.ModsUsed &&& Mods.Easy <> Mods.None then OsuMania.Easy
        elif score_data.ModsUsed &&& Mods.HardRock <> Mods.None then OsuMania.HardRock
        else OsuMania.NoMod

    let metric =
        ScoreProcessor.run (OsuMania.create (float32 chart_od) ruleset_mod) chart.Keys (StoredReplayProvider(replay)) chart.Notes rate

    let sum =
        300.0
        * float (
            score_data.CountGeki
            + score_data.Count300
            + score_data.CountKatu
            + score_data.Count100
            + score_data.Count50
            + score_data.CountMiss
        )

    let acc =
        300.0 * float (score_data.CountGeki + score_data.Count300)
        + 200.0 * float score_data.CountKatu
        + 100.0 * float score_data.Count100
        + 50.0 * float score_data.Count50
        |> fun total -> total / sum

    printfn "Score on %s [%s] -- od%.1f +%A\n----" header.Title header.DiffName chart_od ruleset_mod

    printfn
        "Interlude says: %.2f%% accuracy\n%04i|%04i|%04i|%04i|%04i|%04i\n%ix\n----"
        (metric.Accuracy * 100.0)
        metric.JudgementCounts.[0]
        metric.JudgementCounts.[1]
        metric.JudgementCounts.[2]
        metric.JudgementCounts.[3]
        metric.JudgementCounts.[4]
        metric.JudgementCounts.[5]
        metric.BestCombo

    printfn
        "osu! says: %.2f%% accuracy\n%04i|%04i|%04i|%04i|%04i|%04i\n%ix\n----"
        (acc * 100.0)
        score_data.CountGeki
        score_data.Count300
        score_data.CountKatu
        score_data.Count100
        score_data.Count50
        score_data.CountMiss
        score_data.MaxCombo

    Console.ReadLine() |> ignore

let read_scores () =

    Logging.Info "Reading osu database ..."

    use file = Path.Combine(Imports.OSU_SONG_FOLDER, "..", "scores.db") |> File.OpenRead

    use reader = new BinaryReader(file, Text.Encoding.UTF8)
    let scores = OsuScoreDatabase.Read(reader)

    use file = Path.Combine(Imports.OSU_SONG_FOLDER, "..", "osu!.db") |> File.OpenRead

    use reader = new BinaryReader(file, Text.Encoding.UTF8)
    let main_db = OsuDatabase.Read(reader)

    for beatmap_data in main_db.Beatmaps |> Seq.filter (fun b -> b.Mode = 3uy) do
        let osu_file = Path.Combine(Imports.OSU_SONG_FOLDER, beatmap_data.FolderName, beatmap_data.Filename)

        let chart, od =
            try
                let beatmap =
                    Beatmap.FromFile osu_file
                    |> expect

                Osu_To_Interlude.convert
                    beatmap
                    {
                        Config = ConversionOptions.Default
                        Source = osu_file
                    }
                |> expect
                |> Some
                , beatmap.Difficulty.OverallDifficulty
            with _ ->
                None, 8.0

        if chart.IsNone then
            ()
        else

            match scores.Beatmaps |> Seq.tryFind (fun b -> b.Hash = beatmap_data.Hash) with
            | Some scores ->
                for score in scores.Scores do
                    let replay_file =
                        Path.Combine(
                            Imports.OSU_SONG_FOLDER,
                            "..",
                            "Data",
                            "r",
                            sprintf "%s-%i.osr" score.BeatmapHash score.Timestamp
                        )

                    use file = File.OpenRead replay_file
                    use br = new BinaryReader(file)
                    let replay_data = OsuScoreDatabase_Score.Read br

                    let input = new MemoryStream(replay_data.CompressedReplayBytes.Value)
                    let output = new MemoryStream()

                    let props = Array.zeroCreate 5
                    input.Read(props, 0, 5) |> ignore

                    let lengthBytes = Array.zeroCreate 8
                    input.Read(lengthBytes, 0, 8) |> ignore

                    let dec = LZMA.Decoder()
                    dec.SetDecoderProperties props

                    dec.Code(
                        input,
                        output,
                        replay_data.CompressedReplayBytes.Value.Length,
                        BitConverter.ToInt64(lengthBytes, 0),
                        null
                    )

                    output.Flush()
                    let string_data = output.ToArray() |> System.Text.Encoding.UTF8.GetString

                    let interlude_replay: ReplayData =
                        let mutable time = -chart.Value.Chart.FirstNote
                        let mutable last_state = 256us

                        seq {
                            for entry in string_data.Split(",", StringSplitOptions.RemoveEmptyEntries) do
                                let parts = entry.Split("|")

                                if parts.[0] <> "-12345" then
                                    time <- time + float32 parts.[0] * 1.0f<ms>
                                    let state = uint16 parts.[1]

                                    if state <> last_state then
                                        yield struct (time, uint16 parts.[1])
                                        last_state <- state
                        }
                        |> Array.ofSeq

                    match Mods.to_interlude_rate_and_mods replay_data.ModsUsed with
                    | Some(rate, _) -> compare_interlude_implementation_to_osu (chart.Value.Chart, chart.Value.Header, interlude_replay, rate, od, replay_data)
                    | None -> ()

            | None -> ()

    Console.ReadLine() |> ignore