namespace Prelude.Test

open System
open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Conversions
open Prelude.Charts.Formats.osu
open Prelude.Data.``osu!``
open Prelude.Data.Library
open Prelude.Gameplay
open Prelude.Gameplay.Rulesets
open SevenZip.Compression

module OsuScoreMigration =

    let do_stuff (chart: Chart, header: ChartImportHeader, replay: ReplayData, rate: float32, score_data: OsuScoreDatabase_Score) =
        let metric =
            ScoreProcessor.run (``osu!``.create 8.0f ``osu!``.NoMod) chart.Keys (StoredReplayProvider(replay)) chart.Notes rate

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

        if 100.0 * abs (acc - metric.Accuracy) > 0.2 then
            printfn "Score on %s [%s]\n----" header.Title header.DiffName

            printfn
                "Interlude says: %.2f%% accuracy\n%04i|%04i|%04i|%04i|%04i|%04i\n%ix\n----"
                (metric.Accuracy * 100.0)
                metric.State.Judgements.[0]
                metric.State.Judgements.[1]
                metric.State.Judgements.[2]
                metric.State.Judgements.[3]
                metric.State.Judgements.[4]
                metric.State.Judgements.[5]
                metric.State.BestCombo

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

    let import_scores () =

        Logging.Info "Reading osu database ..."

        use file = Path.Combine(Imports.OSU_SONG_FOLDER, "..", "scores.db") |> File.OpenRead

        use reader = new BinaryReader(file, Text.Encoding.UTF8)
        let scores = OsuScoreDatabase.Read(reader)

        use file = Path.Combine(Imports.OSU_SONG_FOLDER, "..", "osu!.db") |> File.OpenRead

        use reader = new BinaryReader(file, Text.Encoding.UTF8)
        let main_db = OsuDatabase.Read(reader)

        for beatmap_data in
            main_db.Beatmaps
            |> Seq.filter (fun b -> b.Mode = 3uy && round b.OverallDifficulty = 8.0f) do
            let osu_file =
                Path.Combine(Imports.OSU_SONG_FOLDER, beatmap_data.FolderName, beatmap_data.Filename)

            let chart =
                try
                    Beatmap.FromFile osu_file
                    |> expect
                    |> fun b ->
                        Osu_To_Interlude.convert
                            b
                            {
                                Config = ConversionOptions.Default
                                Source = osu_file
                            }
                    |> expect
                    |> Some
                with _ ->
                    None

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
                        | Some(rate, _) -> do_stuff (chart.Value.Chart, chart.Value.Header, interlude_replay, rate, replay_data)
                        | None -> ()

                | None -> ()

        Console.ReadLine() |> ignore

    let replay_generator (mods: Mods) (hash: string) : OsuScoreDatabase_Score =
        let inputs : (int * int) seq =
            seq {
                yield -159, -1 // 158 in window // 159 out //158.5 on od10
                yield 130, 500
                yield 1000, 1500
            }
        let input_as_replay_string =
            let mutable previous_time = -1000
            inputs 
            |> Seq.map (fun (press, release) -> 
                let t = previous_time
                previous_time <- release
                sprintf "%i|1|1|0,%i|0|1|0" (press - t) (release - press)
            )
            |> String.concat ","
        let raw =
            sprintf
                "0|256|500|0,0|256|500|0,-1000|0|1|0,%s,-12345|0|0|32767"
                input_as_replay_string
            |> Text.Encoding.UTF8.GetBytes

        use input = new MemoryStream(raw)
        use output = new MemoryStream()

        let encode = new LZMA.Encoder()
        encode.WriteCoderProperties(output)
        output.Write(BitConverter.GetBytes(input.Length), 0, 8)
        encode.Code(input, output, input.Length, -1, null)
        output.Flush()

        {
            Mode = 3uy
            Version = 20220216
            BeatmapHash = hash
            Player = "Percyqaz"
            ReplayHash = hash
            Count300 = 1s
            Count100 = 0s
            Count50 = 0s
            CountGeki = 0s
            CountKatu = 0s
            CountMiss = 0s
            Score = 1000001
            MaxCombo = 727s
            PerfectCombo = true
            ModsUsed = mods
            LifeBarGraph = ""
            Timestamp = DateTime.UtcNow.ToFileTimeUtc()
            CompressedReplayBytes = Some <| output.ToArray()
            OnlineScoreID = 0
        }

    let replay_writer () =

        Logging.Info "Reading osu database ..."

        use file = Path.Combine(Imports.OSU_SONG_FOLDER, "..", "osu!.db") |> File.OpenRead

        use reader = new BinaryReader(file, Text.Encoding.UTF8)
        let main_db = OsuDatabase.Read(reader)

        match
            main_db.Beatmaps
            |> Seq.tryFind (fun b -> b.Difficulty = "thanks for holding")
        with
        | Some b ->

            let r = replay_generator Mods.None b.Hash
            use fs = File.Open("replay.osr", FileMode.Create)
            use bw = new BinaryWriter(fs)
            r.Write bw
            bw.Flush()
            bw.Close()

            Diagnostics.Process
                .Start(new Diagnostics.ProcessStartInfo("replay.osr", UseShellExecute = true))
                .WaitForExit()

        | None -> printfn "didnt find the map"

    let main () =
        Logging.Info "Reading osu database ..."

        use file = Path.Combine("C:/Users/percy/Downloads/osu!.db") |> File.OpenRead
        use reader = new BinaryReader(file, Text.Encoding.UTF8)
        let main_db = OsuDatabase.Read(reader)

        printfn "%A" main_db
