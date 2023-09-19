namespace Prelude.Test

open System
open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Charts.Formats.``osu!``
open Prelude.Charts.Formats.Interlude
open Prelude.Charts.Formats.Conversions
open Prelude.Data.``osu!``
open Prelude.Gameplay
open SevenZip.Compression

module OsuScoreMigration =

    let do_stuff(chart: Chart, replay: ReplayData, rate: float32, score_data: ScoreDatabase_Score) =
        let metric = Metrics.createScoreMetric (PrefabRulesets.Osu.create 8.0f) chart.Keys (StoredReplayProvider(replay)) chart.Notes rate
        metric.Update(Time.infinity)
        let sum = 300.0 * float (score_data.CountGeki + score_data.Count300 + score_data.CountKatu + score_data.Count100 + score_data.Count50 + score_data.CountMiss)
        let acc = 
            300.0 * float (score_data.CountGeki + score_data.Count300)
            + 200.0 * float score_data.CountKatu
            + 100.0 * float score_data.Count100
            + 50.0 * float score_data.Count50
            |> fun total -> total / sum
        if 100.0 * abs (acc - metric.Value) > 0.2 then
            printfn "Score on %s [%s]\n----" chart.Header.Title chart.Header.DiffName
            printfn "Interlude says: %.2f%% accuracy\n%04i|%04i|%04i|%04i|%04i|%04i\n%ix\n----" 
                (metric.Value * 100.0)
                metric.State.Judgements.[0]
                metric.State.Judgements.[1]
                metric.State.Judgements.[2]
                metric.State.Judgements.[3]
                metric.State.Judgements.[4]
                metric.State.Judgements.[5]
                metric.State.BestCombo
            printfn "osu! says: %.2f%% accuracy\n%04i|%04i|%04i|%04i|%04i|%04i\n%ix\n----" 
                (acc * 100.0)
                score_data.CountGeki
                score_data.Count300
                score_data.CountKatu
                score_data.Count100
                score_data.Count50
                score_data.CountMiss
                score_data.MaxCombo
            Console.ReadLine() |> ignore

    let replay_generator (head_offset: int) (tail_offset: int) (hash: string) : ScoreDatabase_Score =
        let raw = sprintf "0|256|500|0,0|256|500|0,-1000|0|1|0,%i|1|1|0,%i|0|1|0,-12345|0|0|32767" (1000 + head_offset) (500 + tail_offset - head_offset) |> Text.Encoding.UTF8.GetBytes
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
            Count300 = 0s
            Count100 = 0s
            Count50 = 0s
            CountGeki = 0s
            CountKatu = 0s
            CountMiss = 0s
            Score = 1000001
            MaxCombo = 727s
            PerfectCombo = true
            ModsUsed = Mods.None
            LifeBarGraph = ""
            Timestamp = DateTime.UtcNow.ToFileTimeUtc()
            CompressedReplayBytes = Some <| output.ToArray()
            OnlineScoreID = 0
        }

    let main() =

        Logging.Info "Reading osu database ..."
        use file = Path.Combine(Data.Charts.Library.Imports.osuSongFolder, "..", "scores.db") |> File.OpenRead
        use reader = new BinaryReader(file, Text.Encoding.UTF8)
        let scores = ScoreDatabase.Read(reader)

        use file = Path.Combine(Data.Charts.Library.Imports.osuSongFolder, "..", "osu!.db") |> File.OpenRead
        use reader = new BinaryReader(file, Text.Encoding.UTF8)
        let main_db = OsuDatabase.Read(reader)

        for beatmap_data in main_db.Beatmaps |> Seq.filter (fun b -> b.Mode = 3uy && round b.OverallDifficulty = 8.0f) do
            let osu_file = Path.Combine(Data.Charts.Library.Imports.osuSongFolder, beatmap_data.FolderName, beatmap_data.Filename)
            let chart =
                try
                    loadBeatmapFile osu_file
                    |> fun b -> ``osu!``.toInterlude b { Config = ConversionOptions.Default; Source = osu_file }
                    |> Some
                with _ -> None

            if chart.IsNone then () else

            match scores.Beatmaps |> Seq.tryFind (fun b -> b.Hash = beatmap_data.Hash) with
            | Some scores ->
                for score in scores.Scores do
                    let replay_file = Path.Combine(Data.Charts.Library.Imports.osuSongFolder, "..", "Data", "r", sprintf "%s-%i.osr" score.BeatmapHash score.Timestamp)
                    use file = File.OpenRead replay_file
                    use br = new BinaryReader(file)
                    let replay_data = ScoreDatabase_Score.Read br

                    let input = new MemoryStream(replay_data.CompressedReplayBytes.Value)
                    let output = new MemoryStream()

                    let props = Array.zeroCreate 5
                    input.Read(props, 0, 5) |> ignore

                    let lengthBytes = Array.zeroCreate 8
                    input.Read(lengthBytes, 0, 8) |> ignore

                    let dec = LZMA.Decoder()
                    dec.SetDecoderProperties props

                    dec.Code(input, output, replay_data.CompressedReplayBytes.Value.Length, BitConverter.ToInt64(lengthBytes, 0), null)
                    output.Flush()
                    let string_data =
                        output.ToArray()
                        |> System.Text.Encoding.UTF8.GetString

                    let interlude_replay : ReplayData =
                        let mutable time = -chart.Value.FirstNote
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
                        } |> Array.ofSeq

                    match Mods.to_interlude_rate_and_mods replay_data.ModsUsed with
                    | Some (rate, _) -> do_stuff(chart.Value, interlude_replay, rate, replay_data)
                    | None -> ()

            | None -> ()

        Console.ReadLine() |> ignore

    let replay_writer() =
        
        Logging.Info "Reading osu database ..."
        use file = Path.Combine(Data.Charts.Library.Imports.osuSongFolder, "..", "osu!.db") |> File.OpenRead
        use reader = new BinaryReader(file, Text.Encoding.UTF8)
        let main_db = OsuDatabase.Read(reader)
        
        match main_db.Beatmaps |> Seq.tryFind (fun b -> b.Difficulty = "THANK YOU FOR HOLDING") with
        | Some b ->
            
            while true do
                printf "enter first offset> "
                let first_offset = Console.ReadLine() |> int
                printf "enter second offset> "
                let offset = Console.ReadLine() |> int

                let r = replay_generator first_offset offset b.Hash
                use fs = File.Open("replay.osr", FileMode.Create)
                use bw = new BinaryWriter(fs)
                r.Write bw
                bw.Flush()
                bw.Close()

                System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo("replay.osr", UseShellExecute = true)).WaitForExit()
        | None -> printfn "didnt find the map"
            
        Console.ReadLine() |> ignore