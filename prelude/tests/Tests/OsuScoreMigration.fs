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

    let do_stuff(chart: Chart, replay: ReplayData) =
        let metric = Metrics.createScoreMetric (PrefabRulesets.Osu.create 8.0f) chart.Keys (StoredReplayProvider(replay)) chart.Notes 1.0f
        metric.Update(Time.infinity)
        printfn "That replay scored %.2f%% accuracy on osu OD8" (metric.Value * 100.0)

    let main() =

        Logging.Info "Reading osu database ..."
        use file = Path.Combine(Prelude.Data.Charts.Library.Imports.osuSongFolder, "..", "scores.db") |> File.OpenRead
        use reader = new BinaryReader(file, System.Text.Encoding.UTF8)
        let scores = ScoreDatabase.Read(reader)

        use file = Path.Combine(Prelude.Data.Charts.Library.Imports.osuSongFolder, "..", "osu!.db") |> File.OpenRead
        use reader = new BinaryReader(file, System.Text.Encoding.UTF8)
        let main_db = OsuDatabase.Read(reader)

        match main_db.Beatmaps |> Seq.tryFind (fun b -> b.DifficultyID = 2146992) with
        | Some checkmate ->
            let osu_file = Path.Combine(Prelude.Data.Charts.Library.Imports.osuSongFolder, checkmate.FolderName, checkmate.Filename)
            let chart =
                loadBeatmapFile osu_file
                |> fun b -> ``osu!``.toInterlude b { Config = ConversionOptions.Default; Source = osu_file }

            match scores.Beatmaps |> Seq.tryFind (fun b -> b.Hash = checkmate.Hash) with
            | Some scores ->
                for score in scores.Scores do
                    let replay_file = Path.Combine(Prelude.Data.Charts.Library.Imports.osuSongFolder, "..", "Data", "r", sprintf "%s-%i.osr" score.BeatmapHash score.Timestamp)
                    use file = File.OpenRead replay_file
                    use br = new BinaryReader(file)
                    let replay_data = ScoreDatabase_Score.Read br

                    //
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
                        let mutable time = -chart.FirstNote
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
                    
                    do_stuff(chart, interlude_replay)

            | None -> ()
        | None -> printfn "Checkmate!? not found"

        Console.ReadLine() |> ignore