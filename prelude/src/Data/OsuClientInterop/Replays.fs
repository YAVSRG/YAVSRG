namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open System.Text
open SevenZip.Compression
open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Charts
open Prelude.Data.User

type OsuReplay = OsuScoreDatabase_Score

module OsuReplay =

    /// Decodes a replay from osu! into a Replay usable on the 1.0x Interlude version of a chart
    /// osu! Replay timestamps are lined up exactly with the original .osu file
    /// e.g. if the first note is at ms 150 then the first input should have timestamp 150 to line up
    /// Interlude replay timestamps are lined up so a replay timestamp of 0 = the first note, allowing better portability
    /// `original_osu_file_first_note` and `original_osu_file_rate` are accordingly used to make a correctly synced Interlude replay
    let decode (replay: OsuReplay, original_osu_file_first_note: Time, original_osu_file_rate: Rate) : ReplayData =
        if replay.CompressedReplayBytes.IsNone then
            failwith "No replay data here. Maybe you passed a score directly from the osu! database instead of reading the replay file from disk?"

        let input = new MemoryStream(replay.CompressedReplayBytes.Value)
        let output = new MemoryStream()

        let props = Array.zeroCreate 5
        input.Read(props, 0, 5) |> ignore

        let lengthBytes = Array.zeroCreate 8
        input.Read(lengthBytes, 0, 8) |> ignore

        let dec = LZMA.Decoder()
        dec.SetDecoderProperties props

        dec.Code(input, output, replay.CompressedReplayBytes.Value.Length, BitConverter.ToInt64(lengthBytes, 0), null)
        output.Flush()
        let string_data = output.ToArray() |> Encoding.UTF8.GetString

        let mutable time = 0.0f<ms>
        let mutable last_state = 256us

        seq {
            for entry in string_data.Split(",", StringSplitOptions.RemoveEmptyEntries) do

                let parts = entry.Split("|")

                if parts.[0] <> "-12345" then
                    time <- time + float32 parts.[0] * 1.0f<ms>
                    let state = uint16 parts.[1]

                    if state <> last_state then
                        yield struct ((time - original_osu_file_first_note) * float32 original_osu_file_rate, uint16 parts.[1])
                        last_state <- state
        }
        |> Array.ofSeq

    let encode (replay: ReplayData) (first_note: Time) (mods: Mods) (beatmap_hash: string) : OsuReplay =

        let first_note = first_note |> float32 |> round |> int

        let input_as_replay_string =
            let mutable previous_time = -first_note
            replay
            |> Seq.map (fun (struct (timestamp, key_state)) ->
                let t = previous_time
                let rounded_time = timestamp |> float32 |> round |> int
                previous_time <- rounded_time
                sprintf "%i|%i|1|0" (rounded_time - t) key_state
            )
            |> String.concat ","

        let raw =
            sprintf
                "0|256|500|0,0|256|500|0,%s,-12345|0|0|32767"
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
            FilePath = None
            Mode = 3uy
            Version = 20220216
            BeatmapHash = beatmap_hash
            Player = "Percyqaz"
            ReplayHash = beatmap_hash
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

    let to_score (replay: OsuReplay) (chart: Chart) (original_osu_file_first_note: Time) (original_osu_file_rate: float32<rate>) : Result<Score, string> =
        match Mods.to_interlude_rate_and_mods replay.ModsUsed with
        | None -> Error "Invalid mods used in replay"
        | Some(rate, mods) ->

        let combined_rate = MathF.Round(float32 rate * float32 original_osu_file_rate, 3)
        if
            combined_rate <> MathF.Round(combined_rate, 2)
            || combined_rate > 3.0f
            || combined_rate < 0.5f
        then
            Error (sprintf "Rate %.3f isn't supported in Interlude" combined_rate)
        else

        try
            let replay_data = decode(replay, original_osu_file_first_note, original_osu_file_rate)

            Ok {
                Timestamp =
                    DateTime.FromFileTimeUtc(replay.Timestamp).ToLocalTime()
                    |> Timestamp.from_datetime
                Replay = Replay.compress_bytes replay_data
                Rate = combined_rate * 1.0f<rate>
                Mods = mods
                IsImported = true
                IsFailed = false
                Keys = chart.Keys
            }
        with err -> Error err.Message