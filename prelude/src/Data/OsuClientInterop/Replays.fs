namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open System.Text
open SevenZip.Compression
open Prelude
open Prelude.Charts
open Prelude.Gameplay

module OsuReplay =

    let decode_replay (replay: OsuScoreDatabase_Score, chart: Chart, rate: Rate) : ReplayData =

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

        let mutable time = -chart.FirstNote
        let mutable last_state = 256us

        seq {
            for entry in string_data.Split(",", StringSplitOptions.RemoveEmptyEntries) do
                let parts = entry.Split("|")

                if parts.[0] <> "-12345" then
                    time <- time + float32 parts.[0] * rate * 1.0f<ms / rate>
                    let state = uint16 parts.[1]

                    if state <> last_state then
                        yield struct (time, uint16 parts.[1])
                        last_state <- state
        }
        |> Array.ofSeq

    let encode_replay (replay: ReplayData) (mods: Mods) (beatmap_hash: string) =

        let input_as_replay_string =
            let mutable previous_time = -1000
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
