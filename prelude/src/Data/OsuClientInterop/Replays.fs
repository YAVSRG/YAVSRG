namespace Prelude.Data.OsuClientInterop

open System
open System.IO
open System.Text
open SevenZip.Compression
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Gameplay

module OsuReplay =

    let decode_replay (replay: OsuScoreDatabase_Score, chart: Chart, rate: Rate) : Result<ReplayData, string> =
        try
            if replay.CompressedReplayBytes.IsNone then 
                failwith "No replay data here.\nMaybe you passed a score directly from the osu! database instead of reading the replay file from disk?"

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
            |> Ok
        with err ->
            Logging.Error("Failed to decode osu! replay", err)
            Error err.Message

