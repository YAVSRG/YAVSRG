namespace Prelude.Gameplay.Replays

open System.IO
open Percyqaz.Common
open Prelude

/// Replay implementation that represents another player's score, streaming in from a network
type OnlineReplay() =
    let mutable i = 0
    let mutable finished = false
    let mutable current_chart_time = 0.0f<ms>
    let buffer = ResizeArray<ReplayFrame>()

    interface IReplay with
        member this.Finished = finished

        member this.HasNext(time: ChartTime) : bool =
            if i >= buffer.Count then
                false
            else
                let struct (t, _) = buffer.[i]
                t <= time

        member this.GetNext() : ReplayFrame =
            i <- i + 1
            buffer.[i - 1]

        member this.GetFullReplay() : ReplayData =
            if finished then
                buffer.ToArray()
            else
                invalidOp "Online play is not declared as over, we don't have the full replay yet!"

        member this.EnumerateRecentFrames() : ReplayFrame seq =
            seq {
                let mutable j = i - 1
                while j >= 0 do
                    yield buffer.[j]
                    j <- j - 1
            }

    member this.ImportLiveBlock(timestamp: float32<ms>, br: BinaryReader) : unit =
        if finished then
            invalidOp "Online play is declared as over; cannot append to replay"
        elif timestamp < current_chart_time then
            failwithf "Timestamp for replay data went backwards: %f, %f" timestamp current_chart_time
        else
            current_chart_time <- timestamp
            try
                while not (br.BaseStream.Position = br.BaseStream.Length) do
                    buffer.Add(struct (br.ReadSingle() * 1.0f<ms>, br.ReadUInt16()))
            with err ->
                Logging.Error "Error while receiving online replay data: %O" err

    member this.Finish() : unit =
        if not finished then
            finished <- true
        else
            invalidOp "Online play is already declared as over; cannot do so again"

    member this.Time() : ChartTime = current_chart_time