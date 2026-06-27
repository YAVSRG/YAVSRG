namespace Prelude.Gameplay.Replays

open System.IO
open Percyqaz.Common
open Prelude

/// Replay implementation that supports writing information to the underlying buffer live during gameplay
type LiveReplay(first_note: Time) =
    let mutable i = 0
    let mutable finished = false
    let mutable export = 0
    let buffer = ResizeArray<ReplayFrame>()

    let mutable last_time : ChartTime = -Time.infinity

    member this.AddFrame(time: Time, pressed_keys: Bitmask) : unit =
        if finished then invalidOp "Live play is declared as over; cannot append to replay"
        if time - first_note < last_time then
            Logging.Warn "Timestamp for replay data went backwards: %f, %f" (time - first_note) last_time

        last_time <- max last_time (time - first_note)
        buffer.Add(ReplayFrame.Create(last_time, pressed_keys))

    interface ReplaySource with
        member this.Finished = finished

        member this.HasNext(time: ChartTime) : bool =
            if i >= buffer.Count then
                false
            else
                buffer.[i].Time <= time

        member this.GetNext() : ReplayFrame =
            i <- i + 1
            buffer.[i - 1]

        member this.GetFullReplay() : ReplayData =
            if not finished then invalidOp "Live play is not declared as over, we don't have the full replay yet!"
            buffer.ToArray()

        member this.EnumerateRecentFrames() : ReplayFrame seq =
            seq {
                let mutable j = buffer.Count - 1
                while j >= 0 do
                    yield buffer.[j]
                    j <- j - 1
            }

    member this.Finish() : unit =
        if finished then invalidOp "Live play is already declared as over; cannot do so again"
        finished <- true

    /// Export all data not yet exported, for sending over a network in multiplayer
    member this.ExportLiveBlock(bw: BinaryWriter) : unit =
        while export < buffer.Count do
            let replay_frame = buffer.[export]

            if replay_frame.Time > -1000.0f<ms> then
                replay_frame.WriteToStream(bw)

            export <- export + 1

        bw.Flush()