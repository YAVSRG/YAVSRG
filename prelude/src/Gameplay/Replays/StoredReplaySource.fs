namespace Prelude.Gameplay.Replays

open Prelude
open Prelude.Charts

/// Replay implementation for a replay that already existed in full
type StoredReplaySource(data: Replay) =
    let mutable i = 0

    interface ReplaySource with
        member this.Finished = i >= data.Frames.Length

        member this.HasNext(time: ChartTime) : bool =
            if i >= data.Frames.Length then
                false
            else
                data.Frames.[i].Time <= time

        member this.GetNext() : ReplayFrame =
            i <- i + 1
            data.Frames.[i - 1]

        member this.GetFullReplay() : Replay = data

        member this.EnumerateRecentFrames() : ReplayFrame seq =
            seq {
                let mutable j = i - 1
                while j >= 0 do
                    yield data.Frames.[j]
                    j <- j - 1
            }

    new(data: string) = StoredReplaySource(Replay.FromBase64String data)

    static member AutoPlay(note_data: NoteData) : StoredReplaySource =
        Replay.perfect_replay note_data |> StoredReplaySource

    static member WavingAutoPlay(note_data: NoteData) : StoredReplaySource =
        Replay.auto_replay_waving note_data |> StoredReplaySource