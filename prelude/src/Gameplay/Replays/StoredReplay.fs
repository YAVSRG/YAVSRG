namespace Prelude.Gameplay.Replays

open Prelude
open Prelude.Charts

/// Replay implementation for a replay that already existed in full
type StoredReplayProvider(data: ReplayData) =
    let mutable i = 0

    interface IReplay with
        member this.Finished = i >= data.Length

        member this.HasNext(time: ChartTime) : bool =
            if i >= data.Length then
                false
            else
                let struct (t, _) = data.[i]
                t <= time

        member this.GetNext() : ReplayFrame =
            i <- i + 1
            data.[i - 1]

        member this.GetFullReplay() : ReplayData = data

        member this.EnumerateRecentEvents() : ReplayFrame seq =
            seq {
                let mutable j = i - 1
                while j >= 0 do
                    yield data.[j]
                    j <- j - 1
            }

    new(data: string) = StoredReplayProvider(Replay.decompress_string data)

    static member AutoPlay(keys: int, notes: TimeArray<NoteRow>) : StoredReplayProvider =
        Replay.perfect_replay keys notes |> StoredReplayProvider

    static member WavingAutoPlay(keys: int, notes: TimeArray<NoteRow>) : StoredReplayProvider =
        Replay.auto_replay_waving keys notes |> StoredReplayProvider