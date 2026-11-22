namespace Prelude.Data.Stats

open Percyqaz.Data

[<Json.AutoCodec>]
type StatsSyncDownstream =
    {
        NetworkId: int64

        PlayTime: float
        PracticeTime: float
        GameTime: float
        NotesHit: int

        XP: int64
        KeymodePlaytime: Map<int, float>
    }

    member this.Accept(state: Stats) : bool =
        match state.BoundNetworkId with
        | Some id when id <> this.NetworkId -> false
        | _ ->
        // in theory if never synced, could ADD online stats to current

        state.BoundNetworkId <- Some this.NetworkId

        state.TotalStats <-
            { state.TotalStats with
                PlayTime = safe_stat_max state.TotalStats.PlayTime (this.PlayTime - state.CurrentSession.PlayTime)
                PracticeTime =
                    safe_stat_max state.TotalStats.PracticeTime (this.PracticeTime - state.CurrentSession.PracticeTime)
                GameTime = safe_stat_max state.TotalStats.GameTime (this.GameTime - state.CurrentSession.GameTime)
                NotesHit = max state.TotalStats.NotesHit (this.NotesHit - state.CurrentSession.NotesHit)
                XP = max state.TotalStats.XP (this.XP - state.CurrentSession.SessionScore)
                KeymodePlaytime =
                    combine_playtimes
                        safe_stat_max
                        state.TotalStats.KeymodePlaytime
                        (combine_playtimes (fun a b -> a - b) this.KeymodePlaytime state.CurrentSession.KeymodePlaytime)
            }

        true
