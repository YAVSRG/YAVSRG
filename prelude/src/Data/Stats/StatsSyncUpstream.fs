namespace Prelude.Data.Stats

open Percyqaz.Data

[<Json.AutoCodec>]
type StatsSyncUpstream =
    {
        Playtime: float
        PracticeTime: float
        GameTime: float
        NotesHit: int

        XP: int64
        KeymodePlaytime: Map<int, float>

        NetworkId: int64
        Month: int
        KeymodePlaytimeThisMonth: Map<int, float>
        PlaytimeThisMonth: float
        XPThisMonth: int64
    }

    static member Create(state: Stats) : StatsSyncUpstream option =
        match state.BoundNetworkId with
        | None -> None
        | Some network_id ->

        let month = state.Clock.Now() |> timestamp_to_leaderboard_month
        let month_start = start_of_leaderboard_month month
        let month_end = start_of_leaderboard_month(month + 1)

        let sessions =
            state.PreviousSessions.Values
            |> Seq.concat
            |> Seq.filter(fun s -> s.Start >= month_start && s.End <= month_end)
            |> Array.ofSeq

        {
            Playtime = state.TotalStats.PlayTime + state.CurrentSession.PlayTime
            PracticeTime = state.TotalStats.PracticeTime + state.CurrentSession.PracticeTime
            GameTime = state.TotalStats.GameTime + state.CurrentSession.GameTime
            NotesHit = state.TotalStats.NotesHit + state.CurrentSession.NotesHit

            XP = state.TotalStats.XP + state.CurrentSession.SessionScore
            KeymodePlaytime = add_playtimes state.TotalStats.KeymodePlaytime state.CurrentSession.KeymodePlaytime

            NetworkId = network_id
            Month = month
            KeymodePlaytimeThisMonth =
                let month_playtimes =
                    sessions |> Seq.map _.KeymodePlaytime |> Seq.fold add_playtimes Map.empty

                add_playtimes month_playtimes state.CurrentSession.KeymodePlaytime
            PlaytimeThisMonth =
                let month_playtime = sessions |> Array.sumBy _.PlayTime
                month_playtime + state.CurrentSession.PlayTime
            XPThisMonth =
                let month_xp = sessions |> Array.sumBy _.XP
                month_xp + state.CurrentSession.SessionScore
        }
        |> Some
