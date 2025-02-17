namespace Prelude.Data.User.Stats

open Percyqaz.Common
open Percyqaz.Data
open Prelude.Calculator

[<Json.AutoCodec>]
type StatsSyncUpstream =
    {
        Playtime: float
        PracticeTime: float
        GameTime: float
        NotesHit: int

        XP: int64
        KeymodePlaytime: Map<int, float>

        KeymodeSkillsAllTime: KeymodeTinyBreakdown array
        KeymodeSkillsRecent: KeymodeTinyBreakdown array

        NetworkId: int64
        Month: int
        KeymodePlaytimeThisMonth: Map<int, float>
        PlaytimeThisMonth: float
        XPThisMonth: int64
    }

    static member Create () : StatsSyncUpstream option =
        match BOUND_NETWORK_ID with
        | None -> None
        | Some network_id ->

        let month = Timestamp.now() |> timestamp_to_leaderboard_month
        let month_start = start_of_leaderboard_month month
        let month_end = start_of_leaderboard_month (month + 1)

        let sessions =
            PREVIOUS_SESSIONS.Values
            |> Seq.concat
            |> Seq.filter (fun s -> s.Start >= month_start && s.End <= month_end)
            |> Array.ofSeq

        {
            Playtime = TOTAL_STATS.PlayTime + CURRENT_SESSION.PlayTime
            PracticeTime = TOTAL_STATS.PracticeTime + CURRENT_SESSION.PracticeTime
            GameTime = TOTAL_STATS.GameTime + CURRENT_SESSION.GameTime
            NotesHit = TOTAL_STATS.NotesHit + CURRENT_SESSION.NotesHit

            XP = TOTAL_STATS.XP + CURRENT_SESSION.SessionScore
            KeymodePlaytime = add_playtimes TOTAL_STATS.KeymodePlaytime CURRENT_SESSION.KeymodePlaytime

            KeymodeSkillsAllTime = TOTAL_STATS.KeymodeSkills |> Array.map _.Tiny
            KeymodeSkillsRecent = CURRENT_SESSION.KeymodeSkills |> Array.map _.Tiny

            NetworkId = network_id
            Month = month
            KeymodePlaytimeThisMonth =
                let month_playtimes = sessions |> Seq.map _.KeymodePlaytime |> Seq.fold add_playtimes Map.empty
                add_playtimes month_playtimes CURRENT_SESSION.KeymodePlaytime
            PlaytimeThisMonth =
                let month_playtime = sessions |> Array.sumBy _.PlayTime
                month_playtime + CURRENT_SESSION.PlayTime
            XPThisMonth =
                let month_xp = sessions |> Array.sumBy _.XP
                month_xp + CURRENT_SESSION.SessionScore
        }
        |> Some

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

    member this.Accept : bool =
        match BOUND_NETWORK_ID with
        | Some id when id <> this.NetworkId -> false
        | _ ->
        // in theory if never synced, could ADD online stats to current

        BOUND_NETWORK_ID <- Some this.NetworkId
        TOTAL_STATS <-
            { TOTAL_STATS with
                PlayTime = safe_stat_max TOTAL_STATS.PlayTime (this.PlayTime - CURRENT_SESSION.PlayTime)
                PracticeTime = safe_stat_max TOTAL_STATS.PracticeTime (this.PracticeTime - CURRENT_SESSION.PracticeTime)
                GameTime = safe_stat_max TOTAL_STATS.GameTime (this.GameTime - CURRENT_SESSION.GameTime)
                NotesHit = max TOTAL_STATS.NotesHit (this.NotesHit - CURRENT_SESSION.NotesHit)
                XP = max TOTAL_STATS.XP (this.XP - CURRENT_SESSION.SessionScore)
                KeymodePlaytime = combine_playtimes safe_stat_max TOTAL_STATS.KeymodePlaytime (combine_playtimes (-) this.KeymodePlaytime CURRENT_SESSION.KeymodePlaytime)
            }

        true