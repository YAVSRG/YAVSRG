namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Prelude.Calculator
open Prelude.Data.User
open Prelude.Data.Library

type SessionXPGain =
    {
        QuitPenalty: int64
        BaseXP: int64
        LampXP: int64
        AccXP: int64
        SkillXP: int64
    }
    member this.Total = this.QuitPenalty + this.BaseXP + this.LampXP + this.AccXP + this.SkillXP

module Stats =

    let save (database: UserDatabase) = save_stats database

    let private end_current_session (now: int64) (database: UserDatabase) =
        TOTAL_STATS <- TOTAL_STATS.AddSession CURRENT_SESSION
        if CURRENT_SESSION.NotesHit > 0 then
            let database_session : Session = CURRENT_SESSION.ToSession

            let date = timestamp_to_rg_calendar_day database_session.Start |> DateOnly.FromDateTime
            match PREVIOUS_SESSIONS.TryFind date with
            | Some sessions -> PREVIOUS_SESSIONS <- PREVIOUS_SESSIONS.Add (date, database_session :: sessions)
            | None -> PREVIOUS_SESSIONS <- PREVIOUS_SESSIONS.Add (date, [database_session])

            DbSessions.save database_session database.Database

        CURRENT_SESSION <- CurrentSession.StartNew now CURRENT_SESSION.LastTime CURRENT_SESSION.KeymodeSkills
        save database

    let save_current_session (now: int64) (database: UserDatabase) =
        CURRENT_SESSION.LastTime <- now
        if now - SESSION_TIMEOUT > CURRENT_SESSION.LastPlay then
            end_current_session now database
        elif now < CURRENT_SESSION.LastTime then
            Logging.Error "System clock changes could break your session stats"
            end_current_session now database
        else
            save database

    let QUIT_PENALTY = -100L
    let quitter_penalty (database: UserDatabase) =
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + QUIT_PENALTY |> max 0L
        save_current_session (Timestamp.now()) database
        {
            QuitPenalty = QUIT_PENALTY
            BaseXP = 0L
            LampXP = 0L
            AccXP = 0L
            SkillXP = 0L
        }

    let handle_score (score_info: ScoreInfo) (improvement: ImprovementFlags) (database: UserDatabase) =
        if score_info.TimePlayed - int64 (score_info.ChartMeta.Length / score_info.Rate) - STREAK_TIMEOUT > CURRENT_SESSION.LastPlay then
            CURRENT_SESSION.Streak <- CURRENT_SESSION.Streak + 1
        else
            CURRENT_SESSION.Streak <- 1
        CURRENT_SESSION.LastPlay <- max CURRENT_SESSION.LastPlay score_info.TimePlayed

        let base_xp = score_info.Scoring.JudgementCounts |> Array.truncate 5 |> Array.sum
        let streak_bonus = float32 (CURRENT_SESSION.Streak - 1) * 0.1f |> max 0.0f |> min 1.0f

        let lamp_bonus_flat, lamp_bonus_mult =
            match improvement.Lamp with
            | Improvement.None -> 0L, 0.0f
            | Improvement.New
            | Improvement.Faster _ -> 100L, 0.1f
            | Improvement.Better _ -> 200L, 0.4f
            | Improvement.FasterBetter _ -> 200L, 0.9f

        let acc_bonus_flat, acc_bonus_mult =
            match improvement.Accuracy with
            | Improvement.None -> 0L, 0.0f
            | Improvement.New
            | Improvement.Faster _ -> 100L, 0.1f
            | Improvement.Better _ -> 200L, 0.2f
            | Improvement.FasterBetter _ -> 200L, 0.3f

        let base_xp = int64 base_xp + int64 (float32 base_xp * streak_bonus)
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + base_xp

        let lamp_xp = lamp_bonus_flat + int64 (float32 base_xp * lamp_bonus_mult)
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + lamp_xp

        let acc_xp = acc_bonus_flat + int64 (float32 base_xp * acc_bonus_mult)
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + acc_xp

        let all_time_skill_up =
            KeymodeSkillBreakdown.score
                score_info.ChartMeta.Patterns
                score_info.Accuracy
                score_info.Rate
                TOTAL_STATS.KeymodeSkills.[score_info.WithMods.Keys - 3]

        let session_skill_up =
            KeymodeSkillBreakdown.score
                score_info.ChartMeta.Patterns
                score_info.Accuracy
                score_info.Rate
                CURRENT_SESSION.KeymodeSkills.[score_info.WithMods.Keys - 3]

        save_current_session (Timestamp.now()) database

        let skill_xp_mult = (float32 TOTAL_STATS.XP / 100_000f - 1.0f) |> max 0.0f |> min 1.0f
        let skill_xp =
            int64 (session_skill_up.Total * skill_xp_mult)
            + int64 (all_time_skill_up.Total * 10.0f * skill_xp_mult)
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + skill_xp

        {
            QuitPenalty = 0L
            BaseXP = base_xp // todo: separate into base + streak
            LampXP = lamp_xp
            AccXP = acc_xp
            SkillXP = skill_xp // todo: separate into session/all time
        }

    let init (library: Library) (database: UserDatabase) =

        let sessions = DbSessions.get_all database.Database
        PREVIOUS_SESSIONS <-
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        load_stats database

        Migration.migrate library database

        let now = Timestamp.now()
        if CURRENT_SESSION.NotesHit = 0 then
            end_current_session now database
        elif Timestamp.now() - SESSION_TIMEOUT > CURRENT_SESSION.LastPlay then
            end_current_session now database
        elif now < CURRENT_SESSION.LastTime then
            Logging.Error "System clock changes could break your session stats"
            end_current_session now database