namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Prelude.Data.User
open Prelude.Data.Library

type SessionXPGain =
    {
        QuitPenalty: int64
        BaseXP: int64
        LampXP: int64
        AccXP: int64
    }
    member this.Total = this.QuitPenalty + this.BaseXP + this.LampXP + this.AccXP

module Stats =
    
    let STATE = StatsState.Default

    let save (database: UserDatabase) : unit = STATE.Save(database)

    let private end_current_session (now: int64) (database: UserDatabase) : unit =
        STATE.TotalStats <- STATE.TotalStats.AddSession(STATE.CurrentSession)
        if STATE.CurrentSession.NotesHit > 0 then
            let database_session : Session = STATE.CurrentSession.ToSession

            let date = timestamp_to_rg_calendar_day database_session.Start |> DateOnly.FromDateTime
            match STATE.PreviousSessions.TryFind date with
            | Some sessions -> STATE.PreviousSessions <- STATE.PreviousSessions.Add (date, database_session :: sessions)
            | None -> STATE.PreviousSessions <- STATE.PreviousSessions.Add (date, [database_session])

            DbSessions.save database_session database.Database

        STATE.CurrentSession <- CurrentSession.StartNew now
        save database

    let save_current_session (now: int64) (database: UserDatabase) : unit =
        STATE.CurrentSession.LastTime <- now
        if now - SESSION_TIMEOUT > STATE.CurrentSession.LastPlay then
            end_current_session now database
        elif now < STATE.CurrentSession.LastTime then
            Logging.Error "System clock changes could break your session stats"
            end_current_session now database
        else
            save database

    let QUIT_PENALTY = -100L
    let quitter_penalty (database: UserDatabase) : SessionXPGain =
        STATE.CurrentSession.SessionScore <- STATE.CurrentSession.SessionScore + QUIT_PENALTY |> max 0L
        save_current_session (Timestamp.now()) database
        {
            QuitPenalty = QUIT_PENALTY
            BaseXP = 0L
            LampXP = 0L
            AccXP = 0L
        }

    let handle_score (score_info: ScoreInfo) (improvement: ImprovementFlags) (database: UserDatabase) : SessionXPGain =
        if score_info.TimePlayed - int64 (score_info.ChartMeta.Length / score_info.Rate) - STREAK_TIMEOUT > STATE.CurrentSession.LastPlay then
            STATE.CurrentSession.Streak <- STATE.CurrentSession.Streak + 1
        else
            STATE.CurrentSession.Streak <- 1
        STATE.CurrentSession.LastPlay <- max STATE.CurrentSession.LastPlay score_info.TimePlayed

        let base_xp = score_info.Scoring.JudgementCounts |> Array.truncate 5 |> Array.sum
        let streak_bonus = float32 (STATE.CurrentSession.Streak - 1) * 0.1f |> max 0.0f |> min 1.0f

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
        STATE.CurrentSession.SessionScore <- STATE.CurrentSession.SessionScore + base_xp

        let lamp_xp = lamp_bonus_flat + int64 (float32 base_xp * lamp_bonus_mult)
        STATE.CurrentSession.SessionScore <- STATE.CurrentSession.SessionScore + lamp_xp

        let acc_xp = acc_bonus_flat + int64 (float32 base_xp * acc_bonus_mult)
        STATE.CurrentSession.SessionScore <- STATE.CurrentSession.SessionScore + acc_xp

        save_current_session (Timestamp.now()) database

        {
            QuitPenalty = 0L
            BaseXP = base_xp // todo: separate into base + streak
            LampXP = lamp_xp
            AccXP = acc_xp
        }

    let init (library: Library) : unit =

        let sessions = DbSessions.get_all library.UserData.Database
        STATE.PreviousSessions <-
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        STATE.Load(library.UserData)

        Migration.migrate(STATE, library)

        let now = Timestamp.now()
        if STATE.CurrentSession.NotesHit = 0 then
            end_current_session now library.UserData
        elif now - SESSION_TIMEOUT > STATE.CurrentSession.LastPlay then
            end_current_session now library.UserData
        elif now < STATE.CurrentSession.LastTime then
            Logging.Error "System clock changes could break your session stats"
            end_current_session now library.UserData