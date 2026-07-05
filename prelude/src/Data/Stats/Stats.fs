namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Percyqaz.Data.Sqlite
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

type Stats(database: Database) =
    let QUIT_PENALTY = -100L
    member val STATE = StatsState.Default
    
    member this.Save() : unit = this.STATE.Save(database)

    member private this.EndCurrentSession(now: int64) : unit =
        
        let inline add_session_to_state(session: Session) : unit =
            let date = timestamp_to_rg_calendar_day session.Start |> DateOnly.FromDateTime
            match this.STATE.PreviousSessions.TryFind date with
            | Some sessions -> this.STATE.PreviousSessions <- this.STATE.PreviousSessions.Add (date, session :: sessions)
            | None -> this.STATE.PreviousSessions <- this.STATE.PreviousSessions.Add (date, [session])
        
        this.STATE.TotalStats <- this.STATE.TotalStats.AddSession(this.STATE.CurrentSession)
        if this.STATE.CurrentSession.NotesHit > 0 then
            let database_session : Session = this.STATE.CurrentSession.ToSession
            add_session_to_state(database_session)
            DbSessions.save database_session database

        this.STATE.CurrentSession <- CurrentSession.StartNew now
        this.Save()
        
    member this.SaveCurrentSession(now: int64) : unit =
        this.STATE.CurrentSession.LastTime <- now
        if now - SESSION_TIMEOUT > this.STATE.CurrentSession.LastPlay then
            this.EndCurrentSession(now)
        elif now < this.STATE.CurrentSession.LastTime then
            Logging.Error "System clock changes could break your session stats"
            this.EndCurrentSession(now)
        else
            this.Save()
            
    member this.HandleQuit() : SessionXPGain =
        this.STATE.CurrentSession.SessionScore <- this.STATE.CurrentSession.SessionScore + QUIT_PENALTY |> max 0L
        this.SaveCurrentSession(Timestamp.now())
        {
            QuitPenalty = QUIT_PENALTY
            BaseXP = 0L
            LampXP = 0L
            AccXP = 0L
        }
        
    member this.HandleScore(score_info: ScoreInfo, improvement: ImprovementFlags) : SessionXPGain =
        if score_info.TimePlayed - int64 (score_info.ChartMeta.Length / score_info.Rate) - STREAK_TIMEOUT > this.STATE.CurrentSession.LastPlay then
            this.STATE.CurrentSession.Streak <- this.STATE.CurrentSession.Streak + 1
        else
            this.STATE.CurrentSession.Streak <- 1
        this.STATE.CurrentSession.LastPlay <- max this.STATE.CurrentSession.LastPlay score_info.TimePlayed

        let base_xp = score_info.Scoring.JudgementCounts |> Array.truncate 5 |> Array.sum // todo: actually count notes hit
        let streak_bonus = float32 (this.STATE.CurrentSession.Streak - 1) * 0.1f |> max 0.0f |> min 1.0f

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
        this.STATE.CurrentSession.SessionScore <- this.STATE.CurrentSession.SessionScore + base_xp

        let lamp_xp = lamp_bonus_flat + int64 (float32 base_xp * lamp_bonus_mult)
        this.STATE.CurrentSession.SessionScore <- this.STATE.CurrentSession.SessionScore + lamp_xp

        let acc_xp = acc_bonus_flat + int64 (float32 base_xp * acc_bonus_mult)
        this.STATE.CurrentSession.SessionScore <- this.STATE.CurrentSession.SessionScore + acc_xp

        this.SaveCurrentSession(Timestamp.now())

        {
            QuitPenalty = 0L
            BaseXP = base_xp // todo: separate into base + streak
            LampXP = lamp_xp
            AccXP = acc_xp
        }
        
    member this.Migrate(library: Library) : unit =
        Migration.migrate(this.STATE, library)
        
    member private this.TimeoutCurrentSessionIfNeeded() : unit =
        let now = Timestamp.now()
        let current_session_is_empty = this.STATE.CurrentSession.NotesHit = 0
        let current_session_has_timed_out = now - SESSION_TIMEOUT > this.STATE.CurrentSession.LastPlay
        
        if current_session_has_timed_out || current_session_is_empty then
            this.EndCurrentSession(now)
        elif now < this.STATE.CurrentSession.LastTime then
            Logging.Error "System clock changes could break your session stats"
            this.EndCurrentSession(now)
        
    member this.Init(library: Library) : unit =
        let sessions = DbSessions.get_all database
        this.STATE.PreviousSessions <-
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        this.STATE.Load(database)

        this.Migrate(library)
        this.TimeoutCurrentSessionIfNeeded()
        
    static member FromLibrary(library: Library) : Stats =
        let stats = Stats(library.UserData.Database)
        stats.Init(library)
        stats