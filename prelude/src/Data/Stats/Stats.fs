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

type Stats =
    //internal
    {
        Database: Database
        mutable TotalStats : TotalStats
        mutable CurrentSession : CurrentSession
        mutable PreviousSessions : Map<DateOnly, Session list>
        mutable BoundNetworkId : int64 option
        mutable Migrations : Set<string>
    }
        
    member private this.ToSaveData() : StatsSaveData =
        {
            Migrations = this.Migrations
            TotalStats = this.TotalStats
            CurrentSession = this.CurrentSession
            BoundNetworkId = this.BoundNetworkId
        }
        
    member private this.SaveAs(id: string) : unit =
        DbSingletons.save<StatsSaveData> id (this.ToSaveData()) this.Database
    member internal this.SaveBackup(backup_id: string) : unit = this.SaveAs("stats_" + backup_id)
    member internal this.Save() : unit = this.SaveAs("stats")
        
    member this.Migrate(library: Library) : unit =
        ignore library
        //Migration.migrate(this, library)
        
    static member FromLibrary(library: Library) : Stats =
        
        let inline load_previous_sessions(database: Database) : Map<DateOnly, Session list> =
            DbSessions.get_all database
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        
        let inline load_from_database(database: Database) : Stats =
            let previous_sessions = load_previous_sessions(database)
            let stats : StatsSaveData = DbSingletons.get_or_default "stats" StatsSaveData.Default database
            
            {
                Database = database
                CurrentSession = stats.CurrentSession
                PreviousSessions = previous_sessions
                TotalStats = stats.TotalStats
                BoundNetworkId = stats.BoundNetworkId
                Migrations = stats.Migrations
            }
        
        let inline timeout_current_session_if_needed(stats: Stats) : unit =
            let now = Timestamp.now()
            let current_session_is_empty = stats.CurrentSession.NotesHit = 0
            let current_session_has_timed_out = now - SESSION_TIMEOUT > stats.CurrentSession.LastPlay
            
            if current_session_has_timed_out || current_session_is_empty then
                stats.EndCurrentSession(now)
            elif now < stats.CurrentSession.LastTime then
                Logging.Error "System clock changes could break your session stats"
                stats.EndCurrentSession(now)
            
        let user_database = library.UserData.Database
        let stats = load_from_database(user_database)
        stats.Migrate(library)
        timeout_current_session_if_needed(stats)
        stats

    member private this.EndCurrentSession(now: int64) : unit =
        
        let inline add_to_previous_sessions(session: Session) : unit =
            let date = timestamp_to_rg_calendar_day session.Start |> DateOnly.FromDateTime
            match this.PreviousSessions.TryFind date with
            | Some sessions -> this.PreviousSessions <- this.PreviousSessions.Add (date, session :: sessions)
            | None -> this.PreviousSessions <- this.PreviousSessions.Add (date, [session])
        
        this.TotalStats <- this.TotalStats.AddSession(this.CurrentSession)
        if this.CurrentSession.NotesHit > 0 then
            let database_session : Session = this.CurrentSession.ToSession()
            add_to_previous_sessions(database_session)
            DbSessions.save database_session this.Database

        this.CurrentSession <- CurrentSession.StartNew now
        this.Save()
        
    member this.SaveCurrentSession(now: int64) : unit =
        this.CurrentSession.LastTime <- now
        if now - SESSION_TIMEOUT > this.CurrentSession.LastPlay then
            this.EndCurrentSession(now)
        elif now < this.CurrentSession.LastTime then
            Logging.Error "System clock changes could break your session stats"
            this.EndCurrentSession(now)
        else
            this.Save()
            
    member this.HandleQuit() : SessionXPGain =
        this.CurrentSession.SessionScore <- this.CurrentSession.SessionScore + QUIT_PENALTY |> max 0L
        this.SaveCurrentSession(Timestamp.now())
        {
            QuitPenalty = QUIT_PENALTY
            BaseXP = 0L
            LampXP = 0L
            AccXP = 0L
        }
        
    member this.HandleScore(score_info: ScoreInfo, improvement: ImprovementFlags) : SessionXPGain =
        if score_info.TimePlayed - int64 (score_info.ChartMeta.Length / score_info.Rate) - STREAK_TIMEOUT > this.CurrentSession.LastPlay then
            this.CurrentSession.Streak <- this.CurrentSession.Streak + 1
        else
            this.CurrentSession.Streak <- 1
        this.CurrentSession.LastPlay <- max this.CurrentSession.LastPlay score_info.TimePlayed

        let base_xp = score_info.Scoring.JudgementCounts |> Array.truncate 5 |> Array.sum // todo: actually count notes hit
        let streak_bonus = float32 (this.CurrentSession.Streak - 1) * 0.1f |> max 0.0f |> min 1.0f

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
        this.CurrentSession.SessionScore <- this.CurrentSession.SessionScore + base_xp

        let lamp_xp = lamp_bonus_flat + int64 (float32 base_xp * lamp_bonus_mult)
        this.CurrentSession.SessionScore <- this.CurrentSession.SessionScore + lamp_xp

        let acc_xp = acc_bonus_flat + int64 (float32 base_xp * acc_bonus_mult)
        this.CurrentSession.SessionScore <- this.CurrentSession.SessionScore + acc_xp

        this.SaveCurrentSession(Timestamp.now())

        {
            QuitPenalty = 0L
            BaseXP = base_xp // todo: separate into base + streak
            LampXP = lamp_xp
            AccXP = acc_xp
        }
        
    member this.PlayTime : float = this.TotalStats.PlayTime + this.CurrentSession.PlayTime
    member this.PracticeTime : float = this.TotalStats.PracticeTime + this.CurrentSession.PracticeTime
    member this.GameTime : float = this.TotalStats.GameTime + this.CurrentSession.GameTime
    member this.NotesHit : int = this.TotalStats.NotesHit + this.CurrentSession.NotesHit
    member this.PlaysStarted : int = this.TotalStats.PlaysStarted + this.CurrentSession.PlaysStarted
    member this.PlaysRetried : int = this.TotalStats.PlaysRetried + this.CurrentSession.PlaysRetried
    member this.PlaysCompleted : int = this.TotalStats.PlaysCompleted + this.CurrentSession.PlaysCompleted
    member this.PlaysQuit : int = this.TotalStats.PlaysQuit + this.CurrentSession.PlaysQuit