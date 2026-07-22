namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Percyqaz.Data
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
    member this.Total : int64 =
        this.QuitPenalty + this.BaseXP + this.LampXP + this.AccXP
    
[<Json.AutoCodec(false)>]
type StatsSaveData =
    {
        TotalStats: TotalStats
        CurrentSession: CurrentSession
        BoundNetworkId: int64 option
        Migrations: Set<string>
    }
    static member Default: StatsSaveData = { TotalStats = TotalStats.Default; CurrentSession = CurrentSession.Default; Migrations = Set.empty; BoundNetworkId = None }

type Stats =
    private {
        Database: Database
        Clock: Clock
        mutable TotalStats : TotalStats
        mutable CurrentSession : CurrentSession
        DateOfEarliestSession: DateOnly
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
    member this.Save() : unit = this.SaveAs("stats")
        
    static member FromUserDatabase(user_db: UserDatabase, clock: Clock) : Stats =
        
        let inline load_previous_sessions(database: Database) : Map<DateOnly, Session list> =
            DbSessions.get_all database
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
            
        let inline get_earliest_session_date(previous_sessions: Map<DateOnly, _>) : DateOnly =
            let mutable earliest =
                clock.Now() |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime
            Seq.iter (fun date -> earliest <- min date earliest) previous_sessions.Keys
            earliest
        
        let inline load_from_database(database: Database) : Stats =
            let previous_sessions = load_previous_sessions(database)
            let earliest_session_date = get_earliest_session_date(previous_sessions)
            let stats : StatsSaveData = DbSingletons.get_or_default "stats" StatsSaveData.Default database
            
            {
                Database = database
                Clock = clock
                CurrentSession = if stats.CurrentSession.LastTime < 0L then CurrentSession.StartNew(clock.Now()) else stats.CurrentSession
                DateOfEarliestSession = earliest_session_date
                PreviousSessions = previous_sessions
                TotalStats = stats.TotalStats
                BoundNetworkId = stats.BoundNetworkId
                Migrations = stats.Migrations
            }
        
        let inline timeout_current_session_if_needed(stats: Stats) : unit =
            let now = clock.Now()
            
            if stats.CurrentSession.HasTimedOut(now) || stats.CurrentSession.IsEmpty() then
                stats.EndCurrentSession(now)
            elif now < stats.CurrentSession.LastTime then
                Logging.Error "System clock changes could break your session stats"
                stats.EndCurrentSession(now)
            
        let stats = load_from_database(user_db.Database)
        timeout_current_session_if_needed(stats)
        stats
        
    static member FromUserDatabase(user_db: UserDatabase) : Stats = Stats.FromUserDatabase(user_db, SystemClock())

    member private this.EndCurrentSession(now: int64) : unit =
        
        let inline add_to_previous_sessions(session: Session) : unit =
            let date = timestamp_to_rg_calendar_day(session.Start) |> DateOnly.FromDateTime
            match this.PreviousSessions.TryFind(date) with
            | Some sessions -> this.PreviousSessions <- this.PreviousSessions.Add(date, session :: sessions)
            | None -> this.PreviousSessions <- this.PreviousSessions.Add(date, [session])
            
        let inline save_nonempty_session(current_session: CurrentSession) : unit =
            let database_session : Session = current_session.ToSession()
            add_to_previous_sessions(database_session)
            DbSessions.save database_session this.Database
            
        this.TotalStats <- this.TotalStats.AddSession(this.CurrentSession)
        if not(this.CurrentSession.IsEmpty()) then save_nonempty_session(this.CurrentSession)

        this.CurrentSession <- CurrentSession.StartNew(now)
        this.Save()
        
    member this.StartOrContinueSession() : unit =
        let now = this.Clock.Now()
        if this.CurrentSession.HasTimedOut(now) then
            this.EndCurrentSession(now)
        elif now < this.CurrentSession.LastTime then
            Logging.Error "System clock changes could break your session stats"
            this.EndCurrentSession(now)
        else
            this.CurrentSession.LastTime <- now
            this.Save()
        
    member this.PlayTime : float = this.TotalStats.PlayTime + this.CurrentSession.PlayTime
    member this.PracticeTime : float = this.TotalStats.PracticeTime + this.CurrentSession.PracticeTime
    member this.GameTime : float = this.TotalStats.GameTime + this.CurrentSession.GameTime
    member this.NotesHit : int = this.TotalStats.NotesHit + this.CurrentSession.NotesHit
    member this.PlaysStarted : int = this.TotalStats.PlaysStarted + this.CurrentSession.PlaysStarted
    member this.PlaysRetried : int = this.TotalStats.PlaysRetried + this.CurrentSession.PlaysRetried
    member this.PlaysCompleted : int = this.TotalStats.PlaysCompleted + this.CurrentSession.PlaysCompleted
    member this.PlaysQuit : int = this.TotalStats.PlaysQuit + this.CurrentSession.PlaysQuit
    member this.XP : int64 = this.TotalStats.XP + this.CurrentSession.SessionScore
        
    member private this.AddPlayStats(keymode: int, time: float, notes_hit: int) : unit =
        this.CurrentSession.PlayTime <- this.CurrentSession.PlayTime + time
        this.CurrentSession.KeymodePlaytime <- this.CurrentSession.KeymodePlaytime.Change(keymode, fun v -> (Option.defaultValue 0.0 v) + time |> Some)
        this.CurrentSession.NotesHit <- this.CurrentSession.NotesHit + notes_hit

    member this.CompletePlay(keymode: int, time: float, notes_hit: int) : unit =
        this.AddPlayStats(keymode, time, notes_hit)
        this.CurrentSession.PlaysStarted <- this.CurrentSession.PlaysStarted + 1
        this.CurrentSession.PlaysCompleted <- this.CurrentSession.PlaysCompleted + 1
        
    member this.QuitOutOfPlay(keymode: int, time: float, notes_hit: int) : unit =
        this.AddPlayStats(keymode, time, notes_hit)
        this.CurrentSession.PlaysStarted <- this.CurrentSession.PlaysStarted + 1
        this.CurrentSession.PlaysQuit <- this.CurrentSession.PlaysQuit + 1
        
    member this.RetryPlay(keymode: int, time: float, notes_hit: int) : unit =
        this.AddPlayStats(keymode, time, notes_hit)
        this.CurrentSession.PlaysRetried <- this.CurrentSession.PlaysRetried + 1
        
    member this.AddPracticeTime(time: float, notes_hit: int) : unit =
        this.CurrentSession.PracticeTime <- this.CurrentSession.PracticeTime + time
        this.CurrentSession.NotesHit <- this.CurrentSession.NotesHit + notes_hit
        
    member this.AddGameTime(time: float) : unit =
        this.CurrentSession.GameTime <- this.CurrentSession.GameTime + time
        
    member private this.AddXP(xp: int64) : unit =
        this.CurrentSession.SessionScore <- this.CurrentSession.SessionScore + xp |> max 0L
        
    member this.HandleQuit() : SessionXPGain =
        this.AddXP(QUIT_PENALTY)
        this.StartOrContinueSession()
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
        this.CurrentSession.LastTime <- max this.CurrentSession.LastPlay this.CurrentSession.LastTime

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
        let lamp_xp = lamp_bonus_flat + int64 (float32 base_xp * lamp_bonus_mult)
        let acc_xp = acc_bonus_flat + int64 (float32 base_xp * acc_bonus_mult)
        
        this.AddXP(base_xp)
        this.AddXP(lamp_xp)
        this.AddXP(acc_xp)

        this.StartOrContinueSession()

        {
            QuitPenalty = 0L
            BaseXP = base_xp // todo: separate into base + streak
            LampXP = lamp_xp
            AccXP = acc_xp
        }
    
    member this.GetPreviousSessionsForDate(date: DateOnly) : Session list =
        match Map.tryFind date this.PreviousSessions with
        | Some sessions_on_day -> sessions_on_day
        | None -> []
    
    member this.TryGetNextSession(date: DateOnly, session: Session) : (DateOnly * Session) option =
        
        let inline find_session_later_today() : (DateOnly * Session) option =
            let sessions_today = this.PreviousSessions.[date]
            let index = List.tryFindIndex (fun s -> s = session) sessions_today |> Option.defaultValue -1
            if index + 1 < sessions_today.Length then
                Some(date, sessions_today.[index + 1])
            else
                None
                
        let inline find_next_day_with_sessions() : (DateOnly * Session) option =
            let TODAY = this.Clock.Today()
            let mutable date = date
            let rec loop () : (DateOnly * Session) option =
                date <- date.AddDays(1)
                match Map.tryFind date this.PreviousSessions with
                | Some sessions_on_day ->
                    Some(date, sessions_on_day.[0])
                | None ->
                    if date = TODAY then None else loop()
            loop()
                
        find_session_later_today() |> Option.orElseWith find_next_day_with_sessions
        
    member this.TryGetEarliestSession() : (DateOnly * Session) option =
        this.TryGetNextSession(this.DateOfEarliestSession, Unchecked.defaultof<_>)
        
    member this.TryGetPreviousSession(date: DateOnly, session: Session) : (DateOnly * Session) option =
        
        let inline find_session_earlier_today() : (DateOnly * Session) option =
            let sessions_today = this.GetPreviousSessionsForDate(date)
            let index = List.tryFindIndex (fun s -> s = session) sessions_today |> Option.defaultValue -1
            if index > 0 then
                Some(date, sessions_today.[index - 1])
            else
                None
                
        let inline find_prior_day_with_sessions() : (DateOnly * Session) option =
            let mutable date = date
            let rec loop () : (DateOnly * Session) option =
                date <- date.AddDays(-1)
                match Map.tryFind date this.PreviousSessions with
                | Some sessions_on_day ->
                    Some(date, List.last sessions_on_day)
                | None ->
                    if date <= this.DateOfEarliestSession then None else loop()
            loop()
                
        find_session_earlier_today() |> Option.orElseWith find_prior_day_with_sessions
        
    member this.TryGetLatestSession() : (DateOnly * Session) option =
        this.TryGetPreviousSession(this.Clock.Today(), Unchecked.defaultof<_>)
        
    member this.GetCurrentSession() : CurrentSession = this.CurrentSession
        
    // todo: usage of this is minimised for now but ultimately this should be replaced with intent-focused methods
    // e.g. GetActiveYears(), GetSessionsByYear(), GetRecentSessions()
    member this.GetPreviousSessions() : Map<DateOnly, Session list> = this.PreviousSessions