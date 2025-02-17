namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Percyqaz.Data
open Prelude.Calculator
open Prelude.Data.User

type Session = Prelude.Data.User.Session

// todo: make properties internal
[<Json.AutoCodec(false)>]
type CurrentSession =
    {
        Start: int64
        mutable LastPlay: int64
        mutable LastTime: int64

        mutable PlayTime: float
        mutable PracticeTime: float
        mutable GameTime: float
        mutable NotesHit: int

        mutable PlaysStarted: int
        mutable PlaysRetried: int
        mutable PlaysCompleted: int
        mutable PlaysQuit: int

        mutable SessionScore: int64
        mutable Streak: int
        KeymodeSkills: KeymodeSkillBreakdown array
        mutable KeymodePlaytime: Map<int, float>
    }

    member this.ToSession: Session =
        assert(this.NotesHit > 0)
        {
            Start = this.Start
            End = this.LastTime

            PlayTime = this.PlayTime
            PracticeTime = this.PracticeTime
            GameTime = this.GameTime
            NotesHit = this.NotesHit

            PlaysStarted = this.PlaysStarted
            PlaysRetried = this.PlaysRetried
            PlaysCompleted = this.PlaysCompleted
            PlaysQuit = this.PlaysQuit

            XP = this.SessionScore
            KeymodeSkills = this.KeymodeSkills |> Array.map _.Tiny
            KeymodePlaytime = this.KeymodePlaytime
        }

    member this.AddPlaytime (keymode: int) (time: float) =
        this.PlayTime <- this.PlayTime + time
        this.KeymodePlaytime <- this.KeymodePlaytime.Change(keymode, fun v -> (Option.defaultValue 0.0 v) + time |> Some)

    static member StartNew (now: int64) (end_of_last_session: int64) (previous_skills: KeymodeSkillBreakdown array) =
        {
            Start = now
            LastPlay = now
            LastTime = now

            PlayTime = 0.0
            PracticeTime = 0.0
            GameTime = 0.0
            NotesHit = 0

            PlaysStarted = 0
            PlaysRetried = 0
            PlaysCompleted = 0
            PlaysQuit = 0

            SessionScore = 0L
            Streak = 0
            KeymodeSkills =
                let d = KeymodeSkillBreakdown.decay_over_time end_of_last_session now
                previous_skills |> Array.map (fun k -> k.Scale d)
            KeymodePlaytime = Map.empty
        }

    static member Default =
        let now = Timestamp.now()
        {
            Start = now
            LastPlay = now
            LastTime = now

            PlayTime = 0.0
            PracticeTime = 0.0
            GameTime = 0.0
            NotesHit = 0

            PlaysStarted = 0
            PlaysRetried = 0
            PlaysCompleted = 0
            PlaysQuit = 0

            SessionScore = 0L
            Streak = 0
            KeymodeSkills = Array.init 8 (fun _ -> KeymodeSkillBreakdown.Default)
            KeymodePlaytime = Map.empty
        }

[<Json.AutoCodec(false)>]
type TotalStats =
    {
        PlayTime: float
        PracticeTime: float
        GameTime: float
        NotesHit: int

        PlaysStarted: int
        PlaysRetried: int
        PlaysCompleted: int
        PlaysQuit: int

        XP: int64
        KeymodeSkills: KeymodeSkillBreakdown array
        KeymodePlaytime: Map<int, float>
    }

    static member Default =
        {
            PlayTime = 0.0
            PracticeTime = 0.0
            GameTime = 0.0
            NotesHit = 0
            PlaysStarted = 0
            PlaysRetried = 0
            PlaysCompleted = 0
            PlaysQuit = 0

            XP = 0L
            KeymodeSkills = Array.init 8 (fun _ -> KeymodeSkillBreakdown.Default)
            KeymodePlaytime = Map.empty
        }

    member this.AddSession (session: CurrentSession) =
        {
            PlayTime = this.PlayTime + session.PlayTime
            PracticeTime = this.PracticeTime + session.PracticeTime
            GameTime = this.GameTime + session.GameTime
            NotesHit = this.NotesHit + session.NotesHit

            PlaysStarted = this.PlaysStarted + session.PlaysStarted
            PlaysRetried = this.PlaysRetried + session.PlaysRetried
            PlaysCompleted = this.PlaysCompleted + session.PlaysCompleted
            PlaysQuit = this.PlaysQuit + session.PlaysQuit

            XP = this.XP + session.SessionScore
            KeymodeSkills = this.KeymodeSkills
            KeymodePlaytime = add_playtimes this.KeymodePlaytime session.KeymodePlaytime
        }

[<Json.AutoCodec(false)>]
type StatsSaveData =
    {
        TotalStats: TotalStats
        CurrentSession: CurrentSession
        Migrations: Set<string>
        BoundNetworkId: int64 option
    }
    static member Default = { TotalStats = TotalStats.Default; CurrentSession = CurrentSession.Default; Migrations = Set.empty; BoundNetworkId = None }

[<AutoOpen>]
module StatsState =

    let SESSION_TIMEOUT = 2L * 60L * 60L * 1000L // 2 hours
    let STREAK_TIMEOUT = 60L * 1000L // 1 minute

    let mutable internal MIGRATIONS : Set<string> = Set.empty
    let mutable TOTAL_STATS : TotalStats = Unchecked.defaultof<_>
    let mutable CURRENT_SESSION : CurrentSession = Unchecked.defaultof<_>
    let mutable PREVIOUS_SESSIONS : Map<DateOnly, Session list> = Map.empty
    let mutable BOUND_NETWORK_ID = None

    let internal backup_stats (id: string) (database: UserDatabase) =
        DbSingletons.save<StatsSaveData> ("stats_" + id) { TotalStats = TOTAL_STATS; CurrentSession = CURRENT_SESSION; Migrations = MIGRATIONS; BoundNetworkId = BOUND_NETWORK_ID } database.Database

    let internal save_stats (database: UserDatabase) =
        DbSingletons.save<StatsSaveData> "stats" { TotalStats = TOTAL_STATS; CurrentSession = CURRENT_SESSION; Migrations = MIGRATIONS; BoundNetworkId = BOUND_NETWORK_ID } database.Database

    let internal load_stats (database: UserDatabase) =
        let stats : StatsSaveData = DbSingletons.get_or_default "stats" StatsSaveData.Default database.Database
        TOTAL_STATS <- stats.TotalStats
        CURRENT_SESSION <- stats.CurrentSession
        MIGRATIONS <- stats.Migrations
        BOUND_NETWORK_ID <- stats.BoundNetworkId