namespace Prelude.Data.User.Stats

open Percyqaz.Common
open Percyqaz.Data
open Prelude.Data.User

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
        mutable KeymodePlaytime: Map<int, float>
    }

    member this.ToSession() : Session =
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
            KeymodePlaytime = this.KeymodePlaytime
        }

    static member StartNew(now: int64) : CurrentSession =
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
        KeymodePlaytime: Map<int, float>
    }

    static member Default: TotalStats =
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
            KeymodePlaytime = Map.empty
        }

    member this.AddSession(session: CurrentSession) : TotalStats =
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
            KeymodePlaytime = add_playtimes this.KeymodePlaytime session.KeymodePlaytime
        }

[<Json.AutoCodec(false)>]
type StatsSaveData =
    {
        TotalStats: TotalStats
        CurrentSession: CurrentSession
        BoundNetworkId: int64 option
        Migrations: Set<string>
    }
    static member Default: StatsSaveData = { TotalStats = TotalStats.Default; CurrentSession = CurrentSession.StartNew(-1L); Migrations = Set.empty; BoundNetworkId = None }