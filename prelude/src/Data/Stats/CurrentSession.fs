namespace Prelude.Data.User.Stats

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
    
    member this.HasTimedOut(now: int64) : bool = now - SESSION_TIMEOUT > this.LastTime
    member this.IsEmpty() : bool = this.NotesHit = 0

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
        
    static member Default = CurrentSession.StartNew(-1L)