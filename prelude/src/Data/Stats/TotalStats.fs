namespace Prelude.Data.User.Stats

open Percyqaz.Data

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