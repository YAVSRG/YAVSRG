namespace Prelude.Data.User

type Session =
    {
        Start: int64
        End: int64

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