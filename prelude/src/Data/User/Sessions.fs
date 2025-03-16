namespace Prelude.Data.User

open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Calculator

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
        KeymodeSkills: KeymodeTinyBreakdown array
        KeymodePlaytime: Map<int, float>
    }

module DbSessions =

    let internal TABLE: TableCommandHelper =
        {
            Name = "sessions"
            PrimaryKey = Column.Integer("Id").Unique
            Columns =
                [
                    Column.Integer("Start").Unique
                    Column.Integer("End")

                    Column.Real("PlayTime")
                    Column.Real("PracticeTime")
                    Column.Real("GameTime")
                    Column.Integer("NotesHit")

                    Column.Integer("PlaysStarted")
                    Column.Integer("PlaysRetried")
                    Column.Integer("PlaysCompleted")
                    Column.Integer("PlaysQuit")

                    Column.Integer("XP")
                    Column.Text("KeymodeSkills")
                ]
        }
    let internal ADD_KEYMODE_PLAYTIME: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            ALTER TABLE sessions
            ADD COLUMN KeymodePlaytime TEXT NOT NULL DEFAULT '[]'
            """
        }

    let internal CREATE_INDEX: NonQuery<unit> =
        { NonQuery.without_parameters () with
            SQL =
                """
            CREATE INDEX idx_sessions_start
            ON sessions (Start ASC);
            """
        }

    let private GET_ALL: Query<unit, Session> =
        {
            SQL =
                """SELECT Start, End, PlayTime, PracticeTime, GameTime, NotesHit, PlaysStarted, PlaysRetried, PlaysCompleted, PlaysQuit, XP, KeymodeSkills, KeymodePlaytime FROM sessions ORDER BY Start ASC;"""
            Parameters = []
            FillParameters = fun _ _ -> ()
            Read = fun r ->
                {
                    Start = r.Int64
                    End = r.Int64

                    PlayTime = r.Float64
                    PracticeTime = r.Float64
                    GameTime = r.Float64
                    NotesHit = r.Int32

                    PlaysStarted = r.Int32
                    PlaysRetried = r.Int32
                    PlaysCompleted = r.Int32
                    PlaysQuit = r.Int32

                    XP = r.Int64
                    KeymodeSkills = r.Json JSON
                    KeymodePlaytime = r.Json JSON
                }
        }

    let get_all (db: Database) : Session array =
        GET_ALL.Execute () db
        |> expect

    let private SAVE: NonQuery<Session> =
        {
            SQL =
                """
                INSERT OR REPLACE INTO sessions (Start, End, PlayTime, PracticeTime, GameTime, NotesHit, PlaysStarted, PlaysRetried, PlaysCompleted, PlaysQuit, XP, KeymodeSkills, KeymodePlaytime)
                VALUES (@Start, @End, @PlayTime, @PracticeTime, @GameTime, @NotesHit, @PlaysStarted, @PlaysRetried, @PlaysCompleted, @PlaysQuit, @XP, json(@KeymodeSkills), json(@KeymodePlaytime));
            """
            Parameters = [
                "@Start", SqliteType.Integer, 8;
                "@End", SqliteType.Integer, 8;

                "@PlayTime", SqliteType.Real, 8;
                "@PracticeTime", SqliteType.Real, 8;
                "@GameTime", SqliteType.Real, 8;
                "@NotesHit", SqliteType.Integer, 4;

                "@PlaysStarted", SqliteType.Integer, 4;
                "@PlaysRetried", SqliteType.Integer, 4;
                "@PlaysCompleted", SqliteType.Integer, 4;
                "@PlaysQuit", SqliteType.Integer, 4;

                "@XP", SqliteType.Integer, 8;
                "@KeymodeSkills", SqliteType.Text, -1
                "@KeymodePlaytime", SqliteType.Text, -1
            ]
            FillParameters = fun p session ->
                p.Int64 session.Start
                p.Int64 session.End

                p.Float64 session.PlayTime
                p.Float64 session.PracticeTime
                p.Float64 session.GameTime
                p.Int32 session.NotesHit

                p.Int32 session.PlaysStarted
                p.Int32 session.PlaysRetried
                p.Int32 session.PlaysCompleted
                p.Int32 session.PlaysQuit

                p.Int64 session.XP
                p.Json JSON session.KeymodeSkills
                p.Json JSON session.KeymodePlaytime
        }

    let save (session: Session) (db: Database) =
        SAVE.Execute session db |> expect |> ignore

    let save_batch (sessions: Session seq) (db: Database) =
        SAVE.Batch sessions db |> expect |> ignore