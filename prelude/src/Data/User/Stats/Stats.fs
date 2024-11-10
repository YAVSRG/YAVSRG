namespace Prelude.Data.User

open Percyqaz.Common
open Percyqaz.Data

[<Json.AutoCodec(false)>]
type CurrentSession =
    {
        mutable Start: int64 option

        mutable PlayTime: float
        mutable PracticeTime: float
        mutable GameTime: float
        mutable NotesHit: int

        mutable PlaysStarted: int
        mutable PlaysRetried: int
        mutable PlaysCompleted: int
        mutable PlaysQuit: int

        mutable SessionScore: int
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

        XP: int
    }

[<Json.AutoCodec(false)>]
type ArchivedSession =
    {
        Start: int64
        End: int64
        Keymode: int

        PlayTime: float
        PracticeTime: float
        GameTime: float
        NotesHit: int

        PlaysStarted: int
        PlaysRetried: int
        PlaysCompleted: int
        PlaysQuit: int

        // todo: skillset improvement
        // todo: snapshot of skillset values at end of session
    }

open Prelude.Data.Library

module Stats =

    let calculate (library: Library) (database: UserDatabase) =
        let scores =
            seq {
                for chart_id in database.Cache.Keys do
                    for score in database.Cache.[chart_id].Scores do
                        yield chart_id, score
            }
            |> Seq.sortBy (snd >> _.Timestamp)
            |> Array.ofSeq

        let first_score_length = 
            match ChartDatabase.get_meta (fst scores.[0]) library.Charts with
            | Some cc -> cc.Length |> int64
            | None -> 2L * 60L * 1000L

        let mutable session_start_time = (snd scores.[0]).Timestamp - first_score_length
        let mutable last_time = session_start_time
        let mutable score_count = 0

        for chart_id, score in scores do
            let score_length = 
                match ChartDatabase.get_meta chart_id library.Charts with
                | Some cc -> cc.Length |> int64
                | None -> 2L * 60L * 1000L
            if score.Timestamp - last_time > 7200_000L then
                // start of new session
                printfn "Session: %O with %i scores lasting %O" (Timestamp.to_datetime(session_start_time)) score_count (System.TimeSpan.FromMilliseconds(last_time - session_start_time |> float))
                session_start_time <- score.Timestamp - score_length
                score_count <- 1
            else
                score_count <- score_count + 1
            last_time <- score.Timestamp

        printfn "Current session: %O with %i scores" (Timestamp.to_datetime(session_start_time)) score_count