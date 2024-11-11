namespace Prelude.Data.User

open System
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Gameplay

[<Json.AutoCodec(false)>]
type CurrentSession =
    {
        mutable Start: int64
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
    }

    static member StartNew =
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
        }

    static member Default =
        {
            Start = 0L
            LastPlay = 0L
            LastTime = 0L

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
        }

[<Json.AutoCodec(false)>]
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

        SessionScore: int64
        // todo: skillset improvement
        // todo: snapshot of skillset values at end of session
    }

open Prelude.Data.Library

module Stats =

    let SESSION_TIMEOUT = 2L * 60L * 60L * 1000L // 2 hours
    let STREAK_TIMEOUT = 60L * 1000L // 1 minute

    let mutable TOTAL_STATS : TotalStats = Unchecked.defaultof<_>
    let mutable CURRENT_SESSION : CurrentSession = Unchecked.defaultof<_>
    let mutable PREVIOUS_SESSIONS : Map<System.DateOnly, Session list> = Map.empty

    let private end_current_session (database: UserDatabase) =
        TOTAL_STATS <-
            {
                PlayTime = TOTAL_STATS.PlayTime + CURRENT_SESSION.PlayTime
                PracticeTime = TOTAL_STATS.PracticeTime + CURRENT_SESSION.PracticeTime
                GameTime = TOTAL_STATS.GameTime + CURRENT_SESSION.GameTime
                NotesHit = TOTAL_STATS.NotesHit + CURRENT_SESSION.NotesHit

                PlaysStarted = TOTAL_STATS.PlaysStarted + CURRENT_SESSION.PlaysStarted
                PlaysRetried = TOTAL_STATS.PlaysRetried + CURRENT_SESSION.PlaysRetried
                PlaysCompleted = TOTAL_STATS.PlaysCompleted + CURRENT_SESSION.PlaysCompleted
                PlaysQuit = TOTAL_STATS.PlaysQuit + CURRENT_SESSION.PlaysQuit

                XP = TOTAL_STATS.XP + CURRENT_SESSION.SessionScore
                KeymodeSkills = TOTAL_STATS.KeymodeSkills
            }
        if CURRENT_SESSION.PlaysStarted > 0 then
            let session : Session =
                {
                    Start = CURRENT_SESSION.Start
                    End = CURRENT_SESSION.LastTime

                    PlayTime = CURRENT_SESSION.PlayTime
                    PracticeTime = CURRENT_SESSION.PracticeTime
                    GameTime = CURRENT_SESSION.GameTime
                    NotesHit = CURRENT_SESSION.NotesHit

                    PlaysStarted = CURRENT_SESSION.PlaysStarted
                    PlaysRetried = CURRENT_SESSION.PlaysRetried
                    PlaysCompleted = CURRENT_SESSION.PlaysCompleted
                    PlaysQuit = CURRENT_SESSION.PlaysQuit

                    SessionScore = CURRENT_SESSION.SessionScore
                }
            let date = timestamp_to_local_day session.Start |> DateOnly.FromDateTime
            match PREVIOUS_SESSIONS.TryFind date with
            | Some sessions ->
                PREVIOUS_SESSIONS <- PREVIOUS_SESSIONS.Add (date, session :: sessions)
            | None ->
                PREVIOUS_SESSIONS <- PREVIOUS_SESSIONS.Add (date, [session])
            // todo: save to db

        CURRENT_SESSION <- CurrentSession.StartNew
        // todo: DbSingletons.save_together method for data integrity
        DbSingletons.save<TotalStats> "total_stats" TOTAL_STATS database.Database
        DbSingletons.save<CurrentSession> "current_session" CURRENT_SESSION database.Database

    let save_current_session (database: UserDatabase) =
        let now = Timestamp.now()
        CURRENT_SESSION.LastTime <- now
        if now - SESSION_TIMEOUT > CURRENT_SESSION.LastPlay then
            end_current_session database
        elif now < CURRENT_SESSION.LastTime then
            Logging.Error("System clock changes could break your session stats")
            end_current_session database
        else
            DbSingletons.save<CurrentSession> "current_session" CURRENT_SESSION database.Database

    let private calculate (library: Library) (database: UserDatabase) : Session array =
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
            | Some cc -> cc.Length / (snd scores.[0]).Rate |> int64
            | None -> 2L * 60L * 1000L

        let mutable session_start_time = (snd scores.[0]).Timestamp - first_score_length
        let mutable session_playing_time = 0.0f<ms / rate>
        let mutable last_time = session_start_time
        let mutable score_count = 0

        seq {
            for chart_id, score in scores do

                let score_length = 
                    match ChartDatabase.get_meta chart_id library.Charts with
                    | Some cc -> 
                        session_playing_time <- session_playing_time + cc.Length / score.Rate
                        cc.Length / score.Rate |> int64
                    | None -> 2L * 60L * 1000L

                if score.Timestamp - last_time > SESSION_TIMEOUT then
                    // start of new session
                    yield { 
                        Start = session_start_time
                        End = last_time

                        PlayTime = session_playing_time |> float
                        PracticeTime = 0.0
                        GameTime = float (last_time - session_start_time)
                        NotesHit = 0
                        PlaysStarted = score_count
                        PlaysCompleted = score_count
                        PlaysQuit = 0
                        PlaysRetried = 0

                        SessionScore = 0
                    }
                    session_start_time <- score.Timestamp - score_length
                    session_playing_time <- 0.0f<ms / rate>
                    score_count <- 1
                else
                    score_count <- score_count + 1
                last_time <- score.Timestamp

            yield { 
                Start = session_start_time
                End = last_time

                PlayTime = session_playing_time |> float
                PracticeTime = 0.0
                GameTime = float (last_time - session_start_time)
                NotesHit = 0
                PlaysStarted = score_count
                PlaysCompleted = score_count
                PlaysQuit = 0
                PlaysRetried = 0

                SessionScore = 0
            }
        }
        |> Seq.toArray

    type SessionScoreIncrease =
        {
            QuitterPenalty: int64
            BaseXP: int64
            LampXP: int64
            AccXP: int64
            SkillXP: int64
        }

    let QUIT_PENALTY = 100L
    let quitter_penalty (database: UserDatabase) =
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore - QUIT_PENALTY |> max 0L
        save_current_session database

    let handle_score (score_info: ScoreInfo) (improvement: ImprovementFlags) (database: UserDatabase) =
        if score_info.TimePlayed - int64 (score_info.ChartMeta.Length / score_info.Rate) - STREAK_TIMEOUT > CURRENT_SESSION.LastPlay then
            CURRENT_SESSION.Streak <- CURRENT_SESSION.Streak + 1
        else
            CURRENT_SESSION.Streak <- 1
        CURRENT_SESSION.LastPlay <- max CURRENT_SESSION.LastPlay score_info.TimePlayed

        let base_xp = score_info.Scoring.JudgementCounts |> Array.truncate 5 |> Array.sum
        let streak_bonus = float32 (CURRENT_SESSION.Streak - 1) * 0.1f |> max 0.0f |> min 1.0f

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
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + base_xp

        let lamp_xp = lamp_bonus_flat + int64 (float32 base_xp * lamp_bonus_mult)
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + lamp_xp

        let acc_xp = acc_bonus_flat + int64 (float32 base_xp * acc_bonus_mult)
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + acc_xp

        let skill_up = 
            KeymodeSkillBreakdown.score 
                score_info.ChartMeta.Patterns
                score_info.Accuracy
                score_info.Rate
                TOTAL_STATS.KeymodeSkills.[score_info.WithMods.Keys - 3]

        save_current_session database
        DbSingletons.save<TotalStats> "total_stats" TOTAL_STATS database.Database

        printfn "%O" skill_up
        // todo: calculate session-level skill improvement
        // todo: xp for session-level skill improvement
        {
            QuitterPenalty = 0
            BaseXP = base_xp
            LampXP = lamp_xp
            AccXP = acc_xp
            SkillXP = 0
        }

    let init (library: Library) (database: UserDatabase) =

        TOTAL_STATS <- DbSingletons.get_or_default "total_stats" TotalStats.Default database.Database
        CURRENT_SESSION <- DbSingletons.get_or_default "current_session" CurrentSession.StartNew database.Database
        PREVIOUS_SESSIONS <-
            calculate library database
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_local_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        
        let now = Timestamp.now()
        if Timestamp.now() - SESSION_TIMEOUT > CURRENT_SESSION.LastPlay then
            end_current_session database
        elif now < CURRENT_SESSION.LastTime then
            Logging.Error("System clock changes could break your session stats")
            end_current_session database

        if TOTAL_STATS.PlayTime = 0.0 then
            let legacy_stats = load_important_json_file "Stats" (System.IO.Path.Combine(get_game_folder "Data", "stats.json")) false
            TOTAL_STATS <-
                {
                    PlayTime = TOTAL_STATS.PlayTime + legacy_stats.PlayTime
                    PracticeTime = TOTAL_STATS.PracticeTime + legacy_stats.PracticeTime
                    GameTime = TOTAL_STATS.GameTime + legacy_stats.GameTime
                    NotesHit = TOTAL_STATS.NotesHit + legacy_stats.NotesHit

                    PlaysStarted = TOTAL_STATS.PlaysStarted + legacy_stats.PlaysStarted
                    PlaysRetried = TOTAL_STATS.PlaysRetried + legacy_stats.PlaysRetried
                    PlaysCompleted = TOTAL_STATS.PlaysCompleted + legacy_stats.PlaysCompleted
                    PlaysQuit = TOTAL_STATS.PlaysQuit + legacy_stats.PlaysQuit

                    XP = TOTAL_STATS.XP
                    KeymodeSkills = TOTAL_STATS.KeymodeSkills
                }