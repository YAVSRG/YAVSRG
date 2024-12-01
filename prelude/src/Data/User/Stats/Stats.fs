namespace Prelude.Data.User

open System
open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Rulesets

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
        KeymodeSkills: KeymodeSkillBreakdown array
    }

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

open Prelude.Data.Library

[<Json.AutoCodec>]
type Stats =
    {
        TotalStats: TotalStats
        CurrentSession: CurrentSession
    }
    static member Default = { TotalStats = TotalStats.Default; CurrentSession = CurrentSession.Default }

type SessionXPGain =
    {
        QuitPenalty: int64
        BaseXP: int64
        LampXP: int64
        AccXP: int64
        SkillXP: int64
    }
    member this.Total = this.QuitPenalty + this.BaseXP + this.LampXP + this.AccXP + this.SkillXP

module Stats =

    let SESSION_TIMEOUT = 2L * 60L * 60L * 1000L // 2 hours
    let STREAK_TIMEOUT = 60L * 1000L // 1 minute

    let mutable TOTAL_STATS : TotalStats = Unchecked.defaultof<_>
    let mutable CURRENT_SESSION : CurrentSession = Unchecked.defaultof<_>
    let mutable PREVIOUS_SESSIONS : Map<DateOnly, Session list> = Map.empty

    let private end_current_session (now: int64) (database: UserDatabase) =
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
        if CURRENT_SESSION.NotesHit > 0 then
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

                    XP = CURRENT_SESSION.SessionScore
                    KeymodeSkills = CURRENT_SESSION.KeymodeSkills |> Array.map _.Tiny
                }
            let date = timestamp_to_local_day session.Start |> DateOnly.FromDateTime
            match PREVIOUS_SESSIONS.TryFind date with
            | Some sessions ->
                PREVIOUS_SESSIONS <- PREVIOUS_SESSIONS.Add (date, session :: sessions)
            | None ->
                PREVIOUS_SESSIONS <- PREVIOUS_SESSIONS.Add (date, [session])
            DbSessions.save session database.Database

        CURRENT_SESSION <- CurrentSession.StartNew now CURRENT_SESSION.LastTime CURRENT_SESSION.KeymodeSkills
        DbSingletons.save<Stats> "stats" { TotalStats = TOTAL_STATS; CurrentSession = CURRENT_SESSION } database.Database

    let save_current_session (now: int64) (database: UserDatabase) =
        CURRENT_SESSION.LastTime <- now
        if now - SESSION_TIMEOUT > CURRENT_SESSION.LastPlay then
            end_current_session now database
        elif now < CURRENT_SESSION.LastTime then
            Logging.Error("System clock changes could break your session stats")
            end_current_session now database
        else
            DbSingletons.save<Stats> "stats" { TotalStats = TOTAL_STATS; CurrentSession = CURRENT_SESSION } database.Database

    let QUIT_PENALTY = -100L
    let quitter_penalty (database: UserDatabase) =
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + QUIT_PENALTY |> max 0L
        save_current_session (Timestamp.now()) database
        {
            QuitPenalty = QUIT_PENALTY
            BaseXP = 0L
            LampXP = 0L
            AccXP = 0L
            SkillXP = 0L
        }

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

        let all_time_skill_up = 
            KeymodeSkillBreakdown.score
                score_info.ChartMeta.Patterns
                score_info.Accuracy
                score_info.Rate
                TOTAL_STATS.KeymodeSkills.[score_info.WithMods.Keys - 3]

        let session_skill_up = 
            KeymodeSkillBreakdown.score
                score_info.ChartMeta.Patterns
                score_info.Accuracy
                score_info.Rate
                CURRENT_SESSION.KeymodeSkills.[score_info.WithMods.Keys - 3]

        save_current_session (Timestamp.now()) database
        
        let skill_xp_mult = (float32 TOTAL_STATS.XP / 100_000f - 1.0f) |> max 0.0f |> min 1.0f
        let skill_xp =
            int64 (session_skill_up.Total * skill_xp_mult) 
            + int64 (all_time_skill_up.Total * 10.0f * skill_xp_mult)
        CURRENT_SESSION.SessionScore <- CURRENT_SESSION.SessionScore + skill_xp

        {
            QuitPenalty = 0L
            BaseXP = base_xp // todo: separate into base + streak
            LampXP = lamp_xp
            AccXP = acc_xp
            SkillXP = skill_xp // todo: separate into session/all time
        }

    let private calculate (library: Library) (database: UserDatabase) : Session array * KeymodeSkillBreakdown array =
        
        // calculate skillsets
        let sc_j4 = SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        for cc_key in library.Charts.Cache.Keys do
            let cc = library.Charts.Cache.[cc_key]

            let data = UserDatabase.get_chart_data cc.Hash database
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                for (acc, rate, _) in pbs.Accuracy do
                    KeymodeSkillBreakdown.score cc.Patterns acc rate TOTAL_STATS.KeymodeSkills.[cc.Keys - 3] |> ignore
            | None -> ()

        // calculate session
        let scores =
            seq {
                for chart_id in database.Cache.Keys do
                    for score in database.Cache.[chart_id].Scores do
                        yield chart_id, score
            }
            |> Seq.sortBy (snd >> _.Timestamp)
            |> Array.ofSeq

        let first_score_length = 
            if scores.Length = 0 then 0L else
            match ChartDatabase.get_meta (fst scores.[0]) library.Charts with
            | Some cc -> cc.Length / (snd scores.[0]).Rate |> int64
            | None -> 2L * 60L * 1000L

        let mutable session_start_time = if scores.Length = 0 then 0L else (snd scores.[0]).Timestamp - first_score_length
        let mutable session_playing_time = 0.0f<ms / rate>
        let mutable last_time = session_start_time
        let mutable score_count = 0
        let skills = Array.init 8 (fun _ -> KeymodeSkillBreakdown.Default)

        seq {
            for chart_id, score in scores do

                let score_length = 
                    match ChartDatabase.get_meta chart_id library.Charts with
                    | Some cc -> 
                        
                        let data = UserDatabase.get_chart_data cc.Hash database
                        match data.PersonalBests.TryFind(sc_j4_id) with
                        | Some pbs ->
                            match PersonalBests.get_best_above score.Rate pbs.Accuracy with
                            | Some (acc, rate, _) ->
                                KeymodeSkillBreakdown.score cc.Patterns acc score.Rate skills.[cc.Keys - 3] |> ignore
                            | None -> ()
                        | None -> ()

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

                        XP = 0
                        KeymodeSkills = skills |> Array.map _.Tiny
                    }
                    session_start_time <- score.Timestamp - score_length
                    session_playing_time <- 0.0f<ms / rate>
                    score_count <- 1
                    let d = KeymodeSkillBreakdown.decay_over_time last_time session_start_time
                    for i = 0 to skills.Length - 1 do
                        skills.[i] <- skills.[i].Scale d
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
                KeymodeSkills = skills |> Array.map _.Tiny

                XP = 0
            }
            let d = KeymodeSkillBreakdown.decay_over_time last_time (Timestamp.now())
            for i = 0 to skills.Length - 1 do
                skills.[i] <- skills.[i].Scale d
        }
        |> Seq.toArray,
        skills

    let init_startup (library: Library) (database: UserDatabase) =

        let sessions = DbSessions.get_all database.Database
        PREVIOUS_SESSIONS <- 
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_local_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        let stats : Stats = DbSingletons.get_or_default "stats" Stats.Default database.Database
        TOTAL_STATS <- stats.TotalStats
        CURRENT_SESSION <- stats.CurrentSession

        if PREVIOUS_SESSIONS = Map.empty && TOTAL_STATS.GameTime = 0.0 then

            Logging.Info("Backfilling session data from score history...")

            TOTAL_STATS <- TotalStats.Default
            CURRENT_SESSION <- CurrentSession.Default

            let sessions, current_skills = calculate library database
            DbSessions.save_batch sessions database.Database
            PREVIOUS_SESSIONS <- 
                sessions
                |> Seq.groupBy (fun session -> session.Start |> timestamp_to_local_day |> DateOnly.FromDateTime)
                |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
                |> Map.ofSeq
            CURRENT_SESSION <- { CURRENT_SESSION with KeymodeSkills = current_skills }

            let legacy_stats = load_important_json_file "Stats" (System.IO.Path.Combine(get_game_folder "Data", "stats.json")) false
            TOTAL_STATS <- { legacy_stats with XP = legacy_stats.NotesHit; KeymodeSkills = TOTAL_STATS.KeymodeSkills }
        
        let now = Timestamp.now()
        if CURRENT_SESSION.NotesHit = 0 then
            end_current_session now database
        if Timestamp.now() - SESSION_TIMEOUT > CURRENT_SESSION.LastPlay then
            end_current_session now database
        elif now < CURRENT_SESSION.LastTime then
            Logging.Error("System clock changes could break your session stats")
            end_current_session now database

    let format_long_time (time: float) =
        let seconds = time / 1000.0
        let minutes = seconds / 60.0
        let hours = minutes / 60.0
        let days = hours / 24.0

        if days > 1 then
            sprintf "%id %02ih %02im" (floor days |> int) (floor (hours % 24.0) |> int) (floor (minutes % 60.0) |> int)
        elif hours > 1 then
            sprintf "%ih %02im" (floor hours |> int) (floor (minutes % 60.0) |> int)
        else
            sprintf "%im %02is" (floor minutes |> int) (floor (seconds % 60.0) |> int)

    let format_short_time (time: float) =
        let seconds = time / 1000.0
        let minutes = seconds / 60.0
        let hours = minutes / 60.0

        if hours > 1 then
            sprintf "%i:%02i:%02i" (floor hours |> int) (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)
        else
            sprintf "%02i:%02i" (floor (minutes % 60.0) |> int) (floor (seconds % 60.0) |> int)

    let current_level (xp: int64) =
        (float xp / 999.0 |> sqrt |> floor |> int) + 1

    let xp_for_level (level: int) =
        int64 (level - 1) * int64 (level - 1) * 999L