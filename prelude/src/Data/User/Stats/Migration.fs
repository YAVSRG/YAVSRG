namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library

module private Migration =

    let initial_backfill_calculation (library: Library) (database: UserDatabase) : Session array * KeymodeSkillBreakdown array =

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
                        KeymodePlaytime = Map.empty
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

            if session_start_time > 0 then
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
                    KeymodePlaytime = Map.empty

                }
            let d = KeymodeSkillBreakdown.decay_over_time last_time (Timestamp.now())
            for i = 0 to skills.Length - 1 do
                skills.[i] <- skills.[i].Scale d
        }
        |> Seq.toArray,
        skills

    let migrate_legacy_stats (library: Library) (database: UserDatabase) =

        Logging.Info("Backfilling session data from score history...")

        TOTAL_STATS <- TotalStats.Default
        CURRENT_SESSION <- CurrentSession.Default

        let sessions, current_skills = initial_backfill_calculation library database
        DbSessions.save_batch sessions database.Database
        PREVIOUS_SESSIONS <-
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_local_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        CURRENT_SESSION <- { CURRENT_SESSION with KeymodeSkills = current_skills }

        let legacy_stats = load_important_json_file "Stats" (System.IO.Path.Combine(get_game_folder "Data", "stats.json")) false
        TOTAL_STATS <- { legacy_stats with XP = legacy_stats.NotesHit; KeymodeSkills = TOTAL_STATS.KeymodeSkills }

    let migrate (library: Library) (database: UserDatabase) =
        if PREVIOUS_SESSIONS = Map.empty && TOTAL_STATS.GameTime = 0.0 then
            migrate_legacy_stats library database