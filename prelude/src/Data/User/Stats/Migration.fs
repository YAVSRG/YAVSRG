namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Prelude
open Prelude.Calculator
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Data.Library

module private Migration =

    let legacy_backfill (library: Library) (database: UserDatabase) : Session array * KeymodeSkillBreakdown array =

        // calculate skillsets
        let sc_j4 = SC.create 4
        let sc_j4_id = Ruleset.hash sc_j4

        for chart_meta_key in library.Charts.Cache.Keys do
            let chart_meta = library.Charts.Cache.[chart_meta_key]

            let data = UserDatabase.get_chart_data chart_meta.Hash database
            match data.PersonalBests.TryFind(sc_j4_id) with
            | Some pbs ->
                for (acc, rate, _) in pbs.Accuracy do
                    KeymodeSkillBreakdown.score chart_meta.Patterns acc rate TOTAL_STATS.KeymodeSkills.[chart_meta.Keys - 3] |> ignore
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
            | Some chart_meta -> chart_meta.Length / (snd scores.[0]).Rate |> int64
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
                    | Some chart_meta ->

                        let data = UserDatabase.get_chart_data chart_meta.Hash database
                        match data.PersonalBests.TryFind(sc_j4_id) with
                        | Some pbs ->
                            match PersonalBests.get_best_above score.Rate pbs.Accuracy with
                            | Some (acc, rate, _) ->
                                KeymodeSkillBreakdown.score chart_meta.Patterns acc score.Rate skills.[chart_meta.Keys - 3] |> ignore
                            | None -> ()
                        | None -> ()

                        session_playing_time <- session_playing_time + chart_meta.Length / score.Rate
                        chart_meta.Length / score.Rate |> int64
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

        Logging.Info "Backfilling session data from score history..."

        TOTAL_STATS <- TotalStats.Default
        CURRENT_SESSION <- CurrentSession.Default

        let sessions, current_skills = legacy_backfill library database
        DbSessions.save_batch sessions database.Database
        PREVIOUS_SESSIONS <-
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        CURRENT_SESSION <- { CURRENT_SESSION with KeymodeSkills = current_skills }

        let legacy_stats = load_important_json_file "Stats" (System.IO.Path.Combine(get_game_folder "Data", "stats.json")) false
        TOTAL_STATS <- { legacy_stats with XP = legacy_stats.NotesHit; KeymodeSkills = TOTAL_STATS.KeymodeSkills }

    let keymode_playtime_backfill (library: Library) (database: UserDatabase) =

        Logging.Debug "Backfilling keymode playtimes from score history..."

        let scores =
            seq {
                for chart_id in database.Cache.Keys do
                    for score in database.Cache.[chart_id].Scores do
                        yield struct {| ChartId = chart_id; Score = score |}
            }
            |> Seq.sortBy (_.Score >> _.Timestamp)
            |> Array.ofSeq

        let length_of_score (score: struct {| ChartId: string; Score: Score |}) =
            match ChartDatabase.get_meta score.ChartId library.Charts with
            | Some chart_meta -> chart_meta.Length / score.Score.Rate |> float
            | None -> 123056.0

        let sessions =
            PREVIOUS_SESSIONS
            |> Map.values
            |> Seq.concat
            |> Array.ofSeq

        let mutable score_index = 0
        let mutable i = 0
        let mutable total_keymodes = Map.empty
        while i < sessions.Length do
            assert(i = 0 || sessions.[i - 1].Start < sessions.[i].Start)
            let session = sessions.[i]
            let mutable keymode_playtimes = Map.empty
            while score_index < scores.Length && scores.[score_index].Score.Timestamp <= session.End do
                let score = scores.[score_index]
                let length = length_of_score score |> float
                keymode_playtimes <- keymode_playtimes.Change(score.Score.Keys, function None -> Some length | Some v -> Some (v + length))
                score_index <- score_index + 1

            let playtime_sum = keymode_playtimes |> Map.values |> Seq.sum
            if playtime_sum < session.PlayTime then
                let length = session.PlayTime - playtime_sum
                keymode_playtimes <- keymode_playtimes.Change(4, function None -> Some length | Some v -> Some (v + length))
            sessions.[i] <-
                { session with
                    PlayTime = max session.PlayTime playtime_sum
                    GameTime = max session.GameTime (max session.PlayTime playtime_sum + session.PracticeTime)
                    KeymodePlaytime = keymode_playtimes
                }
            total_keymodes <- add_playtimes total_keymodes keymode_playtimes
            i <- i + 1

        let total_playtime = sessions |> Array.sumBy _.PlayTime
        let total_gametime = sessions |> Array.sumBy _.GameTime
        Logging.Debug "Total playtime: %s -> %s" (format_long_time TOTAL_STATS.PlayTime) (format_long_time total_playtime)
        Logging.Debug "Total game time: %s -> %s" (format_long_time TOTAL_STATS.GameTime) (format_long_time total_gametime)

        TOTAL_STATS <-
            { TOTAL_STATS with
                PlayTime = max TOTAL_STATS.PlayTime total_playtime
                GameTime = max TOTAL_STATS.GameTime total_gametime
                KeymodePlaytime = total_keymodes
            }
        MIGRATIONS <- MIGRATIONS.Add "BackfillKeymodePlaytime"
        PREVIOUS_SESSIONS <-
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq
        DbSessions.save_batch sessions database.Database
        save_stats database

    let migrate (library: Library) (database: UserDatabase) =

        if PREVIOUS_SESSIONS = Map.empty && TOTAL_STATS.GameTime = 0.0 then
            migrate_legacy_stats library database

        if not (MIGRATIONS.Contains "BackfillKeymodePlaytime") then
            backup_stats "backup_0.7.27.7" database
            keymode_playtime_backfill library database