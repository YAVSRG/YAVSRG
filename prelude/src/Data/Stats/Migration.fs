namespace Prelude.Data.User.Stats

open System
open Percyqaz.Common
open Prelude
open Prelude.Data.User
open Prelude.Data.Library

module private Migration =

    let legacy_backfill (library: Library) : Session array =

        let scores =
            seq {
                for chart_id in library.UserData.Cache.Keys do
                    for score in library.UserData.Cache.[chart_id].Scores do
                        yield chart_id, score
            }
            |> Seq.sortBy (snd >> _.Timestamp)
            |> Array.ofSeq

        let first_score_length =
            if scores.Length = 0 then 0L else
            match library.Charts.GetChartMeta(fst scores.[0]) with
            | Some chart_meta -> chart_meta.Length / (snd scores.[0]).Rate |> int64
            | None -> 2L * 60L * 1000L

        let mutable session_start_time = if scores.Length = 0 then 0L else (snd scores.[0]).Timestamp - first_score_length
        let mutable session_playing_time = 0.0f<ms / rate>
        let mutable last_time = session_start_time
        let mutable score_count = 0

        seq {
            for chart_id, score in scores do

                let score_length =
                    match library.Charts.GetChartMeta(chart_id) with
                    | Some chart_meta ->
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
                        KeymodePlaytime = Map.empty
                    }
                    session_start_time <- score.Timestamp - score_length
                    session_playing_time <- 0.0f<ms / rate>
                    score_count <- 1
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
                    KeymodePlaytime = Map.empty

                }
        }
        |> Seq.toArray

    let migrate_legacy_stats (library: Library) : unit =

        Logging.Info "Backfilling session data from score history..."

        TOTAL_STATS <- TotalStats.Default
        CURRENT_SESSION <- CurrentSession.Default

        let sessions = legacy_backfill library
        DbSessions.save_batch sessions library.UserData.Database
        PREVIOUS_SESSIONS <-
            sessions
            |> Seq.groupBy (fun session -> session.Start |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime)
            |> Seq.map (fun (local_date, sessions) -> (local_date, List.ofSeq sessions))
            |> Map.ofSeq

        let legacy_stats = load_important_json_file "Stats" (System.IO.Path.Combine(get_game_folder "Data", "stats.json")) false
        TOTAL_STATS <- { legacy_stats with XP = legacy_stats.NotesHit }

    let keymode_playtime_backfill (library: Library) : unit =

        Logging.Debug "Backfilling keymode playtimes from score history..."

        let scores =
            seq {
                for chart_id in library.UserData.Cache.Keys do
                    for score in library.UserData.Cache.[chart_id].Scores do
                        yield struct {| ChartId = chart_id; Score = score |}
            }
            |> Seq.sortBy (_.Score >> _.Timestamp)
            |> Array.ofSeq

        let length_of_score (score: struct {| ChartId: string; Score: Score |}) =
            match library.Charts.GetChartMeta(score.ChartId) with
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
        DbSessions.save_batch sessions library.UserData.Database
        save_stats library.UserData

    let migrate (library: Library) : unit =

        if PREVIOUS_SESSIONS = Map.empty && TOTAL_STATS.GameTime = 0.0 then
            migrate_legacy_stats library

        if not (MIGRATIONS.Contains "BackfillKeymodePlaytime") then
            backup_stats "backup_0.7.27.7" library.UserData
            keymode_playtime_backfill library