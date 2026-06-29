namespace Prelude.Data.User

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay.Replays

module private UserDatabaseMigrations =

    let migrate (db: Database) : Database =
        Database.migrate "AddScoresTable" (fun db -> DbScores.CREATE_TABLE.Execute () db |> expect |> ignore) db
        Database.migrate "AddChartDataTable" (fun db -> DbChartData.CREATE_TABLE.Execute () db |> expect |> ignore) db

        Database.migrate
            "AddChartIdIndexToScores"
            (fun db -> DbScores.CREATE_INDEX.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "ResetPbDataTimestampsAdded"
            (fun db -> DbChartData.RESET_PERSONAL_BESTS.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "AddIsFailedColumnToScores"
            (fun db -> DbScores.ADD_FAILED_COLUMN.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "AddTimestampIndexToScores"
            (fun db -> DbScores.CREATE_TIMESTAMP_INDEX.Execute () db |> expect |> ignore)
            db

        Database.migrate "AddSingletons" (fun db -> DbSingletons.CREATE_TABLE.Execute () db |> expect |> ignore) db

        Database.migrate
            "AddSessions"
            (fun db ->
                Database.create_table DbSessions.TABLE db |> expect |> ignore
                DbSessions.CREATE_INDEX.Execute () db |> expect |> ignore
            )
            db

        Database.migrate
            "RemoveBrokenSessionJan1970"
            (fun db ->
                Database.exec_raw """DELETE FROM SESSIONS WHERE Start = 0""" db
                |> expect
                |> function
                    | 0 -> ()
                    | n -> Logging.Debug "Removed %i sessions with Start set to 0L" n
            )
            db

        Database.migrate
            "AddKeymodePlaytimeStats"
            (fun db -> DbSessions.ADD_KEYMODE_PLAYTIME.Execute () db |> expect |> ignore)
            db

        Database.migrate
            "RemoveKeymodeSkills"
            (fun db -> DbSessions.REMOVE_KEYMODE_SKILLS.Execute () db |> expect |> ignore)
            db

        db

type internal ChangeTracker<'T> =
    {
        Changes: Dictionary<string, 'T>
    }
    static member Empty = { Changes = Dictionary<string, 'T>() }

    member this.Add(chart_id: string, value: 'T) : unit =
        lock this <| fun () -> this.Changes.[chart_id] <- value

    member this.Dump: (string * 'T) array =
        lock this
        <| fun () ->
            let result = this.Changes |> Seq.map (|KeyValue|) |> Array.ofSeq
            this.Changes.Clear()
            result

type ChartSaveData(chart_id: string, data: DbChartData, db: UserDatabase) =
    let mutable offset = data.Offset
    let mutable last_played = data.LastPlayed
    let mutable comment = data.Comment
    let mutable breakpoints = data.Breakpoints
    let mutable personal_bests = data.PersonalBests
    let mutable scores: Score list = []

    // todo: add locks to protect from threads

    member this.Offset
        with get () = offset
        and set v =
            offset <- v
            db.ChangedOffsets.Add(chart_id, v)

    member this.LastPlayed
        with get () = last_played
        and set v =
            last_played <- v
            db.ChangedLastPlayed.Add(chart_id, v)

    member this.Comment
        with get () = comment
        and set v =
            comment <- v
            db.ChangedComments.Add(chart_id, v)

    member this.Breakpoints
        with get () = breakpoints
        and set v =
            breakpoints <- v
            db.ChangedBreakpoints.Add(chart_id, v)

    member this.PersonalBests
        with get () = personal_bests
        and set v =
            personal_bests <- v
            db.ChangedPersonalBests.Add(chart_id, v)

    member this.Scores
        with get () = scores
        and internal set v = scores <- v

    member this.ScoreByTimestamp(timestamp: int64) : Score option =
        scores |> List.tryFind(fun score -> score.Timestamp = timestamp)

and UserDatabase =
    internal
        {
            Database: Database
            Cache: Dictionary<string, ChartSaveData>
            LockObject: obj
            ChangedOffsets: ChangeTracker<Time>
            ChangedLastPlayed: ChangeTracker<int64>
            ChangedComments: ChangeTracker<string>
            ChangedBreakpoints: ChangeTracker<ChartTime list>
            ChangedPersonalBests: ChangeTracker<Map<string, Bests>>
            FastLoaded: bool
        }
    member this.TryGetCachedChartData(chart_id: string) : ChartSaveData option =
        lock this.LockObject
        <| fun () ->

            match this.Cache.TryGetValue(chart_id) with
            | true, data -> Some data
            | false, _ -> None

    member this.GetChartData(chart_id: string) : ChartSaveData =
        lock this.LockObject
        <| fun () ->

            let inline fetch_chart_data () : ChartSaveData =
                if this.FastLoaded then
                    ChartSaveData(chart_id, DbChartData.DEFAULT, this)
                else
                    let chart_db_data: DbChartData = DbChartData.get chart_id this.Database
                    let scores = DbScores.by_chart_id chart_id this.Database
                    ChartSaveData(chart_id, chart_db_data, this, Scores = List.ofArray scores)

            match this.TryGetCachedChartData(chart_id) with
            | Some already_cached -> already_cached
            | None ->
                let new_info = fetch_chart_data()
                this.Cache.[chart_id] <- new_info
                new_info

    member this.SaveChanges() : unit =
        DbChartData.save_offsets this.ChangedOffsets.Dump this.Database
        DbChartData.save_last_played this.ChangedLastPlayed.Dump this.Database
        DbChartData.save_comments this.ChangedComments.Dump this.Database
        DbChartData.save_breakpoints this.ChangedBreakpoints.Dump this.Database
        DbChartData.save_personal_bests this.ChangedPersonalBests.Dump this.Database

    member this.SaveScore(chart_id: string, score: Score) : unit =
        lock this.LockObject
        <| fun () ->

            DbScores.save chart_id score this.Database

            match this.TryGetCachedChartData(chart_id) with
            | None -> ()
            | Some existing_data -> existing_data.Scores <- score :: existing_data.Scores

    member this.SaveScore(score_info: ScoreInfo) : unit =
        this.SaveScore(score_info.ChartMeta.Hash, score_info.ToScore())

    member this.DeleteScore(chart_id: string, timestamp: int64) : bool =
        lock this.LockObject
        <| fun () ->

            let inline remove_score_from_cache () =
                match this.TryGetCachedChartData(chart_id) with
                | None -> ()
                | Some existing_data ->
                    existing_data.Scores <- existing_data.Scores |> List.filter(fun s -> s.Timestamp <> timestamp)

            if DbScores.delete_by_timestamp chart_id timestamp this.Database > 0 then
                remove_score_from_cache()
                true
            else
                false

    member this.DeleteScore(score_info: ScoreInfo) : bool =
        this.DeleteScore(score_info.ChartMeta.Hash, score_info.TimePlayed)

    member this.GetScoresInTimeRange(start_time: int64, end_time: int64) : (string * Score) array =
        DbScores.get_between start_time end_time this.Database

    member this.TransferScores(source_hash: string, target_hash: string) : unit =
        lock this.LockObject
        <| fun () ->

            let before_scores = this.GetChartData(source_hash)
            let after_scores = this.GetChartData(target_hash)
            after_scores.Scores <- before_scores.Scores @ after_scores.Scores
            this.Cache.Remove(source_hash) |> ignore
            let succeeded = DbScores.transfer source_hash target_hash this.Database
            Logging.Debug "Moved %i scores from '%s' to '%s'" succeeded source_hash target_hash

    // Pre-loads the contents of the db into memory (used by game client)
    static member CreateFullyLoaded(database: Database) : UserDatabase =

        let inline fast_load_charts (user_db: UserDatabase) : unit =
            for chart_id, chart_db_data in DbChartData.fast_load user_db.Database do
                user_db.Cache.Add(chart_id, ChartSaveData(chart_id, chart_db_data, user_db))

        let inline fast_load_scores (user_db: UserDatabase) : unit =
            let default_db_data = DbChartData.DEFAULT

            for chart_id, scores in DbScores.fast_load user_db.Database do
                match user_db.TryGetCachedChartData(chart_id) with
                | Some existing -> existing.Scores <- scores
                | None ->
                    user_db.Cache.Add(chart_id, ChartSaveData(chart_id, default_db_data, user_db, Scores = scores))

        let migrated_database = UserDatabaseMigrations.migrate(database)

        let user_db =
            {
                Database = migrated_database
                Cache = Dictionary()
                LockObject = obj()
                ChangedOffsets = ChangeTracker.Empty
                ChangedLastPlayed = ChangeTracker.Empty
                ChangedComments = ChangeTracker.Empty
                ChangedBreakpoints = ChangeTracker.Empty
                ChangedPersonalBests = ChangeTracker.Empty
                FastLoaded = true
            }

        fast_load_charts(user_db)
        fast_load_scores(user_db)
        user_db


    // Scores, chart offsets, etc are fetched from the database on-demand and cached after first retrieval
    static member CreateLazyLoaded(database: Database) : UserDatabase =
        let migrated_database = UserDatabaseMigrations.migrate(database)

        {
            Database = migrated_database
            Cache = Dictionary()
            LockObject = obj()
            ChangedOffsets = ChangeTracker.Empty
            ChangedLastPlayed = ChangeTracker.Empty
            ChangedComments = ChangeTracker.Empty
            ChangedBreakpoints = ChangeTracker.Empty
            ChangedPersonalBests = ChangeTracker.Empty
            FastLoaded = false
        }
