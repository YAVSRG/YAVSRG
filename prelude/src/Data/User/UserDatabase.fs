namespace Prelude.Data.User

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay.Replays

type internal ChangeTracker<'T> =
    {
        Changes: Dictionary<string, 'T>
    }
    static member Empty = { Changes = Dictionary<string, 'T>() }

    member this.Add (chart_id: string) (value: 'T) =
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
    let mutable scores : Score list = []

    // todo: add locks to protect from threads

    member this.Offset
        with get () = offset
        and set v =
            offset <- v
            db.ChangedOffsets.Add chart_id v

    member this.LastPlayed
        with get () = last_played
        and set v =
            last_played <- v
            db.ChangedLastPlayed.Add chart_id v

    member this.Comment
        with get () = comment
        and set v =
            comment <- v
            db.ChangedComments.Add chart_id v

    member this.Breakpoints
        with get () = breakpoints
        and set v =
            breakpoints <- v
            db.ChangedBreakpoints.Add chart_id v

    member this.PersonalBests
        with get () = personal_bests
        and set v =
            personal_bests <- v
            db.ChangedPersonalBests.Add chart_id v

    member this.Scores
        with get () = scores
        and internal set v = scores <- v

    member this.ScoreByTimestamp (timestamp: int64) =
        scores |> List.tryFind (fun score -> score.Timestamp = timestamp)

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

module UserDatabase =

    let get_chart_data_cached (chart_id: string) (db: UserDatabase) : ChartSaveData option =
        lock db.LockObject
        <| fun () ->
            match db.Cache.TryGetValue chart_id with
            | true, res -> Some res
            | false, _ -> None

    let get_chart_data (chart_id: string) (db: UserDatabase) : ChartSaveData =
        lock db.LockObject
        <| fun () ->
            match get_chart_data_cached chart_id db with
            | Some existing -> existing
            | None ->
                let new_info =
                    // Only bother to load from db -> cache if we didn't already fast load everything the database had into cache
                    if db.FastLoaded then
                        ChartSaveData(chart_id, DbChartData.DEFAULT, db)
                    else
                        let chart_db_data: DbChartData = DbChartData.get chart_id db.Database
                        let scores = DbScores.by_chart_id chart_id db.Database
                        ChartSaveData(chart_id, chart_db_data, db, Scores = List.ofArray scores)

                db.Cache.[chart_id] <- new_info
                new_info

    let save_changes (db: UserDatabase) =
        DbChartData.save_offsets db.ChangedOffsets.Dump db.Database
        DbChartData.save_last_played db.ChangedLastPlayed.Dump db.Database
        DbChartData.save_comments db.ChangedComments.Dump db.Database
        DbChartData.save_breakpoints db.ChangedBreakpoints.Dump db.Database
        DbChartData.save_personal_bests db.ChangedPersonalBests.Dump db.Database

    let save_score (chart_id: string) (score: Score) (db: UserDatabase) =
        lock db.LockObject
        <| fun () ->
            DbScores.save chart_id score db.Database |> ignore

            match get_chart_data_cached chart_id db with
            | None -> ()
            | Some existing_data -> existing_data.Scores <- score :: existing_data.Scores

    let delete_score (chart_id: string) (timestamp: int64) (db: UserDatabase) : bool =
        lock db.LockObject
        <| fun () ->
            if DbScores.delete_by_timestamp chart_id timestamp db.Database > 0 then
                match get_chart_data_cached chart_id db with
                | None -> ()
                | Some existing_data ->
                    existing_data.Scores <- existing_data.Scores |> List.filter (fun s -> s.Timestamp <> timestamp)

                true
            else
                false

    let get_scores_between (start_time: int64) (end_time: int64) (db: UserDatabase) : (string * Score) array =
        DbScores.get_between start_time end_time db.Database

    let transfer_scores (before_hash: string) (after_hash: string) (db: UserDatabase) =
        lock db.LockObject <| fun () ->
        let before_scores = get_chart_data before_hash db
        let after_scores = get_chart_data after_hash db
        after_scores.Scores <- before_scores.Scores @ after_scores.Scores
        db.Cache.Remove before_hash |> ignore
        let succeeded = DbScores.transfer before_hash after_hash db.Database
        Logging.Debug "Moved %i scores from '%s' to '%s'" succeeded before_hash after_hash

    let private fast_load (db: UserDatabase) : UserDatabase =
        lock db.LockObject
        <| fun () ->
            assert (db.Cache.Count = 0)

            for chart_id, chart_db_data in DbChartData.fast_load db.Database do
                db.Cache.Add(chart_id, ChartSaveData(chart_id, chart_db_data, db))

            let default_db_data = DbChartData.DEFAULT

            for chart_id, scores in DbScores.fast_load db.Database do
                match get_chart_data_cached chart_id db with
                | Some existing ->
                    assert (existing.Scores.IsEmpty)
                    existing.Scores <- scores
                | None -> db.Cache.Add(chart_id, ChartSaveData(chart_id, default_db_data, db, Scores = scores))

            db

    let private migrate (db: Database) : Database =
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

        Database.migrate
            "AddSingletons"
            (fun db -> DbSingletons.CREATE_TABLE.Execute () db |> expect |> ignore)
            db

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
                |> function 0 -> () | n -> Logging.Debug "Removed %i sessions with Start set to 0L" n
            )
            db

        Database.migrate
            "AddKeymodePlaytimeStats"
            (fun db -> DbSessions.ADD_KEYMODE_PLAYTIME.Execute () db |> expect |> ignore)
            db

        db

    // Fast load true loads the contents of the db into memory (used by game client)
    // Fast load false may be used for tools that just want to fetch stuff for a couple of charts
    let create (use_fast_load: bool) (database: Database) : UserDatabase =
        {
            Database = migrate database
            Cache = Dictionary()
            LockObject = obj ()
            ChangedOffsets = ChangeTracker.Empty
            ChangedLastPlayed = ChangeTracker.Empty
            ChangedComments = ChangeTracker.Empty
            ChangedBreakpoints = ChangeTracker.Empty
            ChangedPersonalBests = ChangeTracker.Empty
            FastLoaded = use_fast_load
        }
        |> if use_fast_load then fast_load else id