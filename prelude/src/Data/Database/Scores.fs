namespace Prelude.Data

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay

type internal ChangeTracker<'T> =
    {
        Changes: Dictionary<string, 'T> 
    }
    static member Empty = { Changes = Dictionary<string, 'T>() }

    member this.Add (chart_id: string) (value: 'T) = lock this <| fun () -> 
        this.Changes.[chart_id] <- value
    member this.Dump : (string * 'T) array = lock this <| fun () -> 
        let result = this.Changes |> Seq.map (|KeyValue|) |> Array.ofSeq
        this.Changes.Clear()
        result

type ChartSaveData(chart_id: string, data: DbChartData, db: ScoreDatabase) =
    let mutable offset = data.Offset
    let mutable last_played = data.LastPlayed
    let mutable comment = data.Comment
    let mutable breakpoints = data.Breakpoints
    let mutable personal_bests = data.PersonalBests
    let mutable scores = []

    // todo: add locks to protect from threads

    member this.Offset
        with get() = offset
        and set v = offset <- v; db.ChangedOffsets.Add chart_id v

    member this.LastPlayed
        with get() = last_played
        and set v = last_played <- v; db.ChangedLastPlayed.Add chart_id v

    member this.Comment
        with get() = comment
        and set v = comment <- v; db.ChangedComments.Add chart_id v

    member this.Breakpoints
        with get() = breakpoints
        and set v = breakpoints <- v; db.ChangedBreakpoints.Add chart_id v

    member this.PersonalBests
        with get() = personal_bests
        and set v = personal_bests <- v; db.ChangedPersonalBests.Add chart_id v

    member this.Scores
        with get() = scores
        and internal set v = scores <- v

and ScoreDatabase =
    internal {
        Database : Database
        Cache: Dictionary<string, ChartSaveData>
        LockObject: obj
        ChangedOffsets: ChangeTracker<Time>
        ChangedLastPlayed: ChangeTracker<int64>
        ChangedComments: ChangeTracker<string>
        ChangedBreakpoints: ChangeTracker<ChartTime list>
        ChangedPersonalBests: ChangeTracker<Map<string, Bests>>
        FastLoaded: bool
    }

module ScoreDatabase =

    let get_cached (chart_id: string) (db: ScoreDatabase) : ChartSaveData option = lock db.LockObject <| fun () ->
        match db.Cache.TryGetValue chart_id with
        | true, res -> Some res
        | false, _ -> None

    let get (chart_id: string) (db: ScoreDatabase) : ChartSaveData = lock db.LockObject <| fun () ->
        match get_cached chart_id db with
        | Some existing -> existing
        | None ->
            let new_info = 
                // Only bother to load from db -> cache if we didn't already fast load everything the database had into cache
                if db.FastLoaded then
                    ChartSaveData(chart_id, DbChartData.DEFAULT, db)
                else
                    let chart_db_data : DbChartData = DbChartData.get chart_id db.Database
                    let scores = DbScores.by_chart_id chart_id db.Database
                    ChartSaveData(chart_id, chart_db_data, db, Scores = List.ofArray scores)
            db.Cache.[chart_id] <- new_info
            new_info

    let save_changes (db: ScoreDatabase) =
        DbChartData.save_offsets db.ChangedOffsets.Dump db.Database
        DbChartData.save_last_played db.ChangedLastPlayed.Dump db.Database
        DbChartData.save_comments db.ChangedComments.Dump db.Database
        DbChartData.save_breakpoints db.ChangedBreakpoints.Dump db.Database
        DbChartData.save_personal_bests db.ChangedPersonalBests.Dump db.Database

    let save_score (chart_id: string) (score: Score) (db: ScoreDatabase) = lock db.LockObject <| fun () ->
        DbScores.save chart_id score db.Database |> ignore
        match get_cached chart_id db with
        | None -> ()
        | Some existing_data -> existing_data.Scores <- score :: existing_data.Scores

    let delete_score (chart_id: string) (timestamp: int64) (db: ScoreDatabase) : bool = lock db.LockObject <| fun () ->
        if DbScores.delete_by_timestamp chart_id timestamp db.Database > 0 then
            match get_cached chart_id db with
            | None -> ()
            | Some existing_data -> 
                existing_data.Scores <- 
                    existing_data.Scores
                    |> List.filter (fun s -> s.Timestamp <> timestamp)
            true
        else
            false

    let private fast_load (db: ScoreDatabase) : ScoreDatabase = lock db.LockObject <| fun () ->
        assert(db.Cache.Count = 0)
        for chart_id, chart_db_data in DbChartData.fast_load db.Database do
            db.Cache.Add(chart_id, ChartSaveData(chart_id, chart_db_data, db))
        let default_db_data = DbChartData.DEFAULT
        for chart_id, scores in DbScores.fast_load db.Database do
            match get_cached chart_id db with
            | Some existing ->
                assert(existing.Scores.IsEmpty)
                existing.Scores <- scores
            | None ->
                db.Cache.Add(chart_id, ChartSaveData(chart_id, default_db_data, db, Scores = scores))
        db
    
    let private migrate (db: Database) : Database =
        Database.migrate
            "AddScoresTable"
            (fun db -> DbScores.CREATE_TABLE.Execute () db |> expect |> ignore)
            db
        Database.migrate
            "AddChartDataTable"
            (fun db -> DbChartData.CREATE_TABLE.Execute () db |> expect |> ignore)
            db
        Database.migrate
            "AddChartIdIndexToScores"
            (fun db -> DbScores.CREATE_INDEX.Execute () db |> expect |> ignore)
            db
        db

    let private legacy_migrate (db: ScoreDatabase) : ScoreDatabase =
        match LegacyScoreDatabase.TryLoad() with
        | None -> db
        | Some legacy_db ->
            Logging.Debug("Found and loaded all scores in scores.json ...")
            seq {
                for hash in legacy_db.Entries.Keys do
                    let data = legacy_db.Entries.[hash]
                    for score in data.Scores do
                        yield hash, score.Migrate
            }
            |> fun scores -> DbScores.save_batch scores db.Database
            
            Logging.Debug("Copied all scores to database in new format")

            for hash in legacy_db.Entries.Keys do
                let data = legacy_db.Entries.[hash]

                if data.PersonalBests.Count > 0 then
                    db.ChangedPersonalBests.Add hash <| Map.ofSeq (data.PersonalBests |> Seq.map (|KeyValue|) |> Seq.map (fun (k, v) -> (k, v.Migrate)))

                if data.LastPlayed.Year > 2000 then 
                    db.ChangedLastPlayed.Add hash <| Timestamp.from_datetime data.LastPlayed

                if data.Comment <> "" then
                    db.ChangedComments.Add hash <| data.Comment

            save_changes db
            
            Logging.Debug("Copied all other things (comments, personal bests, time last played) to database")
            LegacyScoreDatabase.MarkOld()
            Logging.Debug("Marked scores.json as old. All done!")
            db

    // Fast load true loads the contents of the db into memory
    // Fast load false may be used for tools that just want to fetch stuff for a couple of charts
    let create (fast_loaded: bool) (database: Database) : ScoreDatabase =
        legacy_migrate {
            Database = migrate database
            Cache = Dictionary()
            LockObject = obj()
            ChangedOffsets = ChangeTracker.Empty
            ChangedLastPlayed = ChangeTracker.Empty
            ChangedComments = ChangeTracker.Empty
            ChangedBreakpoints = ChangeTracker.Empty
            ChangedPersonalBests = ChangeTracker.Empty
            FastLoaded = fast_loaded
        }
        |> if fast_loaded then fast_load else id