namespace Prelude.Data

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay
open Prelude.Data.Scores

type ChartSaveData =
    {
        Offset: Setting<Time>
        LastPlayed: Setting<int64>
        Comment: Setting<string>
        Breakpoints: Setting<Time list>
        PersonalBests: Setting<Map<string, Bests>>
        Scores: DbCell<Score list>
    }

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

type ScoreDatabase =
    internal {
        Database : Database
        Cache: Dictionary<string, ChartSaveData>
        LockObject: obj
        ChangedOffsets: ChangeTracker<Time>
        ChangedLastPlayed: ChangeTracker<int64>
        ChangedComments: ChangeTracker<string>
        ChangedBreakpoints: ChangeTracker<ChartTime list>
        ChangedPersonalBests: ChangeTracker<Map<string, Bests>>
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
            // todo: can optimise even further by marking a db as fast-loaded
            let chart_db_data : DbChartData = DbChartData.get chart_id db.Database
            let scores = DbScores.by_chart_id chart_id db.Database
            let new_info : ChartSaveData =
                {
                    Offset = Setting.threadsafe chart_db_data.Offset |> Setting.trigger (db.ChangedOffsets.Add chart_id)
                    LastPlayed = Setting.threadsafe chart_db_data.LastPlayed |> Setting.trigger (db.ChangedLastPlayed.Add chart_id)
                    Comment = Setting.threadsafe chart_db_data.Comment |> Setting.trigger (db.ChangedComments.Add chart_id)
                    Breakpoints = Setting.threadsafe chart_db_data.Breakpoints |> Setting.trigger (db.ChangedBreakpoints.Add chart_id)
                    PersonalBests = Setting.threadsafe chart_db_data.PersonalBests |> Setting.trigger (db.ChangedPersonalBests.Add chart_id)
                    Scores = DbCell (List.ofArray scores)
                }
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
        | Some existing_data -> existing_data.Scores.Value <- score :: existing_data.Scores.Value

    let delete_score (chart_id: string) (timestamp: int64) (db: ScoreDatabase) : bool = lock db.LockObject <| fun () ->
        if DbScores.delete_by_timestamp chart_id timestamp db.Database > 0 then
            match get_cached chart_id db with
            | None -> ()
            | Some existing_data -> 
                existing_data.Scores.Value <- 
                    existing_data.Scores.Value
                    |> List.filter (fun s -> s.Timestamp <> timestamp)
            true
        else
            false
    
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
            // mass import all scores
            Logging.Debug("Found and loaded all scores in scores.json ...")
            seq {
                for hash in legacy_db.Entries.Keys do
                    let data = legacy_db.Entries.[hash]
                    for score in data.Scores do
                        yield hash, score.Migrate
            }
            |> fun scores -> DbScores.save_batch scores db.Database
            
            Logging.Debug("Copied all scores to database in new format")

            // mass import all other user data
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

    let create (database: Database) : ScoreDatabase =
        legacy_migrate {
            Database = migrate database
            Cache = Dictionary()
            LockObject = obj()
            ChangedOffsets = ChangeTracker.Empty
            ChangedLastPlayed = ChangeTracker.Empty
            ChangedComments = ChangeTracker.Empty
            ChangedBreakpoints = ChangeTracker.Empty
            ChangedPersonalBests = ChangeTracker.Empty
        }

    let fast_load (db: ScoreDatabase) : ScoreDatabase = lock db.LockObject <| fun () ->
        assert(db.Cache.Count = 0)
        for chart_id, chart_db_data in DbChartData.fast_load db.Database do
            db.Cache.Add(chart_id, 
                {
                    Offset = Setting.threadsafe chart_db_data.Offset |> Setting.trigger (db.ChangedOffsets.Add chart_id)
                    LastPlayed = Setting.threadsafe chart_db_data.LastPlayed |> Setting.trigger (db.ChangedLastPlayed.Add chart_id)
                    Comment = Setting.threadsafe chart_db_data.Comment |> Setting.trigger (db.ChangedComments.Add chart_id)
                    Breakpoints = Setting.threadsafe chart_db_data.Breakpoints |> Setting.trigger (db.ChangedBreakpoints.Add chart_id)
                    PersonalBests = Setting.threadsafe chart_db_data.PersonalBests |> Setting.trigger (db.ChangedPersonalBests.Add chart_id)
                    Scores = DbCell []
                }
            )
        printfn "fast loading scores"
        let default_db_data = DbChartData.DEFAULT
        for chart_id, scores in DbScores.fast_load db.Database do
            match get_cached chart_id db with
            | Some existing ->
                assert(existing.Scores.Value.IsEmpty)
                existing.Scores.Value <- scores
            | None ->
                db.Cache.Add(chart_id,
                    {
                        Offset = Setting.threadsafe default_db_data.Offset |> Setting.trigger (db.ChangedOffsets.Add chart_id)
                        LastPlayed = Setting.threadsafe default_db_data.LastPlayed |> Setting.trigger (db.ChangedLastPlayed.Add chart_id)
                        Comment = Setting.threadsafe default_db_data.Comment |> Setting.trigger (db.ChangedComments.Add chart_id)
                        Breakpoints = Setting.threadsafe default_db_data.Breakpoints |> Setting.trigger (db.ChangedBreakpoints.Add chart_id)
                        PersonalBests = Setting.threadsafe default_db_data.PersonalBests |> Setting.trigger (db.ChangedPersonalBests.Add chart_id)
                        Scores = DbCell scores
                    }
                )
        db