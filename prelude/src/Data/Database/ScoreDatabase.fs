namespace Prelude.Data

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay
open Prelude.Data.Scores

type NewChartSaveData =
    {
        Offset: Setting<Time>
        LastPlayed: Setting<int64>
        Comment: Setting<string>
        Breakpoints: Setting<Time list>
        Scores: DbCell<NewScore list>
        PersonalBests: DbCell<Map<string, Bests>>
    }

type internal ChangeTracker<'T> =
    {
        Changes: Dictionary<string, 'T> 
    }
    static member Empty = { Changes = Dictionary<string, 'T>() }

    member this.Add (chart_id: string) (value: 'T) = this.Changes.[chart_id] <- value
    member this.Dump : (string * 'T) array = 
        let result = this.Changes |> Seq.map (|KeyValue|) |> Array.ofSeq
        this.Changes.Clear()
        result

type ScoreDatabase =
    internal {
        Database : Database
        Cache: Dictionary<string, NewChartSaveData>
        LockObject: obj
        ChangedOffsets: ChangeTracker<Time>
        ChangedLastPlayed: ChangeTracker<int64>
        ChangedComments: ChangeTracker<string>
        ChangedBreakpoints: ChangeTracker<ChartTime list>
    }

module ScoreDatabase =

    let create (database: Database) : ScoreDatabase = 
        {
            Database = database
            Cache = Dictionary()
            LockObject = obj()
            ChangedOffsets = ChangeTracker.Empty
            ChangedLastPlayed = ChangeTracker.Empty
            ChangedComments = ChangeTracker.Empty
            ChangedBreakpoints = ChangeTracker.Empty
        }

    let get_cached (chart_id: string) (db: ScoreDatabase) : NewChartSaveData option =
        match db.Cache.TryGetValue chart_id with
        | true, res -> Some res
        | false, _ -> None

    let get (chart_id: string) (db: ScoreDatabase) : NewChartSaveData =
        match get_cached chart_id db with
        | Some existing -> existing
        | None ->
            // lock here
            let stuff_from_db : DbChartData = failwith "some database fetch"
            let new_info : NewChartSaveData =
                {
                    Offset = Setting.simple stuff_from_db.Offset |> Setting.trigger (db.ChangedOffsets.Add chart_id)
                    LastPlayed = Setting.simple stuff_from_db.LastPlayed |> Setting.trigger (db.ChangedLastPlayed.Add chart_id)
                    Comment = Setting.simple stuff_from_db.Comment |> Setting.trigger (db.ChangedComments.Add chart_id)
                    Breakpoints = Setting.simple stuff_from_db.Breakpoints |> Setting.trigger (db.ChangedBreakpoints.Add chart_id)
                    Scores = DbCell(stuff_from_db.Scores)
                    PersonalBests = DbCell(stuff_from_db.PersonalBests)
                }
            db.Cache.[chart_id] <- new_info
            new_info

    let save_changes (db: ScoreDatabase) =

        let some_db_stuff (data: (string * 'T) array) = failwith "nyi"

        db.ChangedOffsets.Dump |> some_db_stuff
        db.ChangedLastPlayed.Dump |> some_db_stuff
        db.ChangedComments.Dump |> some_db_stuff
        db.ChangedBreakpoints.Dump |> some_db_stuff

    let save_score (chart_id: string) (score: NewScore) (db: ScoreDatabase) =
        DbScore.save chart_id score db.Database |> ignore
        match get_cached chart_id db with
        | None -> ()
        | Some existing_data ->
            existing_data.Scores.Value <- score :: existing_data.Scores.Value

    let save_score_and_bests (chart_id: string) (score: NewScore) (ruleset_id: string) (bests: Bests) (db: ScoreDatabase) =
        DbScore.save chart_id score db.Database |> ignore
        // save bests stuff in the database here
        match get_cached chart_id db with
        | None -> ()
        | Some existing_data ->
            existing_data.Scores.Value <- score :: existing_data.Scores.Value
            existing_data.PersonalBests.Value <- Map.add ruleset_id bests existing_data.PersonalBests.Value

    let delete_score (chart_id: string) (timestamp: int64) (db: ScoreDatabase) : bool =
        DbScore.delete_by_timestamp chart_id timestamp db.Database > 0