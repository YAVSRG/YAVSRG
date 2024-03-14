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

    member this.Add (chart_id: string) (value: 'T) = lock this <| fun () -> 
        this.Changes.[chart_id] <- value
    member this.Dump : (string * 'T) array = lock this <| fun () -> 
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

    let get_cached (chart_id: string) (db: ScoreDatabase) : NewChartSaveData option = lock db.LockObject <| fun () ->
        match db.Cache.TryGetValue chart_id with
        | true, res -> Some res
        | false, _ -> None

    let get (chart_id: string) (db: ScoreDatabase) : NewChartSaveData = lock db.LockObject <| fun () ->
        match get_cached chart_id db with
        | Some existing -> existing
        | None ->
            let chart_db_data : DbChartData = DbChartData.get chart_id db.Database
            let scores = DbScores.by_chart_id chart_id db.Database
            let new_info : NewChartSaveData =
                {
                    Offset = Setting.simple chart_db_data.Offset |> Setting.trigger (db.ChangedOffsets.Add chart_id)
                    LastPlayed = Setting.simple chart_db_data.LastPlayed |> Setting.trigger (db.ChangedLastPlayed.Add chart_id)
                    Comment = Setting.simple chart_db_data.Comment |> Setting.trigger (db.ChangedComments.Add chart_id)
                    Breakpoints = Setting.simple chart_db_data.Breakpoints |> Setting.trigger (db.ChangedBreakpoints.Add chart_id)
                    Scores = DbCell(List.ofArray scores)
                    PersonalBests = DbCell(chart_db_data.PersonalBests)
                }
            db.Cache.[chart_id] <- new_info
            new_info

    let save_changes (db: ScoreDatabase) =
        DbChartData.save_offsets db.ChangedOffsets.Dump db.Database
        DbChartData.save_last_played db.ChangedLastPlayed.Dump db.Database
        DbChartData.save_comments db.ChangedComments.Dump db.Database
        DbChartData.save_breakpoints db.ChangedBreakpoints.Dump db.Database

    let save_score (chart_id: string) (score: NewScore) (db: ScoreDatabase) = lock db.LockObject <| fun () ->
        DbScores.save chart_id score db.Database |> ignore
        match get_cached chart_id db with
        | None -> ()
        | Some existing_data -> existing_data.Scores.Value <- score :: existing_data.Scores.Value

    let save_bests (chart_id: string) (bests: Map<string, Bests>) (db: ScoreDatabase) = lock db.LockObject <| fun () ->
        DbChartData.save_personal_bests chart_id bests db.Database
        match get_cached chart_id db with
        | None -> ()
        | Some existing_data -> existing_data.PersonalBests.Value <- bests

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