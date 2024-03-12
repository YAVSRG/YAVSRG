namespace Prelude.Data

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Mods

type DbCell<'T>(value: 'T) =
    let mutable value = value

    member this.Value 
        with get() = value
        and internal set new_value = value <- new_value

type NewScore =
    {
        Timestamp: int64
        Replay: string
        Rate: float32
        Mods: ModState
        IsImported: bool
        Keys: int
    }

type NewChartSaveData =
    {
        Offset: Setting<Time> // get and settable setting, marks the score as dirty and this gets intermittently saved + on shutdown
        LastPlayed: Setting<int64> // get and settable setting, marks the score as dirty and this gets intermittently saved + on shutdown
        Comment: Setting<string> // get and settable setting, marks the score as dirty and this gets intermittently saved + on shutdown
        Breakpoints: Setting<Time list>
        Scores: DbCell<NewScore list> // readonly, underlying list gets modified by internal methods
        PersonalBests: DbCell<Map<string, Bests>> // readonly, underlying map gets modified by internal methods
    }

type internal DbChartData =
    {
        Offset: Time
        LastPlayed: int64
        Comment: string
        Breakpoints: Time list
        Scores: NewScore list
        PersonalBests: Map<string, Bests>
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
        ChangedBreakpoints: ChangeTracker<Time list>
    }

module Migrations =
    
    let setup_database (db: Database) = db

module ScoreDatabase =

    let create (path: string) : ScoreDatabase = 
        {
            Database = Database.from_file path |> Migrations.setup_database
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

    let save_score_and_bests (chart_id: string) (score: NewScore) (ruleset_id: string) (bests: Bests) (db: ScoreDatabase) =
        // save some stuff in the database here
        match get_cached chart_id db with
        | None -> ()
        | Some existing_data ->
            existing_data.Scores.Value <- score :: existing_data.Scores.Value
            existing_data.PersonalBests.Value <- Map.add ruleset_id bests existing_data.PersonalBests.Value
