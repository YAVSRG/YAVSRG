namespace Prelude.Data.Charts

open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data.Sqlite
open Prelude.Charts

type ChartDatabase =
    internal
        {
            Database: Database
            Cache: Dictionary<string, ChartDatabase>
            LockObject: obj
            FastLoaded: bool
        }

module ChartDatabase =

    //let get_chart_cached (chart_id: string) (db: ChartDatabase) : Chart option =
    //    lock db.LockObject
    //    <| fun () ->
    //        match db.Cache.TryGetValue chart_id with
    //        | true, res -> Some res
    //        | false, _ -> None

    //let get_chart (chart_id: string) (db: ChartDatabase) : Chart option =
    //    lock db.LockObject
    //    <| fun () ->
    //        match get_chart_cached chart_id db with
    //        | Some existing -> existing
    //        | None ->
    //            let new_info =
    //                // Only bother to load from db -> cache if we didn't already fast load everything the database had into cache
    //                if db.FastLoaded then
    //                    ChartSaveData(chart_id, DbChartData.DEFAULT, db)
    //                else
    //                    let chart_db_data: DbChartData = DbChartData.get chart_id db.Database
    //                    let scores = DbScores.by_chart_id chart_id db.Database
    //                    ChartSaveData(chart_id, chart_db_data, db, Scores = List.ofArray scores)

    //            db.Cache.[chart_id] <- new_info
    //            new_info

    let private fast_load (db: ChartDatabase) : ChartDatabase =
        lock db.LockObject
        <| fun () ->
            assert (db.Cache.Count = 0)

            //for chart_id, chart_db_data in DbCharts.fast_load db.Database do
            //    db.Cache.Add(chart_id, ChartSaveData(chart_id, chart_db_data, db))

            //let default_db_data = DbChartData.DEFAULT

            //for chart_id, scores in DbScores.fast_load db.Database do
            //    match get_chart_data_cached chart_id db with
            //    | Some existing ->
            //        assert (existing.Scores.IsEmpty)
            //        existing.Scores <- scores
            //    | None -> db.Cache.Add(chart_id, ChartSaveData(chart_id, default_db_data, db, Scores = scores))

            db

    let private migrate (db: Database) : Database =
        Database.migrate "AddChartsTable" (fun db -> DbCharts.CREATE_TABLE.Execute () db |> expect |> ignore) db
        db

    let private legacy_migrate (db: ChartDatabase) : ChartDatabase =
        db

    // Fast load true loads the contents of the db into memory (used by game client)
    // Fast load false may be used for tools that just want to fetch stuff for a couple of charts
    let create (use_fast_load: bool) (database: Database) : ChartDatabase =
        legacy_migrate
            {
                Database = migrate database
                Cache = Dictionary()
                LockObject = obj ()
                FastLoaded = use_fast_load
            }
        |> if use_fast_load then fast_load else id
