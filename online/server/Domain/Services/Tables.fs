namespace Interlude.Web.Server.Domain.Services

open Percyqaz.Common
open Prelude.Backbeat
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat

module Tables =

    let recalculate_table_rating (user_id: int64, table_id: string, table: TableInfo, charts: (string * int) array) =

        match TableRating.get user_id table_id with
        | Some(existing, timestamp) when Timestamp.now () - timestamp < 300000 -> existing
        | _ ->

        let user_grades =
            Score.aggregate_user_ranked_grades user_id |> Map.ofSeq

        // todo: this implements AverageTop50 but should be a switch to future proof other calculation types
        let new_rating =
            charts
            |> Seq.choose (fun (chart, level) ->
                match user_grades.TryFind chart with
                | Some grade -> Some(Table.points table (level, grade))
                | None -> None
            )
            |> Seq.sortDescending
            |> Seq.truncate 50
            |> Seq.sum
            |> fun total -> total / 50.0

        TableRating.set user_id table_id new_rating

        new_rating

    let recalculate_affected_table_ratings (user_id: int64, chart_id: string) =
        for table_id in Backbeat.Tables.TABLES.Keys do
            match TableLevel.get table_id chart_id with
            | Some _ ->
                recalculate_table_rating (
                    user_id,
                    table_id,
                    Backbeat.Tables.TABLES.[table_id],
                    TableLevel.get_all table_id
                )
                |> ignore
            | None -> ()

    let get_leaderboard_details (table_id: string) =
        let table_rankings = TableRating.leaderboard table_id

        let users =
            table_rankings |> Array.map (fun x -> x.UserId) |> User.by_ids |> Map.ofArray

        table_rankings
        |> Array.indexed
        |> Array.choose (fun (i, table_ranking) ->
            match users.TryFind table_ranking.UserId with
            | None -> None
            | Some user ->

            Some(i, user, table_ranking.Rating)
        )