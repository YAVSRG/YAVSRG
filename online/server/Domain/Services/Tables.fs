namespace Interlude.Web.Server.Domain.Services

open Prelude.Data.Charts.Tables
open Interlude.Web.Server
open Interlude.Web.Server.Domain.Objects

module Tables =

    let recalculate_table_rating (user_id: int64, table_id: string, table: Table) =

        match TableRating.get user_id table_id with
        | Some (existing, timestamp) when Timestamp.now() - timestamp < 300000 -> existing
        | _ ->

        let user_grades = Score.aggregate_user_ranked_grades user_id table.RulesetId |> Map.ofSeq

        let new_rating =
            table.Levels
            |> Seq.map (fun l -> l.Charts |> Seq.map (fun c -> l, c))
            |> Seq.concat
            |> Seq.choose (fun (level, chart) ->
                match user_grades.TryFind chart.Hash with
                | Some grade -> Some (table.Rating grade (level, chart))
                | None -> None
            )
            |> Seq.sortDescending
            |> Seq.truncate 50
            |> Seq.sum
            |> fun total -> total / 50.0

        TableRating.set user_id table_id new_rating

        new_rating

    let recalculate_affected_table_ratings (user_id: int64, chart_id: string) =
        for table_id, table in Backbeat.tables |> Map.toSeq do
            if table.Contains chart_id then
                recalculate_table_rating (user_id, table_id, table) |> ignore

    let get_leaderboard_details (table_id: string) =
        let table_rankings = TableRating.leaderboard table_id
        let users = table_rankings |> Array.map (fun x -> x.UserId) |> User.by_ids |> Map.ofArray

        table_rankings
        |> Array.indexed
        |> Array.choose (fun (i, table_ranking) -> 
            match users.TryFind table_ranking.UserId with
            | None -> None
            | Some user ->

            Some (i, user, table_ranking.Rating)
        )