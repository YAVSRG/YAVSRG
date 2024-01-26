namespace Interlude.Web.Server.API.Charts.Scores

open NetCoreServer
open Prelude
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module Save =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, _ = authorize headers

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Charts.Scores.Save.Request) -> // todo: basic clamp on how much data can be sent in one request (about 10kb?)

            let chart_id = request.ChartId.ToUpper()
            let timestamp = (System.DateTimeOffset.op_Implicit request.Timestamp).ToUnixTimeMilliseconds()

            match! Scores.submit (user_id, chart_id, request.Replay, request.Rate, request.Mods, timestamp) with
            | Scores.ScoreUploadOutcome.UploadFailed -> raise (BadRequestException None)
            | Scores.ScoreUploadOutcome.SongNotRecognised -> response.ReplyJson(None : Charts.Scores.Save.Response)
            | Scores.ScoreUploadOutcome.Unrated -> response.ReplyJson(None : Charts.Scores.Save.Response)
            | Scores.ScoreUploadOutcome.Rated (table_changes, leaderboard_changes) -> 

                response.ReplyJson(
                    Some
                        {
                            // todo: these will be quite sick when set up but aren't set up for now
                            LeaderboardChanges = 
                                leaderboard_changes
                                |> List.map (fun x -> { RulesetId = x.RulesetId; OldRank = x.OldRank; NewRank = x.NewRank })
                            TableChanges =
                                table_changes
                                |> List.map (fun x -> { Table = x.Table; OldPosition = None; NewPosition = (0, 0.0) })
                        }
                    : Charts.Scores.Save.Response
                )

                // todo: move this into a service too

                //// if ruleset and chart match a table, aggregate your new table rating and save to leaderboard
                //for table_id, table in Backbeat.tables |> Map.toSeq do
                //    if table.RulesetId = ruleset_id && table.Contains request.ChartId then

                //        let grades = Score.aggregate_table_grades user_id ruleset_id 1.0f

                //        let rating =
                //            table.Levels
                //            |> Seq.map (fun l -> l.Charts |> Seq.map (fun c -> l, c))
                //            |> Seq.concat
                //            |> Seq.choose (fun (level, chart) ->
                //                if grades.ContainsKey(chart.Hash) then
                //                    Some(table.Rating grades.[chart.Hash] (level, chart))
                //                else
                //                    None
                //            )
                //            |> Seq.sortDescending
                //            |> Seq.truncate 50
                //            |> Seq.sum
                //            |> fun total -> total / 50.0

                //        let old_position =
                //            match TableRanking.rank table_id user_id with
                //            | Some pos -> Some(pos, (TableRanking.rating table_id user_id).Value)
                //            | None -> None

                //        let new_rank = TableRanking.update table_id user_id rating

                //        table_changes <-
                //            {
                //                Table = table_id
                //                OldPosition = old_position
                //                NewPosition = new_rank, rating
                //            }
                //            :: table_changes
        }
