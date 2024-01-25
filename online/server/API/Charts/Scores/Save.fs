namespace Interlude.Web.Server.API.Charts.Scores

open NetCoreServer
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Charts
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain
open Interlude.Web.Server.Domain.Old

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

            let hash = request.ChartId.ToUpper()

            match Backbeat.Charts.by_hash hash with
            | None -> response.ReplyJson((None : Charts.Scores.Save.Response option))
            | Some(chart_info, song) ->

            let timestamp =
                (System.DateTimeOffset.op_Implicit request.Timestamp).ToUnixTimeMilliseconds()

            if Score.exists user_id timestamp then
                return response.ReplyJson((None : Charts.Scores.Save.Response option))
            else

            match! Backbeat.Charts.fetch.RequestAsync(hash) with
            | None -> failwithf "Couldn't get note data for chart %s" hash
            | Some chart ->

            match Mods.check request.Mods with
            | Error _ ->
                Logging.Debug("Rejecting score with invalid mods")
                response.MakeErrorResponse(400) |> ignore
            | Ok status when status >= Mods.ModStatus.Unstored -> response.ReplyJson((None : Charts.Scores.Save.Response option))
            | Ok mod_status ->

            let rate = System.MathF.Round(request.Rate, 2)

            if rate < 0.5f || rate > 2.0f then
                Logging.Debug("Rejecting score with invalid rate")
                response.MakeErrorResponse(400) |> ignore
            else

                // actually calculate what score was obtained

                let replay = Replay.decompress_string request.Replay // todo: zip bomb prevention?
                let modChart = Mods.apply_mods request.Mods chart

                let mutable leaderboard_changes: Charts.Scores.Save.LeaderboardChange list = []
                let mutable table_changes: Charts.Scores.Save.TableChange list = []

                for ruleset_id in Score.RULESETS do
                    let ruleset = Backbeat.rulesets.[ruleset_id]

                    let scoring =
                        Metrics.create ruleset chart.Keys (StoredReplayProvider replay) modChart.Notes rate

                    scoring.Update Time.infinity

                    let score: Score =
                        {
                            UserId = user_id
                            ChartId = hash
                            RulesetId = ruleset_id
                            Score = scoring.Value
                            Grade = Grade.calculate ruleset.Grading.Grades scoring.State
                            Lamp = Lamp.calculate ruleset.Grading.Lamps scoring.State
                            Rate = rate
                            Mods = request.Mods
                            Timestamp = timestamp
                        }

                    Score.save_new score |> ignore

                    let old_score = Leaderboard.score hash ruleset_id user_id

                    if
                        mod_status = Mods.ModStatus.Ranked
                        && rate >= 1.0f
                        && (
                            match old_score with
                            | Some s -> s < score.Score
                            | None -> true
                        )
                    then
                        let old_rank = Leaderboard.rank hash ruleset_id user_id

                        let replay =
                            Leaderboard.Replay.create (request.Replay, rate, request.Mods, request.Timestamp)

                        Leaderboard.Replay.save hash ruleset_id user_id replay
                        let new_rank = Leaderboard.add_score hash ruleset_id user_id score.Score

                        leaderboard_changes <-
                            {
                                RulesetId = ruleset_id
                                OldRank = old_rank
                                NewRank = new_rank
                            }
                            :: leaderboard_changes

                        // if ruleset and chart match a table, aggregate your new table rating and save to leaderboard
                        for table_id, table in Backbeat.tables |> Map.toSeq do
                            if table.RulesetId = ruleset_id && table.Contains request.ChartId then

                                let grades = Score.aggregate_table_grades user_id ruleset_id 1.0f

                                let rating =
                                    table.Levels
                                    |> Seq.map (fun l -> l.Charts |> Seq.map (fun c -> l, c))
                                    |> Seq.concat
                                    |> Seq.choose (fun (level, chart) ->
                                        if grades.ContainsKey(chart.Hash) then
                                            Some(table.Rating grades.[chart.Hash] (level, chart))
                                        else
                                            None
                                    )
                                    |> Seq.sortDescending
                                    |> Seq.truncate 50
                                    |> Seq.sum
                                    |> fun total -> total / 50.0

                                let old_position =
                                    match TableRanking.rank table_id user_id with
                                    | Some pos -> Some(pos, (TableRanking.rating table_id user_id).Value)
                                    | None -> None

                                let new_rank = TableRanking.update table_id user_id rating

                                table_changes <-
                                    {
                                        Table = table_id
                                        OldPosition = old_position
                                        NewPosition = new_rank, rating
                                    }
                                    :: table_changes

                response.ReplyJson(
                    Some
                        {
                            LeaderboardChanges = leaderboard_changes
                            TableChanges = table_changes
                        }
                    : Charts.Scores.Save.Response
                )
        }
