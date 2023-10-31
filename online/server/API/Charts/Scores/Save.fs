namespace Interlude.Web.Server.API.Charts.Scores

open NetCoreServer
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Backbeat.Archive
open Prelude.Charts.Formats.Interlude
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Save =

    let fetch_chart = 
        let cache = Dictionary<string, Chart>()
        let http_client = new System.Net.Http.HttpClient()
        { new Async.Service<string, Chart option>() with
            override this.Handle(hash) =
                async {
                    if cache.ContainsKey hash then return Some cache.[hash] else

                    match Charts.by_hash hash with
                    | None -> return None
                    | Some (chart, song) ->
                 
                    let header = Archive.make_chart_header (chart, song)
                    let! message = http_client.GetAsync("https://cdn.yavsrg.net/" + hash) |> Async.AwaitTask
                    use stream = message.Content.ReadAsStream()
                    use br = new System.IO.BinaryReader(stream)
                    match Chart.readHeadless chart.Keys header "" br with
                    | Some chart ->
                        cache.[hash] <- chart
                        return Some chart
                    | None -> return None
                }
        }

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            // authorisation, validation, etc
            let userId, user = authorize headers

            match JSON.FromString body with
            | Error e -> 
                Logging.Error(sprintf "Error parsing body for api/charts/scores: %s" e.Message)
                response.MakeErrorResponse(400) |> ignore
            | Ok (request : Charts.Scores.Save.Request) -> // todo: basic clamp on how much data can be sent in one request (about 10kb?)

            let hash = request.ChartId.ToUpper()

            match Charts.by_hash hash with
            | None -> response.ReplyJson(None)
            | Some (chart_info, song) ->

            let timestamp = (System.DateTimeOffset.op_Implicit request.Timestamp).ToUnixTimeMilliseconds()
            if Score.exists userId timestamp then
                return response.ReplyJson(None) 
            else

            match! fetch_chart.RequestAsync(hash) with
            | None -> failwithf "Couldn't get note data for chart %s" hash
            | Some chart ->

            match Mods.check request.Mods with
            | Error _ -> 
                Logging.Debug("Rejecting score with invalid mods")
                response.MakeErrorResponse(400) |> ignore
            | Ok status when status >= Mods.ModStatus.Unstored -> response.ReplyJson(None)
            | Ok mod_status ->

            let rate = System.MathF.Round(request.Rate, 2)

            if rate < 0.5f || rate > 2.0f then 
                Logging.Debug("Rejecting score with invalid rate")
                response.MakeErrorResponse(400) |> ignore else

            // actually calculate what score was obtained

            let replay = Replay.decompress request.Replay // todo: zip bomb prevention?
            let modChart = Mods.getModChart request.Mods chart

            let mutable leaderboard_change : Charts.Scores.Save.LeaderboardChange option = None
            let mutable table_change : Charts.Scores.Save.TableChange option = None

            for ruleset_id in Score.SHORT_TERM_RULESET_LIST do
                let ruleset = Charts.rulesets.[ruleset_id]

                let scoring = Metrics.createScoreMetric ruleset chart.Keys (StoredReplayProvider replay) modChart.Notes rate
                scoring.Update Time.infinity

                let score : Score = 
                    {
                        UserId = userId
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

                let old_score = Leaderboard.score hash ruleset_id userId
                if mod_status = Mods.ModStatus.Ranked && rate >= 1.0f && (match old_score with Some s -> s < score.Score | None -> true) then
                    let old_rank = Leaderboard.rank hash ruleset_id userId
                    let replay = Leaderboard.Replay.create (request.Replay, rate, request.Mods, request.Timestamp)
                    Leaderboard.Replay.save hash ruleset_id userId replay
                    let new_rank = Leaderboard.add_score hash ruleset_id userId score.Score
                    leaderboard_change <- Some { RulesetId = ruleset_id; OldRank = old_rank; NewRank = new_rank }

                    // if ruleset and chart match a table, aggregate your new table rating and save to leaderboard
                    match Charts.crescent with
                    | None -> ()
                    | Some table -> 
                        if table.RulesetId <> ruleset_id || not (table.Contains request.ChartId) then () else

                        let grades = Score.aggregate_table_grades userId ruleset_id 1.0f
                        let rating =
                            table.Levels
                            |> Seq.map (fun l -> l.Charts |> Seq.map (fun c -> l, c))
                            |> Seq.concat
                            |> Seq.choose (fun (level, chart) -> 
                                if grades.ContainsKey(chart.Hash) then Some (table.Rating grades.[chart.Hash] (level, chart))
                                else None)
                            |> Seq.sortDescending
                            |> Seq.truncate 50
                            |> Seq.sum
                            |> fun total -> total / 50.0

                        let old_position = 
                            match TableRanking.rank "crescent" userId with
                            | Some pos -> Some (pos, (TableRanking.rating "crescent" userId).Value)
                            | None -> None

                        let new_rank = TableRanking.update "crescent" userId rating
                        table_change <- Some { Table = "crescent"; OldPosition = old_position; NewPosition = new_rank, rating }

            response.ReplyJson(Some { LeaderboardChange = leaderboard_change; TableChange = table_change } : Charts.Scores.Save.Response)
        }