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
            let userId, user = authorize headers

            match JSON.FromString body with
            | Error e -> 
                Logging.Error(sprintf "Error parsing body for api/charts/scores: %s" e.Message)
                response.MakeErrorResponse(400) |> ignore
            | Ok (request : Charts.Scores.Save.Request) ->
            
            // todo: basic clamp on how much data can be sent in one request (about 10kb?)

            let hash = request.ChartId.ToUpper()

            match Charts.by_hash hash with
            | None -> response.ReplyJson(false)
            | Some (chart_info, song) ->

            let timestamp = (System.DateTimeOffset.op_Implicit request.Timestamp).ToUnixTimeMilliseconds()
            if Score.exists userId timestamp then
                Logging.Debug(sprintf "Score with timestamp by this user exists: %i" timestamp)
                return response.ReplyJson(true) 
            else

            match! fetch_chart.RequestAsync(hash) with
            | None -> failwithf "Couldn't get note data for chart %s" hash
            | Some chart ->

            match Mods.check request.Mods with
            | Error _ -> 
                Logging.Debug("Rejecting score with invalid mods")
                response.MakeErrorResponse(400) |> ignore
            | Ok status when status >= Mods.ModStatus.Unstored -> response.ReplyJson(false)
            | Ok _ ->

            // todo: zip bomb prevention?
            let replay = Replay.decompress request.Replay
            let modChart = Mods.getModChart request.Mods chart
            let rate = System.MathF.Round(request.Rate, 2)

            if rate < 0.5f || rate > 2.0f then 
                Logging.Debug("Rejecting score with invalid rate")
                response.MakeErrorResponse(400) |> ignore else

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

                if rate >= 1.0f && (match Leaderboard.score userId hash ruleset_id with Some s -> s < score.Score | None -> true) then
                    match Leaderboard.Replay.create (request.Replay, rate, request.Mods, request.Timestamp) with
                    | Ok replay ->
                        Leaderboard.Replay.save hash ruleset_id userId replay
                        Leaderboard.add_score hash ruleset_id userId score.Score
                    | Error _ -> ()

            response.ReplyJson(true)
        }