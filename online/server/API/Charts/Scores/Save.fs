namespace Interlude.Web.Server.API.Charts.Scores

open NetCoreServer
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Mods
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
            let userId, _ = authorize headers

            match JSON.FromString body with
            | Error e -> Logging.Error(sprintf "Error parsing body for api/charts/scores: %s" e.Message)
            | Ok (request : Charts.Scores.Save.Request) ->

            let hash = request.ChartId.ToUpper()

            let existing_leaderboard_rulesets = Leaderboard.rulesets_by_hash hash

            if existing_leaderboard_rulesets.Length > 0 then
            
                match! fetch_chart.RequestAsync(hash) with
                | None -> failwithf "Couldn't get note data for chart %s, even though it has leaderboards" hash
                | Some chart ->

                // todo: basic clamp on how much data can be sent in one request (about 10kb?)

                let score = Score.create (request.Replay, request.Rate, request.Mods, request.Timestamp)
                Score.save userId hash score

                for ruleset_id in existing_leaderboard_rulesets do

                    // todo: zip bomb shielding
                    let replay = Replay.decompress score.Replay
                    let modChart = getModChart score.Mods chart
                    let scoring = Metrics.createScoreMetric Charts.rulesets.[ruleset_id] chart.Keys (StoredReplayProvider replay) modChart.Notes score.Rate
                    scoring.Update Time.infinity
                    let score = scoring.Value

                    Leaderboard.add_score userId score hash ruleset_id

                response.ReplyJson(true)
            else response.ReplyJson(false)
        }