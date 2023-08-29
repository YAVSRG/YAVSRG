namespace Interlude.Web.Server.API.Charts

open NetCoreServer
open Percyqaz.Common
open Prelude
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module SetScore =

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            let userId, user = authorize headers

            match JSON.FromString body with
            | Error e -> Logging.Error(sprintf "Error parsing body for Charts/SetScore: %s" e.Message)
            | Ok (request : Charts.SetScore.Request) ->

            let hash = request.ChartId

            let existing_leaderboard_rulesets = Leaderboard.rulesets_by_hash hash

            if existing_leaderboard_rulesets.Length > 0 then
            
                // get the chart data

                // save score
                Score.save userId hash (Score.create (request.Replay, request.Rate, request.Mods, request.Timestamp))

                // update leaderboard(s)
                for ruleset in existing_leaderboard_rulesets do
                    let score = 0.95 // score calculation using ruleset and chart notes
                    Leaderboard.add_score userId score hash ruleset

                response.ReplyJson(true)
            else response.ReplyJson(false)
        }