namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Records =

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            let userId, _ = authorize headers

            // todo: table id parameter that selects which table we are asking about
            
            if not (query_params.ContainsKey "user") then
                response.MakeErrorResponse(400, "'user' is required") |> ignore
            else
                
            if not (query_params.ContainsKey "table") then
                response.MakeErrorResponse(400, "'table' is required") |> ignore
            else

            match User.by_username query_params.["user"].[0] with
            | None -> raise NotFoundException
            | Some (targetUserId, _) ->

            if query_params.["table"].[0] <> "crescent" then response.MakeErrorResponse(404) |> ignore else

            match Charts.crescent with
            | None -> response.ReplyJson(
                {
                    Scores = [||]
                } : Tables.Records.Response)
            | Some table -> 
                let scores = Score.aggregate_table_scores targetUserId Score.SHORT_TERM_RULESET_LIST.[0] 1.0f
                let charts = table.Levels |> Seq.map (fun level -> level.Charts) |> Seq.concat
                let ruleset = Charts.rulesets.[Score.SHORT_TERM_RULESET_LIST.[0]]
                response.ReplyJson(
                {
                    Scores = 
                        charts
                        |> Seq.choose (fun chart -> 
                            if scores.ContainsKey(chart.Hash) then 
                                Some ({ 
                                    Hash = chart.Hash
                                    Id = chart.Id
                                    Score = scores.[chart.Hash]
                                    Grade = (Prelude.Gameplay.Grade.calculate_with_target ruleset.Grading.Grades scores.[chart.Hash]).Grade
                                } : Tables.Records.Score) else None)
                        |> Array.ofSeq
                } : Tables.Records.Response)
        }