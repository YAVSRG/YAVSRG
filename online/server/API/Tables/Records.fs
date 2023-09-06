namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Records =

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            let userId, _ = authorize headers
            
            if not (query_params.ContainsKey "user") then
                response.MakeErrorResponse(400, "'user' is required") |> ignore
            else

            match User.by_username query_params.["user"].[0] with
            | None -> response.MakeErrorResponse(404) |> ignore
            | Some (targetUserId, targetUser) ->

            match Charts.crescent with
            | None -> response.ReplyJson(
                {
                    Scores = [||]
                } : Tables.Records.Response)
            | Some table -> 
                let grades = Score.aggregate_table_grades targetUserId Score.SHORT_TERM_RULESET_LIST.[0] 1.0f
                let charts = table.Levels |> Seq.map (fun level -> level.Charts) |> Seq.concat
                response.ReplyJson(
                {
                    Scores = 
                        charts
                        |> Seq.choose (fun chart -> 
                            if grades.ContainsKey(chart.Hash) then Some ({ Hash = chart.Hash; Id = chart.Id; Grade = grades.[chart.Hash] } : Tables.Records.Score) else None)
                        |> Array.ofSeq
                } : Tables.Records.Response)
        }