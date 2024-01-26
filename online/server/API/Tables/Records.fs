namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Objects
open Interlude.Web.Server.Domain.Services

module Records =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "user"
            require_query_parameter query_params "table"
            let user_id, _ = authorize headers

            match User.by_username query_params.["user"].[0] with
            | None -> raise NotFoundException
            | Some(target_user_id, _) ->

            match Backbeat.tables.TryFind(query_params.["table"].[0]) with
            | None -> response.MakeErrorResponse(404) |> ignore
            | Some table ->

            let scores = Score.aggregate_user_ranked_scores target_user_id table.RulesetId |> Map.ofArray

            let charts = table.Levels |> Seq.map (fun level -> level.Charts) |> Seq.concat
            let ruleset = Backbeat.rulesets.[table.RulesetId]

            response.ReplyJson(
                {
                    Scores =
                        charts
                        |> Seq.choose (fun chart ->
                            if scores.ContainsKey(chart.Hash) then
                                Some(
                                    {
                                        Hash = chart.Hash
                                        Id = chart.Id
                                        Score = scores.[chart.Hash]
                                        Grade =
                                            (Prelude.Gameplay.Grade.calculate_with_target
                                                ruleset.Grading.Grades
                                                scores.[chart.Hash])
                                                .Grade
                                    }
                                    : Tables.Records.Score
                                )
                            else
                                None
                        )
                        |> Array.ofSeq
                }
                : Tables.Records.Response
            )
        }
