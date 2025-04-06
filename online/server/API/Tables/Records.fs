namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Prelude.Gameplay.Scoring
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Services
open Interlude.Web.Server.Domain.Backbeat

module Records =

    open Tables.Records

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

            let table_id = query_params.["table"].[0].ToLower()

            if not (Backbeat.Tables.exists table_id) then
                raise NotFoundException

            let table = Backbeat.Tables.TABLES.[table_id]

            // todo: aggregate grades and scores together
            let scores =
                Score.aggregate_user_ranked_scores target_user_id |> Map.ofArray

            let charts = TableLevel.get_all table_id |> Array.map fst
            let ruleset = Backbeat.rulesets.[table.RulesetId]

            response.ReplyJson(
                {
                    Scores =
                        charts
                        |> Seq.choose (fun chart ->
                            if scores.ContainsKey(chart) then
                                Some(
                                    {
                                        Hash = chart
                                        Score = scores.[chart]
                                        Grade =Grade.calculate ruleset.Grades scores.[chart]
                                    }
                                    : Score
                                )
                            else
                                None
                        )
                        |> Array.ofSeq
                }
                : Response
            )
        }