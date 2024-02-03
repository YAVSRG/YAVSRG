namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Percyqaz.Common
open Prelude
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Domain.Services

module Accept =

    open Tables.Suggestions.Accept

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, user = authorize headers

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Request) ->

            if not (Backbeat.Tables.exists request.TableId) then raise NotFoundException
            if not (user.Badges.Contains Badge.TABLE_EDITOR) then raise PermissionDeniedException

            let chart_id = request.ChartId.ToUpper()
            match Backbeat.Charts.by_hash chart_id with
            | Some (known_chart, _) ->

                if known_chart.Keys <> Backbeat.Tables.TABLES.[request.TableId].Keymode then

                    Logging.Debug(sprintf "Cannot accept chart %A into table %s because keymode doesn't match" known_chart request.TableId)
                    TableSuggestion.reject request.TableId chart_id user_id "Wrong keymode" |> ignore
                    response.ReplyJson(false)

                elif TableSuggestion.accept request.TableId chart_id user_id request.Level then

                    TableLevel.add_or_move user_id request.TableId chart_id request.Level
                    Leaderboard.create chart_id Backbeat.Tables.TABLES.[request.TableId].RulesetId
                    response.ReplyJson(true)

                else

                    response.ReplyJson(false)

            | None -> response.ReplyJson(false)
        }
