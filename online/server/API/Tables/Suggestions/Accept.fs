namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Percyqaz.Common
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server
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

            if not (user.Badges.Contains Badge.TABLE_EDITOR) then
                raise PermissionDeniedException

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Request) ->

            match Backbeat.Tables.get request.TableId with
            | None -> raise NotFoundException
            | Some table ->

            let chart_id = request.ChartId.ToUpper()

            match Backbeat.Charts.by_hash chart_id with
            | Some(known_chart, known_song) ->

                if known_chart.Keys <> table.Keymode then

                    Logging.Debug
                        "Cannot accept chart %A into table %s because keymode doesn't match"
                        known_chart
                        request.TableId

                    TableSuggestion.reject request.TableId chart_id user_id "Wrong keymode"
                    |> ignore

                    response.ReplyJson(false)

                elif TableSuggestion.accept request.TableId chart_id user_id request.Level then

                    TableLevel.add_or_move user_id request.TableId chart_id request.Level

                    sprintf "# :checkered_flag: %s\nNow in %s, %s"
                        known_song.FormattedTitle
                        table.Name
                        (table.LevelName request.Level)
                    |> Discord.feed_log

                    response.ReplyJson(true)

                else

                    response.ReplyJson(false)

            | None -> response.ReplyJson(false)
        }