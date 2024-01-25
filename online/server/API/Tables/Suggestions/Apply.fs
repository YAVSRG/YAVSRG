namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Percyqaz.Common
open Prelude
open Prelude.Data.Charts.Tables
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Old
open Interlude.Web.Server.Domain

module Apply =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, user = authorize headers

            if not (user.Badges.Contains Objects.Badge.TABLE_EDITOR) then
                Logging.Error(sprintf "User '%s' doesn't have permission to apply table suggestions" user.Username)
                raise PermissionDeniedException
            else

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Tables.Suggestions.Apply.Request) ->

            match TableSuggestion.by_id request.Id with
            | None -> response.ReplyJson(false)
            | Some suggestion ->

            match TableWithSuggestions.get suggestion.TableFor with
            | None ->
                Logging.Error(sprintf "Couldn't find stored table for '%s'" suggestion.TableFor)
                response.ReplyJson(false)
            | Some table ->

            match Backbeat.Charts.by_hash suggestion.ChartId with
            | None ->
                Logging.Error(sprintf "Can't apply suggestion since this chart isn't in the backbeat database")
                response.ReplyJson(false)
            | Some(chart, song) ->

            if suggestion.SuggestedLevels.Values.Contains(request.Level) then
                response.ReplyJson(true)

                if table.RemoveChart(suggestion.ChartId) then
                    Logging.Info(
                        sprintf
                            "Moving '%s' to level %i, closing suggestion #%i"
                            suggestion.Title
                            request.Level
                            request.Id
                    )
                else
                    Logging.Info(
                        sprintf
                            "Adding '%s' to level %i, closing suggestion #%i"
                            suggestion.Title
                            request.Level
                            request.Id
                    )

                let result =
                    table.AddChart(
                        request.Level,
                        Table.generate_cid (chart.FormattedCreators, song.Title),
                        chart.Keys,
                        suggestion.ChartId
                    )

                TableWithSuggestions.update (
                    suggestion.TableFor,
                    { table with
                        Version = Table.generate_table_version ()
                    }
                )

                TableSuggestion.delete request.Id

                response.ReplyJson(result)
            else
                response.ReplyJson(false)
        }
