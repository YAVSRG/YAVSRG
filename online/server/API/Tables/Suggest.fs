namespace Interlude.Web.Server.API.Tables

open NetCoreServer
open Prelude
open Percyqaz.Common
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Suggest =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let userId, user = authorize headers

            if not (user.Badges.Contains Badge.TABLE_EDITOR) then
                Logging.Error(sprintf "User '%s' doesn't have permission to suggest for tables" user.Username)
                response.ReplyJson(false)
            else

            match JSON.FromString body with
            | Error e ->
                Logging.Error(sprintf "Error parsing body for api/tables/suggest: %s" e.Message)
                response.ReplyJson(false)
            | Ok(request: Tables.Suggest.Request) ->

            if request.TableFor <> "crescent" then
                response.ReplyJson(false)
            else if

                TableAddSuggestion.exists (request.ChartId, request.TableFor)
            then
                response.ReplyJson(true)
            else

            let timestamp = System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

            let id =
                TableAddSuggestion.save_new
                    {
                        UserId = userId
                        ChartId = request.ChartId
                        OsuBeatmapId = request.OsuBeatmapId
                        EtternaPackId = request.EtternaPackId
                        Artist = request.Artist
                        Title = request.Title
                        Difficulty = request.Difficulty
                        TableFor = request.TableFor
                        SuggestedLevel = request.SuggestedLevel
                        Timestamp = timestamp
                    }

            Logging.Info(sprintf "Saved table suggestion #%i" id)
            response.ReplyJson(true)
        }
