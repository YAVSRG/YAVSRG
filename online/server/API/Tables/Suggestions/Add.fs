namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Prelude
open Percyqaz.Common
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Add =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let userId, user = authorize headers

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Tables.Suggestions.Add.Request) ->

            match Backbeat.tables.TryFind(request.TableFor) with
            | None -> response.ReplyJson(false) // Table not found
            | Some table ->

            match table.TryLevel(request.SuggestedLevel) with
            | None -> response.ReplyJson(false) // Suggested for a level that doesn't exist
            | _ ->

            match table.LevelOf(request.ChartId) with
            | Some level when level.Rank = request.SuggestedLevel -> response.ReplyJson(true) // Chart is already in this table, at this level. Discard suggestion
            | _ ->

            let timestamp = System.DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()

            match TableSuggestion.try_get_existing (request.ChartId, request.TableFor) with
            | Some(existing_id, existing_value) ->
                TableSuggestion.save (
                    existing_id,
                    { existing_value with
                        OsuBeatmapId = max existing_value.OsuBeatmapId request.OsuBeatmapId
                        EtternaPackId = max existing_value.EtternaPackId request.EtternaPackId
                        SuggestedLevels = Map.add userId request.SuggestedLevel existing_value.SuggestedLevels
                        LastUpdated = timestamp
                    }
                )

                Logging.Info(sprintf "Updated table suggestion #%i" existing_id)
                response.ReplyJson(true)
            | None ->

            let id =
                TableSuggestion.save_new
                    {
                        ChartId = request.ChartId
                        TableFor = request.TableFor
                        OsuBeatmapId = request.OsuBeatmapId
                        EtternaPackId = request.EtternaPackId
                        Artist = request.Artist
                        Title = request.Title
                        Difficulty = request.Difficulty
                        SuggestedLevels = Map.ofList [ userId, request.SuggestedLevel ]
                        LastUpdated = timestamp
                    }

            Logging.Info(sprintf "Saved new table suggestion #%i" id)
            response.ReplyJson(true)
        }
