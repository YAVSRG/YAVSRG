namespace Interlude.Web.Server.API.Charts

open NetCoreServer
open Percyqaz.Common
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Bot

module Add =

    open Charts.Add

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, user = authorize headers

            if not (user.Badges.Contains Badge.DEVELOPER) then
                raise PermissionDeniedException

            match JSON.FromString body with
            | Error e -> raise (BadRequestException None)
            | Ok(request: Request) ->

            let chart_id = request.ChartId.ToUpper()

            match Songs.chart_and_song_by_id chart_id with
            | Some (song_id, existing_chart, existing_song) ->
                Songs.update_song song_id (existing_song.MergeWithIncoming request.Song) |> ignore
                Songs.update_chart chart_id (existing_chart.MergeWithIncoming request.Chart) |> ignore
                Logging.Info "Accepted changes to existing chart + song '%s'" existing_song.Title
            | None ->

                match Songs.search_songs 2L 0 request.Song.FormattedTitle |> Array.tryExactlyOne with
                | Some (song_id, existing_song) ->
                    if existing_song.Title = request.Song.Title && existing_song.Artists = request.Song.Artists then
                        Songs.update_song song_id (existing_song.MergeWithIncoming request.Song) |> ignore
                        Songs.add_chart chart_id request.Chart song_id
                        Logging.Info "Accepted new chart for existing song '%s'" existing_song.Title
                    else
                        let new_id = Songs.add_chart_song chart_id request.Chart request.Song
                        Bot.create_admin_prompt (AdminInteractables.do_songs_match (song_id, existing_song, new_id, request.Song))
                        Logging.Info "Accepted new chart for possibly new song '%s'" request.Song.Title
                | None ->
                    Songs.add_chart_song chart_id request.Chart request.Song |> ignore
                    Logging.Info "Accepted new chart for new song '%s'" request.Song.Title

            response.ReplyJson(true)
        }