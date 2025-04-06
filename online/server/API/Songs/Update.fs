namespace Interlude.Web.Server.API.Songs

open NetCoreServer
open Percyqaz.Common
open Prelude
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Domain.Backbeat
open Interlude.Web.Server.Bot

module Update =

    open Songs.Update

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

            match Songs.song_by_id request.SongId with
            | Some (existing_song) ->
                Songs.update_song request.SongId (existing_song.MergeWithIncoming request.Song) |> ignore
                Logging.Info "Accepted changes to existing song '%s'" existing_song.Title
                response.ReplyJson(true)
            | None ->
                response.ReplyJson(false)
        }