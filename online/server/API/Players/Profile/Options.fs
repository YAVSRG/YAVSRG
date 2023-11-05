namespace Interlude.Web.Server.API.Players.Profile

open NetCoreServer
open Prelude
open Percyqaz.Common
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Options =

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
            | Error e -> Logging.Error(sprintf "Error parsing body for api/players/profile: %s" e.Message)
            | Ok(request: Players.Profile.Options.Request) ->

            if
                user.Badges
                |> Seq.map Badge.badge_color
                |> Seq.concat
                |> Seq.contains request.Color
            then
                User.save (userId, { user with Color = Some request.Color })
                response.ReplyJson(true)
            else
                response.ReplyJson(false)
        }
