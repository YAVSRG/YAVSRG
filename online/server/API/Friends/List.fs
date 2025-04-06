namespace Interlude.Web.Server.API.Friends

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Core
open Interlude.Web.Server.Online

module List =

    open Friends.List

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let user_id, _ = authorize headers

            let friends = Friends.friends_list user_id

            let online =
                Session.find_session_ids_by_usernames (friends |> Array.map (fun (id, u) -> u.Username))

            response.ReplyJson(
                {
                    Friends =
                        Array.zip friends online
                        |> Array.map (fun ((_, friend), session) ->
                            {
                                Username = friend.Username
                                Color = friend.Color
                                Online = session.IsSome
                            }
                        )
                }
                : Response
            )
        }