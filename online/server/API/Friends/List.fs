namespace Interlude.Web.Server.API.Friends

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain
open Interlude.Web.Server.Online

module List =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let userId, _ = authorize headers

            let friends = Friends.friends_list userId
            let! online = LoggedInUsers.find_sessions (friends |> Array.map (fun u -> u.Username))

            response.ReplyJson(
                {
                    Friends =
                        Array.zip friends online
                        |> Array.map (fun (friend, session) ->
                            {
                                Username = friend.Username
                                Color = friend.Color |> Option.defaultValue Badge.DEFAULT_COLOR
                                Online = session.IsSome
                            }
                        )
                }
                : Friends.List.Response
            )
        }
