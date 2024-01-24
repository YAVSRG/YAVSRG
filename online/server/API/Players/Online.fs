namespace Interlude.Web.Server.API.Players

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Online
open Interlude.Web.Server.Domain

module Online =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            let _, _ = authorize headers

            let online = Session.list_online_users ()
            let users = User.by_ids (online |> Array.map fst)

            response.ReplyJson(
                {
                    Players =
                        users
                        |> Array.choose (
                            Option.map (fun x ->
                                {
                                    Username = x.Username
                                    Color = x.Color |> Option.defaultValue Badge.DEFAULT_COLOR
                                }
                            )
                        )
                }
                : Players.Online.Response
            )
        }
