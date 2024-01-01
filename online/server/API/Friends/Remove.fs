namespace Interlude.Web.Server.API.Friends

open NetCoreServer
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module Remove =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "user"
            let userId, _ = authorize headers

            match User.by_username query_params.["user"].[0] with
            | Some(id, user) ->
                Friends.remove_friend (userId, id)
                response.ReplyJson(true)
            | None -> response.ReplyJson(false)
        }
