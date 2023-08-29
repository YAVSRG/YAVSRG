namespace Interlude.Web.Server.API.Friends

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module List =

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            let userId, _ = authorize headers

            let friends = Friends.friends_list userId
            response.ReplyJson(
                {
                    Friends = friends |> Array.choose (Option.map (fun x -> { Username =  x.Username } )) 
                } : Friends.List.Response)
        }