namespace Interlude.Web.Server.API.Friends

open NetCoreServer
open System.Net.Http
open Percyqaz.Json
open Interlude.Web.Shared.API
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain

module List =

    let ROUTE = (GET, "/friends")

    [<Json.AutoCodec>]
    type Friend =
        {
            Username: string
        }

    [<Json.AutoCodec>]
    type Response =
        {
            Friends: Friend array
        }

    let handle (body: string, query_params: Map<string, string array>, headers: Map<string, string>, response: HttpResponse) = 
        async {
            let userId, _ = authorize headers

            let friends = Friends.friends_list userId
            response.ReplyJson(
                {
                    Friends = friends |> Array.choose (Option.map (fun x -> { Username =  x.Username } )) 
                })
        }