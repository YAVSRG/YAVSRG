namespace Interlude.Web.Server.API

open NetCoreServer
open Prelude

[<AutoOpen>]
module Utils =
    
    type HttpResponse with
        member this.ReplyJson<'T>(data: 'T) =
            this.MakeGetResponse(JSON.ToString data, "application/json")
            |> ignore
        member this.ReplyRedirect(url: string) =
            this.Clear().SetBegin(303).SetHeader("Location", url).SetBody()
            |> ignore