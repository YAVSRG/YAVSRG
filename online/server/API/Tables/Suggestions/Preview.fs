namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain
open Interlude.Web.Server.Domain.Old

module Preview =

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "table"

            let table = query_params.["table"].[0]

            if not (Backbeat.tables.ContainsKey table) then
                raise NotFoundException
            else

            match TableWithSuggestions.get table with
            | None -> raise NotFoundException
            | Some table ->

            response.ReplyJson({ Table = table }: Tables.Suggestions.Preview.Response)
        }
