namespace Interlude.Web.Server.API.Tables.Suggestions

open NetCoreServer
open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server.API
open Interlude.Web.Server.Domain.Services

module Missing =

    open Tables.Suggestions.Missing

    let handle
        (
            body: string,
            query_params: Map<string, string array>,
            headers: Map<string, string>,
            response: HttpResponse
        ) =
        async {
            require_query_parameter query_params "table"
            let _, _ = authorize headers

            let table_id = query_params.["table"].[0].ToLower()

            if not (Backbeat.Tables.exists table_id) then
                raise NotFoundException
            else

            // not yet implemented
            response.ReplyJson({ Charts = [||] }: Response)
        }