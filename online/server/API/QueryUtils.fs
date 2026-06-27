namespace Interlude.Web.Server.API

module [<AutoOpen>] QueryUtils =

    let require_query_parameter (query_params: Map<string, string array>) (name: string) =
        if not (query_params.ContainsKey name) then
            raise (BadRequestException(Some(sprintf "'%s' is required" name)))