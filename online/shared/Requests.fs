namespace Interlude.Web.Shared.Requests

open Percyqaz.Json
open Interlude.Web.Shared.API

module Auth =
    
    /// url parameters:
    ///  code - given by discord auth to identify you
    ///  state - given by server to identify which Interlude client is logging in
    module Discord =

        let ROUTE = (GET, "/auth/discord")

module Friends =

    /// requires login token as Authorization header
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

        
    /// requires login token as Authorization header
    /// url parameters:
    ///  user - name of user to add as friend (not case sensitive)
    module Add =
        
        let ROUTE = (POST, "/friends")

    /// requires login token as Authorization header
    /// url parameters:
    ///  user - name of user to add as friend (not case sensitive)
    module Remove =
            
        let ROUTE = (DELETE, "/friends")
    

module Charts =

    /// url parameters
    ///  id - hash of chart to get details for
    module Identify =

        let ROUTE = (GET, "/charts/identify")

        [<Json.AutoCodec>]
        type Response =
            {
                Found: bool
                Song: Prelude.Backbeat.Archive.Song
                Chart: Prelude.Backbeat.Archive.Chart
                Mirrors: string list
            }

module Health =

    module HealthCheck =

        let ROUTE = (GET, "/health")

        [<Json.AutoCodec>]
        type Response = { Status: string }