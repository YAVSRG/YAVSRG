namespace Interlude.Web.Shared.Requests

open Percyqaz.Json

type HttpMethod =
    | GET
    | POST
    | DELETE

module Auth =
    
    /// url parameters:
    ///  code - given by discord auth to identify you
    ///  state - given by server to identify which Interlude client is logging in
    module Discord =

        let ROUTE = (GET, "/auth/discord")

module Charts =

    /// url parameters:
    ///  chart - hash of chart to get details for
    module Identify =

        let ROUTE = (GET, "/charts/identify")

        [<Json.AutoCodec>]
        type Info =
            {
                Song: Prelude.Backbeat.Archive.Song
                Chart: Prelude.Backbeat.Archive.Chart
                Mirrors: string list
            }

        [<Json.AutoCodec>]
        type Response =
            {
                Info: Info option
            }
            
    module Scores =

        /// requires login token as Authorization header
        module Save =

            let ROUTE = (POST, "/charts/scores")

            [<Json.AutoCodec>]
            type Request =
                {
                    ChartId: string
                    Replay: string
                    Rate: float32
                    Mods: Prelude.Gameplay.Mods.ModState
                    Timestamp: System.DateTime
                }
                
        /// requires login token as Authorization header
        /// url parameters:
        ///  chart - hash of chart to get details for
        ///  ruleset - id of ruleset to get details for
        module Leaderboard =

            let ROUTE = (GET, "/charts/scores")

            [<Json.AutoCodec>]
            type Score =
                {
                    Username: string
                    Rank: int
                    Replay: string
                    Rate: float32
                    Mods: Prelude.Gameplay.Mods.ModState
                    Timestamp: System.DateTime
                }

            [<Json.AutoCodec>]
            type Response =
                {
                    RulesetId: string
                    Scores: Score array
                }

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
    module Add =
        
        let ROUTE = (POST, "/friends")

        [<Json.AutoCodec>]
        type Request = { User: string }

    /// requires login token as Authorization header
    /// url parameters:
    ///  user - name of user to add as friend (not case sensitive)
    module Remove =
            
        let ROUTE = (DELETE, "/friends")

module Health =

    module HealthCheck =

        let ROUTE = (GET, "/health")

        [<Json.AutoCodec>]
        type Response = { Status: string }