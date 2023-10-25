namespace Interlude.Web.Shared.Requests

open Percyqaz.Json
open Interlude.Web.Shared.API

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

        let get(chart: string, callback: Response option -> unit) = 
            Client.get<Response>(snd ROUTE + "?chart=" + chart, callback)
            
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

            [<Json.AutoCodec>]
            type LeaderboardChange =
                {
                    RulesetId: string
                    OldRank: int64 option
                    NewRank: int64
                }

            [<Json.AutoCodec>]
            type TableChange =
                {
                    Table: string
                    OldPosition: (int64 * float) option
                    NewPosition: (int64 * float)
                }

            [<Json.AutoCodec>]
            type Response =
                {
                    // todo: in future, one chart could be in multiple leaderboards
                    LeaderboardChange: LeaderboardChange option
                    // todo: in future, one chart could be in multiple tables
                    TableChange: TableChange option
                }

            let post(request: Request, callback: Response option -> unit) = 
                Client.post_return<Request, Response>(snd ROUTE, request, callback)
                
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
            
            let get(chart: string, ruleset: string, callback: Response option -> unit) = 
                Client.get<Response>(snd ROUTE + "?chart=" + chart + "&ruleset=" + escape ruleset, callback)

module Tables =

    /// requires login token as Authorization header
    /// url parameters:
    ///  user - name of user to get records for
    ///  table - id of table to get records for e.g 'crescent' or 'mizu'
    module Records =

        let ROUTE = (GET, "/tables/records")

        [<Json.AutoCodec>]
        type Score =
            {
                Id: string
                Hash: string
                Score: float
                Grade: int
            }

        [<Json.AutoCodec>]
        type Response = { Scores: Score array }
        
        let get(user: string, table: string, callback: Response option -> unit) = 
            Client.get<Response>(snd ROUTE + "?user=" + escape user + "&table=" + escape table, callback)
    
    /// requires login token as Authorization header
    /// url parameters:
    ///  table - id of table to get ranking for e.g 'crescent' or 'mizu'
    module Leaderboard =

        let ROUTE = (GET, "/tables/ranking")

        [<Json.AutoCodec>]
        type Player =
            {
                Username: string
                Color: int32
                Rank: int
                Rating: float
            }

        [<Json.AutoCodec>]
        type Response = { Players: Player array }
        
        let get(table: string, callback: Response option -> unit) = 
            Client.get<Response>(snd ROUTE + "?table=" + escape table, callback)
    
    /// requires login token as Authorization header
    module Suggest =
    
        let ROUTE = (POST, "/tables/suggestions")

        [<Json.AutoCodec>]
        type Request =
            {
                ChartId: string
                OsuBeatmapId: int
                EtternaPackId: int
                Artist: string
                Title: string
                Difficulty: string
                TableFor: string
                SuggestedLevel: int
            }
            
        let post(request: Request, callback: bool -> unit) =
            Client.post<Request>(snd ROUTE, request, callback)
    
    /// url parameters:
    ///  table - id of table to get ranking for e.g 'crescent' or 'mizu'
    module Suggestions =
    
        let ROUTE = (GET, "/tables/suggestions")
        
        [<Json.AutoCodec>]
        type Suggestion = 
            {
                ChartId: string
                OsuBeatmapId: int
                EtternaPackId: int
                Artist: string
                Title: string
                Difficulty: string
                SuggestedLevel: int
            }

        [<Json.AutoCodec>]
        type Response = { Suggestions: Suggestion array }
        
        let get(table: string, callback: Response option -> unit) = 
            Client.get<Response>(snd ROUTE + "?table=" + escape table, callback)

module Players =

    /// requires login token as Authorization header
    module Online =

        let ROUTE = (GET, "/players/online")

        [<Json.AutoCodec>]
        type Player =
            {
                Username: string
                Color: int
            }

        [<Json.AutoCodec>]
        type Response =
            {
                Players: Player array
            }
            
        let get(callback: Response option -> unit) = 
            Client.get<Response>(snd ROUTE, callback)
        
    /// requires login token as Authorization header
    /// url parameters:
    ///  user - OPTIONAL: name of user to view profile of (not case sensitive)
    module Profile =

        let ROUTE = (GET, "/players/profile")

        [<Json.AutoCodec>]
        type Badge =
            {
                Name: string
                Colors: int32 list
            }
                
        [<Json.AutoCodec>]
        type RecentScore =
            {
                Artist: string
                Title: string
                Difficulty: string
                Score: float
                Lamp: string
                Mods: string
                Timestamp: int64
            }

        [<Json.AutoCodec>]
        type Response =
            {
                Username: string
                Color: int
                Badges: Badge array
                RecentScores: RecentScore array
                DateSignedUp: int64
            }
            
        let get_me(callback: Response option -> unit) = 
            Client.get<Response>(snd ROUTE, callback)

        let get(user: string, callback: Response option -> unit) =
            Client.get<Response>(snd ROUTE + "?user=" + escape user, callback)
    
    /// requires login token as Authorization header
    module ProfileOptions =

        let ROUTE = (POST, "/players/profile/options")

        [<Json.AutoCodec>]
        type Request = { Color: int }
        
        let post(request: Request, callback: bool -> unit) =
            Client.post<Request>(snd ROUTE, request, callback)

module Friends =

    /// requires login token as Authorization header
    module List =

        let ROUTE = (GET, "/friends")

        [<Json.AutoCodec>]
        type Friend =
            {
                Username: string
                Color: int
            }

        [<Json.AutoCodec>]
        type Response =
            {
                Friends: Friend array
            }
            
        let get(callback: Response option -> unit) =
            Client.get<Response>(snd ROUTE, callback)
        
    /// requires login token as Authorization header
    module Add =
        
        let ROUTE = (POST, "/friends")

        [<Json.AutoCodec>]
        type Request = { User: string }

        let post(request: Request, callback: bool -> unit) =
            Client.post<Request>(snd ROUTE, request, callback)

    /// requires login token as Authorization header
    /// url parameters:
    ///  user - name of user to add as friend (not case sensitive)
    module Remove =
            
        let ROUTE = (DELETE, "/friends")

        let delete(user: string, callback: bool -> unit) =
            Client.delete(snd ROUTE + "?user=" + escape user, callback)

module Health =

    module HealthCheck =

        let ROUTE = (GET, "/health")

        [<Json.AutoCodec>]
        type Response = { Status: string }

        let get(callback: Response option -> unit) =
            Client.get<Response>(snd ROUTE, callback)