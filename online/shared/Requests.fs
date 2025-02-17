namespace Interlude.Web.Shared.Requests

open Percyqaz.Data
open Prelude
open Prelude.Mods
open Interlude.Web.Shared.API

module Health =

    module Status =

        let ROUTE = (GET, "/health")

        [<Json.AutoCodec>]
        type Response = { Status: string }

        let get (callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE, callback)

module Auth =

    /// url parameters:
    ///  code - given by discord auth to identify you
    ///  state - state token previously provided by server to identify which Interlude client is logging in
    module Discord =

        let ROUTE = (GET, "/auth/discord")

module Charts =

    /// url parameters:
    ///  chart - hash of chart to get details for
    module Identify =

        let ROUTE = (GET, "/charts/identify")

        open Prelude.Backbeat.Archive

        [<Json.AutoCodec>]
        type Info = { Song: Song; Chart: Chart }

        [<Json.AutoCodec>]
        type Response = { Info: Info option }

        let get (chart: string, callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE + "?chart=" + chart, callback)

        let get_async (chart: string, callback: Response option -> unit) =
            Client.get_async<Response> (snd ROUTE + "?chart=" + chart, callback)

    /// requires login token as Authorization header
    module Add =

        let ROUTE = (POST, "/charts/add")

        open Prelude.Backbeat.Archive

        [<Json.AutoCodec>]
        type Request =
            {
                ChartId: string
                Chart: Chart
                Song: Song
            }

        let post (request: Request, callback: bool option -> unit) =
            Client.post<Request> (snd ROUTE, request, callback)

        let post_async (request: Request, callback: bool option -> unit) =
            Client.post_async<Request, bool> (snd ROUTE, request, callback)

    module Scores =

        /// requires login token as Authorization header
        module Save =

            let ROUTE = (POST, "/charts/scores")

            [<Json.AutoCodec>]
            type Request =
                {
                    ChartId: string
                    Replay: string
                    Rate: Rate
                    Mods: ModState
                    Timestamp: int64
                }

            [<Json.AutoCodec>]
            type ScoreResult =
                {
                    LeaderboardPosition: int option
                }

            type Response = ScoreResult option

            let post (request: Request, callback: Response option -> unit) =
                Client.post_return<Request, Response> (snd ROUTE, request, callback)

        /// requires login token as Authorization header
        /// url parameters:
        ///  chart - hash of chart to get details for
        module Leaderboard =

            let ROUTE = (GET, "/charts/scores")

            [<Json.AutoCodec>]
            type Score =
                {
                    Username: string
                    Rank: int
                    Replay: string
                    Rate: Rate
                    Mods: ModState
                    Timestamp: int64
                }

            [<Json.AutoCodec>]
            type Response =
                {
                    Scores: Score array
                }

            let get (chart: string, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + "?chart=" + chart, callback)

            let get_async (chart: string, callback: Response option -> unit) =
                Client.get_async<Response> (snd ROUTE + "?chart=" + chart, callback)

module Songs =

    open Prelude.Backbeat.Archive

    /// url parameters:
    ///  query - search query to find songs for
    module Search =

        let ROUTE = (GET, "/songs/search")

        [<Json.AutoCodec>]
        type Result =
            {
                SongId: int64
                Song: Song
                //Charts: Map<string, Chart>
            }

        [<Json.AutoCodec>]
        type Response = { Results: Result array }

        let get (query: string, callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE + "?query=" + query, callback)

    /// requires login token as Authorization header
    /// url parameters:
    ///  page - page number to fetch
    module Scan =

        let ROUTE = (GET, "/songs/scan")

        [<Json.AutoCodec>]
        type Result = { SongId: int64; Song: Song }

        [<Json.AutoCodec>]
        type Response = { Results: Result array; HasNextPage: bool }

        let get (page: int, callback: Response option -> unit) =
            Client.get (snd ROUTE + "?page=" + page.ToString(), callback)

    /// requires login token as Authorization header
    module Update =

        let ROUTE = (POST, "/songs/update")

        [<Json.AutoCodec>]
        type Request =
            {
                SongId: int64
                Song: Song
            }

        let post (request: Request, callback: bool option -> unit) =
            Client.post<Request> (snd ROUTE, request, callback)

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
                Hash: string
                Score: float
                Grade: int
            }

        [<Json.AutoCodec>]
        type Response = { Scores: Score array }

        // todo: query parameters lo and hi for just selecting levels within a range

        let get (user: string, table: string, callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE + "?user=" + escape user + "&table=" + escape table, callback)

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

        let get (table: string, callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE + "?table=" + escape table, callback)

    module List =

        let ROUTE = (GET, "/tables")

        open Prelude.Backbeat

        [<Json.AutoCodec>]
        type Table =
            {
                Id: string
                Info: TableInfo
                LastUpdated: int64
            }

        [<Json.AutoCodec>]
        type Response = { Tables: Table array }

        let get (callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE, callback)

    /// url parameters:
    ///  table - id of table to get charts for e.g 'crescent' or 'mizu'
    ///  section - OPTIONAL: name of section to get charts for
    module Charts =

        let ROUTE = (GET, "/tables/charts")

        open Prelude.Backbeat.Archive

        [<Json.AutoCodec>]
        type ChartInfo =
            {
                Hash: string
                Level: int
                Song: Song
                Chart: Chart
            }

        [<Json.AutoCodec>]
        type Response = { Charts: ChartInfo array }

        let get (table: string, callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE + "?table=" + table, callback)

        let get_section (table: string, section: string, callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE + "?table=" + table + "&section=" + section, callback)

    module Suggestions =

        /// requires login token as Authorization header
        module Vote =

            let ROUTE = (POST, "/tables/suggestions")

            [<Json.AutoCodec>]
            type Request =
                {
                    ChartId: string
                    TableId: string
                    Level: int
                }

            [<Json.AutoCodec>]
            [<RequireQualifiedAccess>]
            type Response =
                | Ok
                | OkDetailsRequired
                | Rejected

            let post (request: Request, callback: Response option -> unit) =
                Client.post_return<Request, Response> (snd ROUTE, request, callback)

        /// requires login token as Authorization header
        module ProvideDetails =

            let ROUTE = (POST, "/tables/suggestions/details")

            [<Json.AutoCodec>]
            type Request =
                {
                    ChartId: string
                    TableId: string
                    OsuBeatmapId: int
                    EtternaPackId: int
                    Artist: string
                    Title: string
                    Creator: string
                    Difficulty: string
                }

            let post (request: Request, callback: bool option -> unit) =
                Client.post<Request> (snd ROUTE, request, callback)

        /// requires login token as Authorization header
        /// url parameters:
        ///  table - id of table to get suggestions for e.g 'crescent' or 'mizu'
        module List =

            let ROUTE = (GET, "/tables/suggestions")

            open Prelude.Backbeat.Archive

            [<Json.AutoCodec>]
            type Suggestion =
                {
                    ChartId: string
                    Votes: Map<int, int>
                    BackbeatInfo: (Chart * Song) option
                }

            [<Json.AutoCodec>]
            type Response = { Suggestions: Suggestion array }

            let get (table: string, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + "?table=" + escape table, callback)

        /// url parameters:
        ///  table - id of table to get missing charts for for e.g 'crescent' or 'mizu'
        module Missing =

            let ROUTE = (GET, "/tables/suggestions/missing")

            [<Json.AutoCodec>]
            type MissingChart =
                {
                    ChartId: string
                    OsuBeatmapId: int
                    EtternaPackId: int
                    Artist: string
                    Title: string
                    Creator: string
                    Difficulty: string
                }

            [<Json.AutoCodec>]
            type Response = { Charts: MissingChart array }

            let get (table: string, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + "?table=" + escape table, callback)

        /// requires login token as Authorization header
        /// requires 'table-editor' badge for permission
        module Accept =

            let ROUTE = (POST, "/tables/suggestions/accept")

            [<Json.AutoCodec>]
            type Request =
                {
                    TableId: string
                    ChartId: string
                    Level: int
                }

            let post (request: Request, callback: bool option -> unit) =
                Client.post<Request> (snd ROUTE, request, callback)

        /// requires login token as Authorization header
        /// requires 'table-editor' badge for permission
        module Reject =

            let ROUTE = (POST, "/tables/suggestions/reject")

            [<Json.AutoCodec>]
            type Request =
                {
                    TableId: string
                    ChartId: string
                    Reason: string
                }

            let post (request: Request, callback: bool option -> unit) =
                Client.post<Request> (snd ROUTE, request, callback)

module Players =

    /// requires login token as Authorization header
    module Online =

        let ROUTE = (GET, "/players/online")

        [<Json.AutoCodec>]
        type Player = { Username: string; Color: int }

        [<Json.AutoCodec>]
        type Response = { Players: Player array }

        let get (callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE, callback)

    /// requires login token as Authorization header
    /// url parameters:
    ///  query - partial name of user to find matches for
    module Search =

        let ROUTE = (GET, "/players/search")

        [<Json.AutoCodec>]
        type Player = { Username: string; Color: int }

        [<Json.AutoCodec>]
        type Response = { Matches: Player array }

        let get (query: string, callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE + "?query=" + escape query, callback)

    module Profile =

        /// requires login token as Authorization header
        /// url parameters:
        ///  user - OPTIONAL: name of user to view profile of (not case sensitive)
        module View =
            let ROUTE = (GET, "/players/profile")

            [<Json.AutoCodec>]
            type Badge = { Name: string; Colors: int32 list }

            [<Json.AutoCodec>]
            type RecentScore =
                {
                    Artist: string
                    Title: string
                    Difficulty: string
                    Score: float
                    Lamp: int
                    Rate: float32<rate>
                    Mods: ModState
                    Timestamp: int64
                }

            [<Json.AutoCodec>]
            type ProfileStats =
                {
                    LastUpdated: int64
                    XP: int64
                    TotalPlaytime: float
                    Keymodes: Map<int, float * float32>
                }

            [<Json.AutoCodec>]
            type Response =
                {
                    Username: string
                    Color: int
                    Badges: Badge array
                    RecentScores: RecentScore array
                    DateSignedUp: int64
                    IsFriend: bool
                    IsMutualFriend: bool
                    Stats: ProfileStats
                }

            let get_me (callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE, callback)

            let get (user: string, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + "?user=" + escape user, callback)

        /// requires login token as Authorization header
        module Options =

            let ROUTE = (POST, "/players/profile/options")

            [<Json.AutoCodec>]
            type Request = { Color: int }

            let post (request: Request, callback: bool option -> unit) =
                Client.post<Request> (snd ROUTE, request, callback)

module Friends =

    /// requires login token as Authorization header
    module List =

        let ROUTE = (GET, "/friends")

        [<Json.AutoCodec>]
        type Friend =
            {
                Username: string
                Color: int
                Online: bool
            }

        [<Json.AutoCodec>]
        type Response = { Friends: Friend array }

        let get (callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE, callback)

    /// requires login token as Authorization header
    module Add =

        let ROUTE = (POST, "/friends")

        [<Json.AutoCodec>]
        type Request = { User: string }

        let post (request: Request, callback: bool option -> unit) =
            Client.post<Request> (snd ROUTE, request, callback)

    /// requires login token as Authorization header
    /// url parameters:
    ///  user - name of user to add as friend (not case sensitive)
    module Remove =

        let ROUTE = (DELETE, "/friends")

        let delete (user: string, callback: bool option -> unit) =
            Client.delete (snd ROUTE + "?user=" + escape user, callback)

module Stats =

    open Prelude.Data.User.Stats

    /// requires login token as Authorization header
    module Fetch =

        let ROUTE = (GET, "/stats")

        type Response = StatsSyncDownstream

        let get (callback: Response option -> unit) =
            Client.get<Response> (snd ROUTE, callback)

    /// requires login token as Authorization header
    module Sync =

        let ROUTE = (POST, "/stats")

        type Request = StatsSyncUpstream

        let post (request: Request, callback: bool option -> unit) =
            Client.post<Request> (snd ROUTE, request, callback)

    module Leaderboard =

        [<Json.AutoCodec>]
        type XPLeaderboardEntry =
            {
                Username: string
                Color: int
                XP: int64
                Playtime: float
            }

        [<Json.AutoCodec>]
        type XPResponse =
            {
                Leaderboard: XPLeaderboardEntry array
                You: (int64 * XPLeaderboardEntry) option
            }

        /// requires login token as Authorization header
        module XP =

            let ROUTE = (GET, "/leaderboard/xp")

            type Response = XPResponse

            let get (sort_by_playtime: bool, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + (if sort_by_playtime then "?sort=playtime" else ""), callback)

        /// requires login token as Authorization header
        module MonthlyXP =

            let ROUTE = (GET, "/leaderboard/monthly/xp")

            type Response = XPResponse

            let get (sort_by_playtime: bool, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + (if sort_by_playtime then "?sort=playtime" else ""), callback)

        [<Json.AutoCodec>]
        type KeymodeLeaderboardEntry =
            {
                Username: string
                Color: int
                Playtime: float
                Combined: float32
                Jacks: float32
                Chordstream: float32
                Stream: float32
            }

        type Sort =
            | Playtime
            | Combined
            | Jacks
            | Chordstream
            | Stream
            override this.ToString() =
                match this with
                | Playtime -> "playtime"
                | Combined -> "combined"
                | Jacks -> "jacks"
                | Chordstream -> "chordstream"
                | Stream -> "stream"

        [<Json.AutoCodec>]
        type KeymodeResponse =
            {
                Leaderboard: KeymodeLeaderboardEntry array
                You: (int64 * KeymodeLeaderboardEntry) option
            }

        /// requires login token as Authorization header
        module Keymode =

            let ROUTE = (GET, "/leaderboard/keymode")

            type Response = KeymodeResponse

            let get (keys: int, sort: Sort, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + "?sort=" + sort.ToString() + "&keys=" + keys.ToString(), callback)

        /// requires login token as Authorization header
        module MonthlyKeymode =

            let ROUTE = (GET, "/leaderboard/monthly/keymode")

            type Response = KeymodeResponse

            let get (keys: int, sort: Sort, callback: Response option -> unit) =
                Client.get<Response> (snd ROUTE + "?sort=" + sort.ToString() + "&keys=" + keys.ToString(), callback)