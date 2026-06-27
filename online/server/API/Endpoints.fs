namespace Interlude.Web.Server.API

open Interlude.Web.Shared
open Interlude.Web.Shared.Requests
open Interlude.Web.Server
open Interlude.Web.Server.API

type Endpoints =

    static member CreateApiRoot =
        API()
            .Register(Health.Status.ROUTE, Health.Status.handle)

            .RegisterIf(not SECRETS.IsProduction, (API.HttpMethod.GET, "/auth/dummy"), Auth.Dummy.handle)

            .Register(Auth.Discord.ROUTE, Auth.Discord.handle)

            .Register(Charts.Identify.ROUTE, Charts.Identify.handle)
            .Register(Charts.Add.ROUTE, Charts.Add.handle)
            .Register(Charts.Scores.Save.ROUTE, Charts.Scores.Save.handle)
            .Register(Charts.Scores.Leaderboard.ROUTE, Charts.Scores.Leaderboard.handle)

            .Register(Songs.Search.ROUTE, Songs.Search.handle)
            .Register(Songs.Scan.ROUTE, Songs.Scan.handle)
            .Register(Songs.Update.ROUTE, Songs.Update.handle)

            .Register(Tables.Records.ROUTE, Tables.Records.handle)
            .Register(Tables.Leaderboard.ROUTE, Tables.Leaderboard.handle)
            .Register(Tables.List.ROUTE, Tables.List.handle)
            .Register(Tables.Charts.ROUTE, Tables.Charts.handle)

            .Register(Tables.Suggestions.Vote.ROUTE, Tables.Suggestions.Vote.handle)
            .Register(Tables.Suggestions.List.ROUTE, Tables.Suggestions.List.handle)
            .Register(Tables.Suggestions.Missing.ROUTE, Tables.Suggestions.Missing.handle)
            .Register(Tables.Suggestions.Accept.ROUTE, Tables.Suggestions.Accept.handle)
            .Register(Tables.Suggestions.Reject.ROUTE, Tables.Suggestions.Reject.handle)

            .Register(Players.Online.ROUTE, Players.Online.handle)
            .Register(Players.Search.ROUTE, Players.Search.handle)

            .Register(Players.Profile.View.ROUTE, Players.Profile.View.handle)
            .Register(Players.Profile.Options.ROUTE, Players.Profile.Options.handle)

            .Register(Friends.List.ROUTE, Friends.List.handle)
            .Register(Friends.Add.ROUTE, Friends.Add.handle)
            .Register(Friends.Remove.ROUTE, Friends.Remove.handle)

            .Register(Stats.Sync.ROUTE, Stats.Sync.handle)
            .Register(Stats.Fetch.ROUTE, Stats.Fetch.handle)
            .Register(Stats.Leaderboard.XP.ROUTE, Stats.Leaderboard.XP.handle)
            .Register(Stats.Leaderboard.MonthlyXP.ROUTE, Stats.Leaderboard.MonthlyXP.handle)
            .Register(Stats.Leaderboard.Keymode.ROUTE, Stats.Leaderboard.Keymode.handle)
            .Register(Stats.Leaderboard.MonthlyKeymode.ROUTE, Stats.Leaderboard.MonthlyKeymode.handle)