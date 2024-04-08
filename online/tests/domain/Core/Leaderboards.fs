namespace Interlude.Web.Tests.Domain.Core

open NUnit.Framework

open Interlude.Web.Server.Domain.Core

module Leaderboards =

    [<Test>]
    let Create_Exists () =
        Assert.True(Leaderboard.create "CreateExistsChartId" "CreateExistsRulesetId")
        Assert.True(Leaderboard.exists "CreateExistsChartId" "CreateExistsRulesetId")

    [<Test>]
    let Create_Idempotent () =
        Assert.False(Leaderboard.exists "CreateIdempotent" "CreateIdempotent")
        Assert.True(Leaderboard.create "CreateIdempotent" "CreateIdempotent")
        Assert.False(Leaderboard.create "CreateIdempotent" "CreateIdempotent")
        Assert.False(Leaderboard.create "CreateIdempotent" "CreateIdempotent")
        Assert.True(Leaderboard.exists "CreateIdempotent" "CreateIdempotent")

    [<Test>]
    let DoesntExist () =
        Assert.False(Leaderboard.exists "DoesntExistChartId" "DoesntExistRulesetId")

    [<Test>]
    let DoesntExist2 () =
        Assert.True(Leaderboard.create "DoesntExist2ChartId" "DoesntExist2ChartId")
        Assert.False(Leaderboard.exists "WRONGCHARTID" "DoesntExist2RulesetId")
        Assert.False(Leaderboard.exists "WRONGCHARTID" "WRONGRULESETID")
        Assert.False(Leaderboard.exists "DoesntExist2ChartId" "WRONGRULESETID")
