namespace Interlude.Web.Tests.Domain

open NUnit.Framework

open Interlude.Web.Server.Domain.Objects

module Leaderboards =

    [<Test>]
    let Create_Exists () =
        Leaderboard.create "CreateExistsChartId" "CreateExistsRulesetId"
        Assert.True(Leaderboard.exists "CreateExistsChartId" "CreateExistsRulesetId")
    
    [<Test>]
    let Create_Idempotent () =
        Assert.False(Leaderboard.exists "CreateIdempotent" "CreateIdempotent")
        Leaderboard.create "CreateIdempotent" "CreateIdempotent"
        Leaderboard.create "CreateIdempotent" "CreateIdempotent"
        Leaderboard.create "CreateIdempotent" "CreateIdempotent"
        Assert.True(Leaderboard.exists "CreateIdempotent" "CreateIdempotent")

    [<Test>]
    let DoesntExist () =
        Assert.False(Leaderboard.exists "DoesntExistChartId" "DoesntExistRulesetId")
        
    [<Test>]
    let DoesntExist2 () =
        Leaderboard.create "DoesntExist2ChartId" "DoesntExist2ChartId"
        Assert.False(Leaderboard.exists "WRONGCHARTID" "DoesntExist2RulesetId")
        Assert.False(Leaderboard.exists "WRONGCHARTID" "WRONGRULESETID")
        Assert.False(Leaderboard.exists "DoesntExist2ChartId" "WRONGRULESETID")