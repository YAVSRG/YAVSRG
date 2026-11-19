namespace Prelude.Tests.Gameplay.Rulesets

open NUnit.Framework
open Prelude.Gameplay.Rulesets

module RulesetEditingTests =

    [<Test>]
    let SCJ4_DeletingJudgements () =
        let ruleset = SC.create(4)

        let ruleset_without_bad = ruleset.WithoutJudgement(4)

        printfn "%A" ruleset_without_bad

        match ruleset_without_bad.Validate() with
        | Ok _ -> Assert.Pass()
        | Error reason -> Assert.Fail(reason)

    [<Test>]
    let SCJ4_DeletingAllJudgementsButMiss () =
        let mutable ruleset = SC.create(4)

        for _ = 1 to 5 do

            ruleset <- ruleset.WithoutJudgement(0)
            printfn "%A" ruleset

            match ruleset.Validate() with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()

    [<Test>]
    let SCJ4_DuplicatingPerfectTwice () =
        let mutable ruleset = SC.create(4)

        for i = 0 to 1 do

            ruleset <- ruleset.WithDuplicatedJudgement(1)
            printfn "%A" ruleset

            match ruleset.Validate() with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()

    [<Test>]
    let SCJ4_DuplicatingAllJudgements () =
        let mutable ruleset = SC.create(4)

        for i = 5 downto 0 do

            ruleset <- ruleset.WithDuplicatedJudgement(i)
            printfn "%A" ruleset

            match ruleset.Validate() with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()
