namespace Prelude.Tests.Rulesets

open NUnit.Framework
open Prelude
open Prelude.Gameplay.Rulesets

module Editing =

    [<Test>]
    let SCJ4_DeletingJudgements () =
        let ruleset = SC.create 4

        let ruleset_without_bad = Ruleset.remove_judgement 4 ruleset

        printfn "%A" ruleset_without_bad

        match Ruleset.check ruleset_without_bad with
        | Ok _ -> Assert.Pass()
        | Error reason -> Assert.Fail(reason)
        
    [<Test>]
    let SCJ4_DeletingAllJudgementsButMiss () =
        let mutable ruleset = SC.create 4

        for _ = 1 to 5 do

            ruleset <- Ruleset.remove_judgement 0 ruleset
            printfn "%A" ruleset

            match Ruleset.check ruleset with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()

    [<Test>]
    let SCJ4_DuplicatingPerfectTwice () =
        let mutable ruleset = SC.create 4

        for i = 0 to 1 do

            ruleset <- Ruleset.duplicate_judgement 1 ruleset
            printfn "%A" ruleset

            match Ruleset.check ruleset with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()
        
    [<Test>]
    let SCJ4_DuplicatingAllJudgements () =
        let mutable ruleset = SC.create 4

        for i = 5 downto 0 do

            ruleset <- Ruleset.duplicate_judgement i ruleset
            printfn "%A" ruleset

            match Ruleset.check ruleset with
            | Ok _ -> ()
            | Error reason -> Assert.Fail(reason)

        Assert.Pass()