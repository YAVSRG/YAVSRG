namespace Prelude.Tests.Scoring // todo: rename to Rulesets

open NUnit.Framework
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.RulesetsV2
open Helpers

module Events =

    let RULESET = SC.create 4

    [<Test>]
    let HitOneNote () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Build()
        let replay = ReplayBuilder().KeyDown(0.0f<ms>, 0).Build()
        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity
        Assert.Pass()