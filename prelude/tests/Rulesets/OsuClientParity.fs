namespace Prelude.Tests.Rulesets

open NUnit.Framework
open Prelude
open Prelude.Gameplay.RulesetsV2
open Prelude.Gameplay.ScoringV2

module OsuClientParity =

    let RULESET = OsuMania.create 8.0f OsuMania.NoMod

    [<Test>]
    let RecreateLylcaruisBugReport () =

        let notes = 
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Note(108.0f<ms>)
                .Note(222.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-71.0f<ms>, 30.0f<ms>)
                .KeyDownFor(108.0f<ms> - 121.0f<ms>, 30.0f<ms>) // this input is getting eaten because it is before note 0 which has already been hit
                .KeyDownFor(222.0f<ms> - 125.0f<ms>, 30.0f<ms>)
                .KeyDownFor(222.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = GameplayEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                HIT(-71.0f<ms / rate>, false)
                HIT(-11.0f<ms / rate>, false)
                HIT(0.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Action
        )