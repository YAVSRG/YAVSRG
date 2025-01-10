namespace Prelude.Tests.Rulesets

open NUnit.Framework
open Prelude
open Percyqaz.Common
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring
open Prelude.Tests.Rulesets

module QuaverParity =

    let RULESET = Quaver.create Quaver.Standard

    [<Test>]
    let Quaver_ExpectedBehaviour_DropHold () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(0.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        let CONVENTIONAL_LATE_WINDOW = snd RULESET.ReleaseWindows

        Assert.AreEqual(
            [
                Hold{| Delta = 0.0f<ms / rate>; Judgement = Some (0, 1.0); Missed = false |}
                DropHold
                Release{| Delta = CONVENTIONAL_LATE_WINDOW; Judgement = Some (5, -0.5); Missed = true; Overhold = false; Dropped = true |}
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let Quaver_ExpectedBehaviour_Overhold () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(0.0f<ms>, 2000.0f<ms>)
                .Build()

        let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        let CONVENTIONAL_LATE_WINDOW = snd RULESET.ReleaseWindows

        Assert.AreEqual(
            [
                Hold{| Delta = 0.0f<ms / rate>; Judgement = Some (0, 1.0); Missed = false |}
                Release{| Delta = CONVENTIONAL_LATE_WINDOW; Judgement = Some (4, -1.0); Missed = true; Overhold = true; Dropped = false |}
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let Quaver_ExpectedBehaviour_OkayWindowGivesGood () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(0.0f<ms>, 1190.0f<ms>)
                .Build()

        let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                Hold{| Delta = 0.0f<ms / rate>; Judgement = Some (0, 1.0); Missed = false |}
                Release{| Delta = 190.0f<ms / rate>; Judgement = Some (3, 0.25); Missed = false; Overhold = false; Dropped = false |}
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let Quaver_ExpectedBehaviour_GoodWindowGivesGood () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(0.0f<ms>, 1158.0f<ms>)
                .Build()

        let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                Hold{| Delta = 0.0f<ms / rate>; Judgement = Some (0, 1.0); Missed = false |}
                Release{| Delta = 158.0f<ms / rate>; Judgement = Some (3, 0.25); Missed = false; Overhold = false; Dropped = false |}
            ],
            event_processing.Events |> Seq.map _.Action
        )

    [<Test>]
    let Quaver_ExpectedBehaviour_GreatWindowGivesGreat () =
        let notes =
            ChartBuilder(4)
                .Hold(0.0f<ms>, 1000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownUntil(0.0f<ms>, 1113.0f<ms>)
                .Build()

        let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual(
            [
                Hold{| Delta = 0.0f<ms / rate>; Judgement = Some (0, 1.0); Missed = false |}
                Release{| Delta = 113.0f<ms / rate>; Judgement = Some (2, 0.65); Missed = false; Overhold = false; Dropped = false |}
            ],
            event_processing.Events |> Seq.map _.Action
        )