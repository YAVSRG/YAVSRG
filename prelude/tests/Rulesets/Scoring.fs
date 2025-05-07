namespace Prelude.Tests.Rulesets

open System
open NUnit.Framework
open Percyqaz.Common
open Prelude
open Prelude.Formats
open Prelude.Formats.Osu
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring

module Scoring =

    let RULESET = SC.create 4

    [<Test>]
    let BasicEndToEnd () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Note(1000.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)
                .KeyDownFor(500.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1045.0f<ms>, 30.0f<ms>)
                .Build()

        let result = ScoreProcessor.run RULESET 4 replay notes 1.0f<rate>

        printfn "Accuracy: %.2f%%" (result.Accuracy * 100.0)
        printfn "Judgements: %A" result.JudgementCounts

        Assert.AreEqual(1, result.JudgementCounts.[0])
        Assert.AreEqual(1, result.JudgementCounts.[1])
        Assert.AreEqual(0.95, result.Accuracy)
        Assert.AreEqual(2, result.CurrentCombo)
        Assert.AreEqual(result.CurrentCombo, result.MaxPossibleCombo)
        Assert.AreEqual(result.BestCombo, result.MaxPossibleCombo)
        Assert.AreEqual(0, result.ComboBreaks)

    [<Test>]
    let TimeStepped_Example () =
        let notes =
            ChartBuilder(4)
                .Note(0.0f<ms>)
                .Hold(1000.0f<ms>, 2000.0f<ms>)
                .Hold(3000.0f<ms>, 4000.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-45.0f<ms>, 30.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 1000.0f<ms>)
                .KeyDownFor(3000.0f<ms>, 30.0f<ms>)
                .KeyDownFor(3100.0f<ms>, 900.0f<ms>)
                .Build()

        let stepper = ScoreProcessor.create RULESET 4 replay notes 1.0f<rate>

        stepper.OnEvent.Add(printfn "%A")

        let step time =
            printfn "STEPPING TO %.1fms:" time

            stepper.Update time

            printfn "Accuracy: %.2f%%" (stepper.Accuracy * 100.0)
            printfn "Judgements: %A" stepper.JudgementCounts

        Assert.True(stepper.JudgementCounts |> Seq.forall ((=) 0))
        Assert.AreEqual(1.0, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)

        step -100.0f<ms>

        Assert.True(stepper.JudgementCounts |> Seq.forall ((=) 0))
        Assert.AreEqual(1.0, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)

        step -45.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.9, stepper.Accuracy)
        Assert.AreEqual(1, stepper.CurrentCombo)

        step 1000.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.9, stepper.Accuracy)
        Assert.AreEqual(2, stepper.CurrentCombo)

        step 2000.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(3, stepper.CurrentCombo)

        step 3000.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(4, stepper.CurrentCombo)

        step 3050.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)
        Assert.AreEqual(4, stepper.BestCombo)
        Assert.AreEqual(4, stepper.MaxPossibleCombo)

        step 3100.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(0.95, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)
        Assert.AreEqual(4, stepper.BestCombo)
        Assert.AreEqual(4, stepper.MaxPossibleCombo)

        step 4000.0f<ms>

        Assert.AreEqual(1, stepper.JudgementCounts.[0])
        Assert.AreEqual(1, stepper.JudgementCounts.[1])
        Assert.AreEqual(1, stepper.JudgementCounts.[3])
        Assert.AreEqual(1.4 / 3.0, stepper.Accuracy)
        Assert.AreEqual(0, stepper.CurrentCombo)
        Assert.AreEqual(4, stepper.BestCombo)
        Assert.AreEqual(5, stepper.MaxPossibleCombo)

    [<Test>]
    let SCJ4_Note_ExpectedJudgements () =
        let TEST_CASES =
            [
                0.0f<ms>, 0
                22.5f<ms>, 0
                -22.5f<ms>, 0
                22.6f<ms>, 1
                -22.6f<ms>, 1
                45.0f<ms>, 1
                -45.0f<ms>, 1
                45.1f<ms>, 2
                -45.1f<ms>, 2
                90.0f<ms>, 2
                -90.0f<ms>, 2
                90.1f<ms>, 3
                -90.1f<ms>, 3
                135.0f<ms>, 3
                -135.0f<ms>, 3
                135.1f<ms>, 4
                -135.1f<ms>, 4
                180.0f<ms>, 4
                -180.0f<ms>, 4
                180.1f<ms>, 5
                -180.1f<ms>, 5
            ]

        for offset, expected_judgement in TEST_CASES do

            let notes = ChartBuilder(4).Note(0.0f<ms>).Build()

            let replay = ReplayBuilder().KeyDownFor(offset, 1.0f<ms>).Build()

            let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
            event_processing.Update Time.infinity

            Assert.AreEqual(1, event_processing.JudgementCounts.[expected_judgement])

    [<Test>]
    let SCJ4_HoldNote_ExpectedJudgements_ExactRelease () =
        let TEST_CASES =
            [
                0.0f<ms>, 0
                22.5f<ms>, 0
                -22.5f<ms>, 0
                22.6f<ms>, 1
                -22.6f<ms>, 1
                45.0f<ms>, 1
                -45.0f<ms>, 1
                45.1f<ms>, 2
                -45.1f<ms>, 2
                90.0f<ms>, 2
                -90.0f<ms>, 2
                90.1f<ms>, 3
                -90.1f<ms>, 3
                135.0f<ms>, 3
                -135.0f<ms>, 3
                135.1f<ms>, 4
                -135.1f<ms>, 4
                180.0f<ms>, 4
                -180.0f<ms>, 4
                180.1f<ms>, 4
                -180.1f<ms>, 5
            ]

        for offset, expected_judgement in TEST_CASES do

            let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

            let replay = ReplayBuilder().KeyDownUntil(offset, 1000.0f<ms>).Build()

            let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
            event_processing.Update Time.infinity

            Assert.AreEqual(expected_judgement, Array.IndexOf(event_processing.JudgementCounts, 1))

    [<Test>]
    let SCJ4_HoldNote_ExpectedJudgements_LatestPossibleRelease () =
        let TEST_CASES =
            [
                0.0f<ms>, 0
                22.5f<ms>, 0
                -22.5f<ms>, 0
                22.6f<ms>, 1
                -22.6f<ms>, 1
                45.0f<ms>, 1
                -45.0f<ms>, 1
                45.1f<ms>, 2
                -45.1f<ms>, 2
                90.0f<ms>, 2
                -90.0f<ms>, 2
                90.1f<ms>, 3
                -90.1f<ms>, 3
                135.0f<ms>, 3
                -135.0f<ms>, 3
                135.1f<ms>, 4
                -135.1f<ms>, 4
                180.0f<ms>, 4
                -180.0f<ms>, 4
                180.1f<ms>, 4
                -180.1f<ms>, 5
            ]

        for offset, expected_judgement in TEST_CASES do

            let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

            let replay = ReplayBuilder().KeyDownUntil(offset, 1180.0f<ms>).Build()

            let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
            event_processing.Update Time.infinity

            Assert.AreEqual(expected_judgement, Array.IndexOf(event_processing.JudgementCounts, 1))

    [<Test>]
    let SCJ4_HoldNote_ExpectedJudgements_EarliestPossibleRelease () =
        let TEST_CASES =
            [
                0.0f<ms>, 0
                22.5f<ms>, 0
                -22.5f<ms>, 0
                22.6f<ms>, 1
                -22.6f<ms>, 1
                45.0f<ms>, 1
                -45.0f<ms>, 1
                45.1f<ms>, 2
                -45.1f<ms>, 2
                90.0f<ms>, 2
                -90.0f<ms>, 2
                90.1f<ms>, 3
                -90.1f<ms>, 3
                135.0f<ms>, 3
                -135.0f<ms>, 3
                135.1f<ms>, 4
                -135.1f<ms>, 4
                180.0f<ms>, 4
                -180.0f<ms>, 4
                180.1f<ms>, 4
                -180.1f<ms>, 5
            ]

        for offset, expected_judgement in TEST_CASES do

            let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

            let replay = ReplayBuilder().KeyDownUntil(offset, 820.0f<ms>).Build()

            let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
            event_processing.Update Time.infinity

            Assert.AreEqual(expected_judgement, Array.IndexOf(event_processing.JudgementCounts, 1))

    [<Test>]
    let SCJ4_HoldNote_ExpectedJudgements_Dropped () =
        let TEST_CASES =
            [
                0.0f<ms>, 3
                22.5f<ms>, 3
                -22.5f<ms>, 3
                22.6f<ms>, 3
                -22.6f<ms>, 3
                45.0f<ms>, 3
                -45.0f<ms>, 3
                45.1f<ms>, 3
                -45.1f<ms>, 3
                90.0f<ms>, 3
                -90.0f<ms>, 3
                90.1f<ms>, 3
                -90.1f<ms>, 3
                135.0f<ms>, 3
                -135.0f<ms>, 3
                135.1f<ms>, 4
                -135.1f<ms>, 4
                180.0f<ms>, 4
                -180.0f<ms>, 4
                180.1f<ms>, 5
                -180.1f<ms>, 5
            ]

        for offset, expected_judgement in TEST_CASES do

            let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

            let replay = ReplayBuilder().KeyDownUntil(offset, 500.0f<ms>).Build()

            let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
            event_processing.Update Time.infinity

            Assert.AreEqual(expected_judgement, Array.IndexOf(event_processing.JudgementCounts, 1))

    [<Test>]
    let SCJ4_HoldNote_ExpectedJudgements_Regrabbed () =
        let TEST_CASES =
            [
                0.0f<ms>, 3
                22.5f<ms>, 3
                -22.5f<ms>, 3
                22.6f<ms>, 3
                -22.6f<ms>, 3
                45.0f<ms>, 3
                -45.0f<ms>, 3
                45.1f<ms>, 3
                -45.1f<ms>, 3
                90.0f<ms>, 3
                -90.0f<ms>, 3
                90.1f<ms>, 3
                -90.1f<ms>, 3
                135.0f<ms>, 3
                -135.0f<ms>, 3
                135.1f<ms>, 4
                -135.1f<ms>, 4
                180.0f<ms>, 4
                -180.0f<ms>, 4
                180.1f<ms>, 4
                -180.1f<ms>, 4
            ]

        for offset, expected_judgement in TEST_CASES do

            let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

            let replay =
                ReplayBuilder()
                    .KeyDownUntil(offset, 500.0f<ms>)
                    .KeyDownUntil(501.0f<ms>, 1000.0f<ms>)
                    .Build()

            let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
            event_processing.Update Time.infinity

            Assert.AreEqual(expected_judgement, Array.IndexOf(event_processing.JudgementCounts, 1))

    [<Test>]
    let SCJ4_HoldNote_ExpectedJudgements_Overheld () =
        let TEST_CASES =
            [
                0.0f<ms>, 3
                22.5f<ms>, 3
                -22.5f<ms>, 3
                22.6f<ms>, 3
                -22.6f<ms>, 3
                45.0f<ms>, 3
                -45.0f<ms>, 3
                45.1f<ms>, 3
                -45.1f<ms>, 3
                90.0f<ms>, 3
                -90.0f<ms>, 3
                90.1f<ms>, 3
                -90.1f<ms>, 3
                135.0f<ms>, 3
                -135.0f<ms>, 3
                135.1f<ms>, 4
                -135.1f<ms>, 4
                180.0f<ms>, 4
                -180.0f<ms>, 4
                180.1f<ms>, 5
                -180.1f<ms>, 5
            ]

        for offset, expected_judgement in TEST_CASES do

            let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

            let replay = ReplayBuilder().KeyDownUntil(offset, 1500.0f<ms>).Build()

            let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
            event_processing.Update Time.infinity

            Assert.AreEqual(expected_judgement, Array.IndexOf(event_processing.JudgementCounts, 1))

    [<Test>]
    let SCJ4_HoldNote_CompletelyMissed () =
        let notes = ChartBuilder(4).Hold(0.0f<ms>, 1000.0f<ms>).Build()

        let replay = ReplayBuilder().Build()

        let event_processing = ScoringEventCollector(RULESET, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.AreEqual([| 0; 0; 0; 0; 0; 1 |], event_processing.JudgementCounts)

    [<Test>]
    let WifeCurve_J4_Monotonic () =
        let wife_points =
            seq {
                for i = 0 to 200 do
                    let offset = float32 i * 1.0f<ms / rate>
                    let result = Wife3Curve.calculate 4 offset

                    printfn "%.0fms -> %.5f" offset result

                    yield result
            }
            |> Array.ofSeq

        Assert.AreEqual(wife_points, Array.sortDescending wife_points)

    [<Test>]
    let WifeCurve_AllJudges_Monotonic () =
        for judge = 4 to 9 do

            let wife_points =
                seq {
                    for i = 0 to 200 do
                        let offset = float32 i * 1.0f<ms / rate>
                        let result = Wife3Curve.calculate judge offset

                        yield result
                }
                |> Array.ofSeq

            Assert.AreEqual(wife_points, Array.sortDescending wife_points)

    [<Test>]
    let WifeCurve_Symmetrical () =
        for judge = 4 to 9 do
            let offset = 31.415f<ms / rate>
            Assert.AreEqual(Wife3Curve.calculate judge offset, Wife3Curve.calculate judge -offset)

    let DONUT_HOLE_CHART =
        let beatmap =
            Beatmap.FromFile "./Data/Hachi - DONUT HOLE (Raveille) [Filling].osu" |> expect

        (Osu_To_Interlude.convert
            beatmap
            {
                Config = ConversionOptions.Pack("osu!", None, LinkAssetFiles)
                Source = "./Data/Hachi - DONUT HOLE (Raveille) [Filling].osu"
            }
         |> expect)
            .Chart

    [<Test>]
    let Autoplay_PerfectScores () =

        let perfect_replay =
            Replay.perfect_replay DONUT_HOLE_CHART.Keys DONUT_HOLE_CHART.Notes

        let result_scj4 =
            ScoreProcessor.run
                (SC.create 4)
                DONUT_HOLE_CHART.Keys
                (StoredReplay(perfect_replay))
                DONUT_HOLE_CHART.Notes
                1.0f<rate>

        let result_osumania =
            ScoreProcessor.run
                (OsuMania.create 10.0f OsuMania.NoMod)
                DONUT_HOLE_CHART.Keys
                (StoredReplay(perfect_replay))
                DONUT_HOLE_CHART.Notes
                1.0f<rate>

        let result_etterna =
            ScoreProcessor.run
                (Wife3.create 4)
                DONUT_HOLE_CHART.Keys
                (StoredReplay(perfect_replay))
                DONUT_HOLE_CHART.Notes
                1.0f<rate>

        Assert.AreEqual(1.0, result_scj4.Accuracy)
        Assert.AreEqual(0, result_scj4.ComboBreaks)

        Assert.AreEqual(1.0, result_osumania.Accuracy)
        Assert.AreEqual(0, result_osumania.ComboBreaks)

        Assert.AreEqual(1.0, result_etterna.Accuracy)
        Assert.AreEqual(0, result_etterna.ComboBreaks)

    [<Test>]
    let GhostTapJudgement_BreaksCombo_When_JudgementBreaksCombo () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Note(1000.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 10.0f<ms>)
                .KeyDownFor(500.0f<ms>, 10.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 10.0f<ms>)
                .Build()

        let ruleset =
            { RULESET with
                HitMechanics =
                    { RULESET.HitMechanics with
                        GhostTapJudgement = Some 5
                    }
            }

        let event_processing = ScoringEventCollector(ruleset, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.True(ruleset.Judgements.[5].BreaksCombo)
        Assert.AreEqual([| 2; 0; 0; 0; 0; 1 |], event_processing.JudgementCounts)
        Assert.AreEqual(1, event_processing.ComboBreaks)
        Assert.AreEqual(1, event_processing.CurrentCombo)

    [<Test>]
    let GhostTapJudgement_DoesNotIncreaseCombo_When_JudgementNormallyIncreasesCombo () =
        let notes = ChartBuilder(4).Note(0.0f<ms>).Note(1000.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms>, 10.0f<ms>)
                .KeyDownFor(500.0f<ms>, 10.0f<ms>)
                .KeyDownFor(1000.0f<ms>, 10.0f<ms>)
                .Build()

        let ruleset =
            { RULESET with
                HitMechanics =
                    { RULESET.HitMechanics with
                        GhostTapJudgement = Some 2
                    }
            }

        let event_processing = ScoringEventCollector(ruleset, 4, replay, notes, 1.0f<rate>)
        event_processing.Update Time.infinity

        Assert.False(ruleset.Judgements.[2].BreaksCombo)
        Assert.AreEqual([| 2; 0; 1; 0; 0; 0 |], event_processing.JudgementCounts)
        Assert.AreEqual(0, event_processing.ComboBreaks)
        Assert.AreNotEqual(3, event_processing.CurrentCombo)
        Assert.AreEqual(2, event_processing.CurrentCombo)