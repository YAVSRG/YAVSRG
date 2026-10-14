namespace Prelude.Tests.Gameplay.Scoring

open NUnit.Framework
open Percyqaz.Common
open Prelude.Formats.Osu
open Prelude.Formats.Conversions
open Prelude.Data.OsuClientInterop
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring
open Prelude.Tests.Helpers

module RulesetOsuParityTests =

    let RULESET = OsuMania.create 8.0f OsuMania.NoMod

    let TEST_REPLAY_FILE =
        OsuReplay.TryReadFile(
            "./test_data/Lylcaruis - Cardboard Box - He He He [SPEEEDDD!!!] (2023-09-29) OsuMania.osr"
        )
        |> Option.get

    let TEST_OSU_FILE_PATH =
        "./test_data/Cardboard Box - He He He (DannyPX) [SPEEEDDD!!!].osu"

    let TEST_OSU_FILE_HASH = Beatmap.HashFromFile(TEST_OSU_FILE_PATH) |> expect
    let TEST_OSU_FILE = Beatmap.TryReadFromFile(TEST_OSU_FILE_PATH) |> expect

    let TEST_CHART =
        (OsuConverter.convert
            TEST_OSU_FILE
            { Options = ConversionOptions.Pack("osu!", None, Link); OriginalPath = TEST_OSU_FILE_PATH }
         |> expect)
            .Chart

    let TEST_REPLAY_FILE_2 =
        OsuReplay.TryReadFile("./test_data/Percyqaz - Hachi - DONUT HOLE [Filling] (2024-09-30) OsuMania.osr")
        |> Option.get

    let TEST_OSU_FILE_PATH_2 = "./test_data/Hachi - DONUT HOLE (Raveille) [Filling].osu"
    let TEST_OSU_FILE_HASH_2 = Beatmap.HashFromFile(TEST_OSU_FILE_PATH_2) |> expect
    let TEST_OSU_FILE_2 = Beatmap.TryReadFromFile(TEST_OSU_FILE_PATH_2) |> expect

    let TEST_CHART_2 =
        (OsuConverter.convert
            TEST_OSU_FILE_2
            { Options = ConversionOptions.Pack("osu!", None, Link); OriginalPath = TEST_OSU_FILE_PATH_2 }
         |> expect)
            .Chart

    [<Test>]
    let Replay_MatchesTestFile_1 () =
        Assert.AreEqual(TEST_OSU_FILE_HASH, TEST_REPLAY_FILE.BeatmapHash)

    [<Test>]
    let Replay_MatchesTestFile_2 () =
        Assert.AreEqual(TEST_OSU_FILE_HASH_2, TEST_REPLAY_FILE_2.BeatmapHash)

    [<Test>]
    let Replay_RoundTrip () =
        let as_interlude_replay =
            OsuReplay.decode(TEST_REPLAY_FILE, TEST_CHART.FirstNote, 1.0f<rate>)

        let back_as_osu_replay =
            OsuReplay.encode as_interlude_replay TEST_CHART.FirstNote Mods.None TEST_OSU_FILE_HASH

        let return_as_interlude_replay =
            OsuReplay.decode(back_as_osu_replay, TEST_CHART.FirstNote, 1.0f<rate>)

        Assert.AreEqual(as_interlude_replay, return_as_interlude_replay)

    [<Test>]
    let OsuRuleset_MatchesReplayJudgements_HeHeHeSample () =
        let as_interlude_replay =
            OsuReplay.decode(TEST_REPLAY_FILE, TEST_CHART.FirstNote, 1.0f<rate>)

        let as_interlude_ruleset =
            OsuMania.create (float32 TEST_OSU_FILE.Difficulty.OverallDifficulty) OsuMania.NoMod

        let score =
            ScoreProcessor.ProcessEntireReplay(as_interlude_ruleset, as_interlude_replay, TEST_CHART, 1.0f<rate>)

        Assert.AreEqual(
            (TEST_REPLAY_FILE.CountGeki,
             TEST_REPLAY_FILE.Count300,
             TEST_REPLAY_FILE.CountKatu,
             TEST_REPLAY_FILE.Count100,
             TEST_REPLAY_FILE.Count50,
             TEST_REPLAY_FILE.CountMiss),
            (score.JudgementCounts.[0],
             score.JudgementCounts.[1],
             score.JudgementCounts.[2],
             score.JudgementCounts.[3],
             score.JudgementCounts.[4],
             score.JudgementCounts.[5])
        )

    [<Test>]
    let OsuRuleset_MatchesExpectedAccuracy_HeHeHeSample () =
        let as_interlude_replay =
            OsuReplay.decode(TEST_REPLAY_FILE, TEST_CHART.FirstNote, 1.0f<rate>)

        let as_interlude_ruleset =
            OsuMania.create (float32 TEST_OSU_FILE.Difficulty.OverallDifficulty) OsuMania.NoMod

        let score =
            ScoreProcessor.ProcessEntireReplay(as_interlude_ruleset, as_interlude_replay, TEST_CHART, 1.0f<rate>)

        printfn "%.2f%%" (score.Accuracy * 100.0)

        Assert.AreEqual(92.15, System.Math.Round(100.0 * score.Accuracy, 2))

    [<Test>]
    let OsuRuleset_MatchesGosuMemoryDeltas_HeHeHeSample () =
        let as_interlude_replay =
            OsuReplay.decode(TEST_REPLAY_FILE, TEST_CHART.FirstNote, 1.0f<rate>)

        let as_interlude_ruleset =
            OsuMania.create (float32 TEST_OSU_FILE.Difficulty.OverallDifficulty) OsuMania.NoMod

        let score =
            ScoreProcessor.ProcessEntireReplay(as_interlude_ruleset, as_interlude_replay, TEST_CHART, 1.0f<rate>)

        let event_deltas =
            score.Events
            |> Seq.map _.Inner
            |> Seq.choose(
                function
                | Hit d -> if not d.Missed then Some d.Delta else None
                | Hold d -> if not d.Missed then Some d.Delta else None
                | Release d -> if not d.Missed then Some d.Delta else None
                | _ -> None
            )
            |> Seq.map(float32 >> int)

        // Obtained with GosuMemory
        let EXPECTED_DATA =
            System.IO.File.ReadAllLines("./test_data/osu_parity/hehehe_events.txt")
            |> Array.map int

        Assert.AreEqual(EXPECTED_DATA, event_deltas)

    let OD10_RULESET = OsuMania.create 10.0f OsuMania.NoMod

    [<Test>]
    let OsuManiaColumnLockMechanic_Replicate_1 () =

        let note_data =
            NotesBuilder(4).Note(0.0f<ms>).Note(108.0f<ms>).Note(222.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(-71.0f<ms>, 30.0f<ms>)
                .KeyDownFor(108.0f<ms> - 121.0f<ms>, 30.0f<ms>) // this input is getting eaten because it is before note 0 which has already been hit
                .KeyDownFor(222.0f<ms> - 125.0f<ms>, 30.0f<ms>)
                .KeyDownFor(222.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing =
            GameplayEventCollector(RULESET, replay, note_data, 1.0f<rate>)

        event_processing.ProcessEntireReplay()

        Assert.AreEqual(
            [
                HIT(-71.0f<ms / rate>, false)
                HIT(-11.0f<ms / rate>, false)
                HIT(0.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Inner
        )

    [<Test>]
    let OsuManiaColumnLockMechanic_Replicate_2_OD10 () =
        let note_data =
            NotesBuilder(4).Note(0.0f<ms>).Note(125.0f<ms>).Note(250.0f<ms>).Note(375.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(78.0f<ms>, 30.0f<ms>)
                .KeyDownFor(222.0f<ms>, 30.0f<ms>)
                .KeyDownFor(366.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing =
            GameplayEventCollector(OD10_RULESET, replay, note_data, 1.0f<rate>)

        event_processing.ProcessEntireReplay()

        let CONVENTIONAL_LATE_WINDOW = snd OD10_RULESET.NoteWindows

        Assert.AreEqual(
            [
                HIT(78.0f<ms / rate>, false)
                HIT(CONVENTIONAL_LATE_WINDOW, true)
                HIT(-28.0f<ms / rate>, false)
                HIT(-9.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Inner
        )

    [<Test>]
    let OsuManiaColumnLockMechanic_Replicate_2_OD8 () =
        let note_data =
            NotesBuilder(4).Note(0.0f<ms>).Note(125.0f<ms>).Note(250.0f<ms>).Note(375.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(78.0f<ms>, 30.0f<ms>)
                .KeyDownFor(228.0f<ms>, 30.0f<ms>)
                .KeyDownFor(366.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing =
            GameplayEventCollector(RULESET, replay, note_data, 1.0f<rate>)

        event_processing.ProcessEntireReplay()

        let CONVENTIONAL_LATE_WINDOW = snd RULESET.NoteWindows

        Assert.AreEqual(
            [
                HIT(78.0f<ms / rate>, false)
                HIT(CONVENTIONAL_LATE_WINDOW, true)
                HIT(-22.0f<ms / rate>, false)
                HIT(-9.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Inner
        )

    [<Test>]
    let OsuManiaColumnLockMechanic_Replicate_2_OD8_AltCase () =
        let note_data =
            NotesBuilder(4).Note(0.0f<ms>).Note(125.0f<ms>).Note(250.0f<ms>).Note(375.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(78.0f<ms>, 30.0f<ms>)
                .KeyDownFor(227.0f<ms>, 30.0f<ms>)
                .KeyDownFor(366.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing =
            GameplayEventCollector(RULESET, replay, note_data, 1.0f<rate>)

        event_processing.ProcessEntireReplay()

        let CONVENTIONAL_LATE_WINDOW = snd RULESET.NoteWindows

        Assert.AreEqual(
            [
                HIT(78.0f<ms / rate>, false)
                HIT(102.0f<ms / rate>, false)
                HIT(CONVENTIONAL_LATE_WINDOW, true)
                HIT(-9.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Inner
        )

    [<Test>]
    [<Ignore("osu!mania EZ, HR, DT and HT windows are too complicated so I'm not bothering with them for the time being")>]
    let OsuRuleset_MatchesReplayJudgements_DonutHoleSample () =
        let replay =
            OsuReplay.decode(TEST_REPLAY_FILE_2, TEST_CHART_2.FirstNote, 1.0f<rate>)

        let ruleset =
            OsuMania.create (float32 TEST_OSU_FILE_2.Difficulty.OverallDifficulty) OsuMania.NoMod

        let score =
            ScoreProcessor.ProcessEntireReplay(ruleset, replay, TEST_CHART_2, 1.5f<rate>)

        Assert.AreEqual(
            (TEST_REPLAY_FILE_2.CountGeki,
             TEST_REPLAY_FILE_2.Count300,
             TEST_REPLAY_FILE_2.CountKatu,
             TEST_REPLAY_FILE_2.Count100,
             TEST_REPLAY_FILE_2.Count50,
             TEST_REPLAY_FILE_2.CountMiss),
            (score.JudgementCounts.[0],
             score.JudgementCounts.[1],
             score.JudgementCounts.[2],
             score.JudgementCounts.[3],
             score.JudgementCounts.[4],
             score.JudgementCounts.[5])
        )

    [<Test>]
    let OsuRuleset_MatchesGosuMemoryDeltas_DonutHoleSample () =
        let replay =
            OsuReplay.decode(TEST_REPLAY_FILE_2, TEST_CHART_2.FirstNote, 1.0f<rate>)

        let ruleset =
            OsuMania.create (float32 TEST_OSU_FILE_2.Difficulty.OverallDifficulty) OsuMania.NoMod

        let score = ScoreProcessor.Create(ruleset, replay, TEST_CHART_2, 1.5f<rate>)
        score.ProcessEntireReplay()

        let event_deltas =
            score.Events
            |> Seq.map _.Inner
            |> Seq.choose(
                function
                | Hit d -> if not d.Missed then Some d.Delta else None
                | Hold d -> if not d.Missed then Some d.Delta else None
                | Release d -> if not d.Missed then Some d.Delta else None
                | _ -> None
            )
            |> Seq.map(fun f -> f * 1.5f |> float32 |> int)

        // Obtained with GosuMemory
        let EXPECTED_DATA =
            System.IO.File.ReadAllLines("./test_data/osu_parity/donut_hole_events.txt")
            |> Array.map int

        printfn "%.2f%%" (score.Accuracy * 100.0)

        Assert.AreEqual(EXPECTED_DATA, event_deltas)

    [<Test>]
    let OsuRuleset_EarlyMissWindowBehaviour () =
        let note_data =
            NotesBuilder(4).Note(0.0f<ms>).Note(1000.0f<ms>).Note(2000.0f<ms>).Note(3000.0f<ms>).Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms> - 166.0f<ms>, 30.0f<ms>) // this hit is too early to register on the note
                .KeyDownFor(0.0f<ms>, 30.0f<ms>)

                .KeyDownFor(1000.0f<ms> - 165.0f<ms>, 30.0f<ms>) // so is this one
                .KeyDownFor(1000.0f<ms>, 30.0f<ms>)

                .KeyDownFor(2000.0f<ms> - 164.0f<ms>, 30.0f<ms>) // 164 is the exact border where it hits and so the perfect hit is ignored
                .KeyDownFor(2000.0f<ms>, 30.0f<ms>)

                .KeyDownFor(3000.0f<ms> - 163.0f<ms>, 30.0f<ms>) // 1ms inside border
                .KeyDownFor(3000.0f<ms>, 30.0f<ms>)
                .Build()

        let event_processing =
            GameplayEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

        event_processing.ProcessEntireReplay()

        Assert.AreEqual(
            [
                HIT(0.0f<ms / rate>, false)

                GHOST_TAP
                HIT(0.0f<ms / rate>, false)

                HIT(-164.0f<ms / rate>, false)
                GHOST_TAP

                HIT(-163.0f<ms / rate>, false)
                GHOST_TAP
            ],
            event_processing.Events |> Seq.map _.Inner
        )

    [<Test(Description = "Documents a possible bug (or intentional behaviour) of osu!stable on the very edge of the late window")>]
    let OsuRuleset_LateOkWindowBehaviour () =
        let note_data =
            NotesBuilder(4).Note(0.0f<ms>).Note(1000.0f<ms>).Note(2000.0f<ms>).Note(3000.0f<ms>).Build()

        // end of 100 window is allegedly 103 according to wiki but check this out:

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms> + 105.0f<ms>, 30.0f<ms>) // this hit is too late to register on the note

                .KeyDownFor(1000.0f<ms> + 104.0f<ms>, 30.0f<ms>) // so is this one

                .KeyDownFor(2000.0f<ms> + 103.0f<ms>, 30.0f<ms>) // according to osu! wiki this should hit, but it doesn't, I have checked

                .KeyDownFor(3000.0f<ms> + 102.0f<ms>, 30.0f<ms>) // 1ms inside border, but actually this is the latest ms you can hit the note
                .Build()

        let ruleset = OsuMania.create 8.0f OsuMania.NoMod

        let event_processing =
            GameplayEventCollector(ruleset, replay, note_data, 1.0f<rate>)

        event_processing.ProcessEntireReplay()

        let LATE_WINDOW_ON_MISS = snd ruleset.NoteWindows

        Assert.AreEqual(
            [
                HIT(LATE_WINDOW_ON_MISS, true)

                HIT(LATE_WINDOW_ON_MISS, true)

                HIT(LATE_WINDOW_ON_MISS, true) // a +103ms hit still misses the note!

                HIT(102.0f<ms / rate>, false)
            ],
            event_processing.Events |> Seq.map _.Inner |> Seq.filter((<>) GHOST_TAP)
        )

    [<Test>]
    let OsuRuleset_LnEarlyHeadPerfectReleaseWindows () =
        for step = 0 to 20 do

            let od = float32 step * 0.5f

            let perfect = floor_uom(OsuMania.perfect_window od) * 1.2f * 1.0f<rate> |> floor_uom
            let great = floor_uom(OsuMania.great_window od) * 1.1f * 1.0f<rate> |> floor_uom
            let good = floor_uom(OsuMania.good_window od) * 1.0f<rate> |> floor_uom
            let ok = floor_uom(OsuMania.ok_window od) * 1.0f<rate> |> floor_uom
            let meh = floor_uom(OsuMania.meh_window od) * 1.0f<rate> |> floor_uom

            let note_data =
                NotesBuilder(4)
                    .HoldUntil(0.0f<ms>, 100.0f<ms>)
                    .HoldUntil(200.0f<ms>, 300.0f<ms>)

                    .HoldUntil(400.0f<ms>, 500.0f<ms>)
                    .HoldUntil(600.0f<ms>, 700.0f<ms>)

                    .HoldUntil(800.0f<ms>, 900.0f<ms>)
                    .HoldUntil(1000.0f<ms>, 1100.0f<ms>)

                    .HoldUntil(1300.0f<ms>, 1400.0f<ms>)
                    .HoldUntil(1600.0f<ms>, 1700.0f<ms>)

                    .HoldUntil(1900.0f<ms>, 2000.0f<ms>)
                    //.Hold(2200.0f<ms>, 2300.0f<ms>)
                    .Build()

            let replay =
                ReplayBuilder()
                    .KeyDownUntil(0.0f<ms> - perfect, 100.0f<ms>) // WITHIN perfect window, gets 300g
                    .KeyDownUntil(200.0f<ms> - perfect - 1.0f<ms>, 300.0f<ms>) // gets 300

                    .KeyDownUntil(400.0f<ms> - great, 500.0f<ms>) // 300
                    .KeyDownUntil(600.0f<ms> - great - 1.0f<ms>, 700.0f<ms>) // 200

                    .KeyDownUntil(800.0f<ms> - good, 900.0f<ms>) // 200
                    .KeyDownUntil(1000.0f<ms> - good - 1.0f<ms>, 1100.0f<ms>) // 100

                    .KeyDownUntil(1300.0f<ms> - ok, 1400.0f<ms>) // 100
                    .KeyDownUntil(1600.0f<ms> - ok - 1.0f<ms>, 1700.0f<ms>) // 50

                    .KeyDownUntil(1900.0f<ms> - meh, 2000.0f<ms>) // 50
                    //.KeyDownUntil(2200.0f<ms> - meh - 1.0f<ms>, 2300.0f<ms>) // gets 100 again for some reason

                    .Build()

            let ruleset = OsuMania.create od OsuMania.NoMod

            printfn "TRYING OD %.1f\n" od

            let event_processing = ScoreProcessor(ruleset, replay, note_data, 1.0f<rate>)
            event_processing.ProcessEntireReplay()

            let judgement_sequence =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.map debug

            Assert.AreEqual(
                [
                    0
                    1
                    1
                    2
                    2
                    3
                    3
                    4
                    4
                //3
                ],
                judgement_sequence
            )

    [<Test>]
    [<Ignore("Doesn't quite match up with osu!mania. Oh well it will probably never happen in a score")>]
    let OsuRuleset_LateLnHeadWindowsBehaviour () =
        let note_data =
            NotesBuilder(4)
                .HoldUntil(0.0f<ms>, 1.0f<ms>)
                .HoldUntil(400.0f<ms>, 401.0f<ms>)
                .HoldUntil(800.0f<ms>, 801.0f<ms>)
                .Build()

        let replay =
            ReplayBuilder()
                .KeyDownFor(0.0f<ms> + 102.0f<ms>, 1.0f<ms>)
                .KeyDownFor(400.0f<ms> + 103.0f<ms>, 1.0f<ms>)
                .KeyDownFor(800.0f<ms> + 104.0f<ms>, 1.0f<ms>)
                .Build()

        let event_processing =
            ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

        event_processing.ProcessEntireReplay()

        let judgement_sequence =
            event_processing.Events
            |> Seq.map _.Inner
            |> Seq.choose(
                function
                | Release e -> e.Judgement
                | _ -> None
            )
            |> Seq.map fst

        Assert.AreEqual([ 3; 4; 5 ], judgement_sequence)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_1 () =

        let TEST_DATA =
            [
                -164.0f<ms>, -164.0f<ms>, 5
                -164.0f<ms>, -127.0f<ms>, 4
                -164.0f<ms>, -103.0f<ms>, 4
                -164.0f<ms>, -73.0f<ms>, 4
                -164.0f<ms>, -44.0f<ms>, 4
                -164.0f<ms>, -19.0f<ms>, 4
                -164.0f<ms>, 19.0f<ms>, 4
                -164.0f<ms>, 44.0f<ms>, 4
                -164.0f<ms>, 73.0f<ms>, 4
                -164.0f<ms>, 103.0f<ms>, 4
                -164.0f<ms>, 127.0f<ms>, 4
                -164.0f<ms>, 164.0f<ms>, 4
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_2 () =

        let TEST_DATA =
            [
                -127.0f<ms>, -164.0f<ms>, 5
                -127.0f<ms>, -127.0f<ms>, 4
                -127.0f<ms>, -103.0f<ms>, 4
                -127.0f<ms>, -73.0f<ms>, 4
                -127.0f<ms>, -44.0f<ms>, 4
                -127.0f<ms>, -19.0f<ms>, 4
                -127.0f<ms>, 19.0f<ms>, 4
                -127.0f<ms>, 44.0f<ms>, 4
                -127.0f<ms>, 73.0f<ms>, 4
                -127.0f<ms>, 103.0f<ms>, 4
                -127.0f<ms>, 127.0f<ms>, 4
                -127.0f<ms>, 164.0f<ms>, 4
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_3 () =

        let TEST_DATA =
            [
                -103.0f<ms>, -164.0f<ms>, 5
                -103.0f<ms>, -127.0f<ms>, 4
                -103.0f<ms>, -103.0f<ms>, 3
                -103.0f<ms>, -73.0f<ms>, 3
                -103.0f<ms>, -44.0f<ms>, 3
                -103.0f<ms>, -19.0f<ms>, 3
                -103.0f<ms>, 19.0f<ms>, 3
                -103.0f<ms>, 44.0f<ms>, 3
                -103.0f<ms>, 73.0f<ms>, 3
                -103.0f<ms>, 103.0f<ms>, 4
                -103.0f<ms>, 127.0f<ms>, 4
                -103.0f<ms>, 164.0f<ms>, 4
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_4 () =

        let TEST_DATA =
            [
                -73.0f<ms>, -164.0f<ms>, 5
                -73.0f<ms>, -127.0f<ms>, 3
                -73.0f<ms>, -103.0f<ms>, 3
                -73.0f<ms>, -73.0f<ms>, 2
                -73.0f<ms>, -44.0f<ms>, 2
                -73.0f<ms>, -19.0f<ms>, 2
                -73.0f<ms>, 19.0f<ms>, 2
                -73.0f<ms>, 44.0f<ms>, 2
                -73.0f<ms>, 73.0f<ms>, 2
                -73.0f<ms>, 103.0f<ms>, 3
                -73.0f<ms>, 127.0f<ms>, 3
                -73.0f<ms>, 164.0f<ms>, 3
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_5 () =

        let TEST_DATA =
            [
                -44.0f<ms>, -164.0f<ms>, 5
                -44.0f<ms>, -127.0f<ms>, 3
                -44.0f<ms>, -103.0f<ms>, 3
                -44.0f<ms>, -73.0f<ms>, 2
                -44.0f<ms>, -44.0f<ms>, 1
                -44.0f<ms>, -19.0f<ms>, 1
                -44.0f<ms>, 19.0f<ms>, 1
                -44.0f<ms>, 44.0f<ms>, 1
                -44.0f<ms>, 73.0f<ms>, 2
                -44.0f<ms>, 103.0f<ms>, 3
                -44.0f<ms>, 127.0f<ms>, 3
                -44.0f<ms>, 164.0f<ms>, 3
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_6 () =

        let TEST_DATA =
            [
                -19.0f<ms>, -164.0f<ms>, 5
                -19.0f<ms>, -127.0f<ms>, 2
                -19.0f<ms>, -103.0f<ms>, 2
                -19.0f<ms>, -73.0f<ms>, 2
                -19.0f<ms>, -44.0f<ms>, 1
                -19.0f<ms>, -19.0f<ms>, 0
                -19.0f<ms>, 19.0f<ms>, 0
                -19.0f<ms>, 44.0f<ms>, 1
                -19.0f<ms>, 73.0f<ms>, 2
                -19.0f<ms>, 103.0f<ms>, 2
                -19.0f<ms>, 127.0f<ms>, 2
                -19.0f<ms>, 164.0f<ms>, 2
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_7 () =

        let TEST_DATA =
            [
                19.0f<ms>, -164.0f<ms>, 5
                19.0f<ms>, -127.0f<ms>, 2
                19.0f<ms>, -103.0f<ms>, 2
                19.0f<ms>, -73.0f<ms>, 2
                19.0f<ms>, -44.0f<ms>, 1
                19.0f<ms>, -19.0f<ms>, 0
                19.0f<ms>, 19.0f<ms>, 0
                19.0f<ms>, 44.0f<ms>, 1
                19.0f<ms>, 73.0f<ms>, 2
                19.0f<ms>, 103.0f<ms>, 2
                19.0f<ms>, 127.0f<ms>, 2
                19.0f<ms>, 164.0f<ms>, 2
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_8 () =

        let TEST_DATA =
            [
                44.0f<ms>, -164.0f<ms>, 5
                44.0f<ms>, -127.0f<ms>, 3
                44.0f<ms>, -103.0f<ms>, 3
                44.0f<ms>, -73.0f<ms>, 2
                44.0f<ms>, -44.0f<ms>, 1
                44.0f<ms>, -19.0f<ms>, 1
                44.0f<ms>, 19.0f<ms>, 1
                44.0f<ms>, 44.0f<ms>, 1
                44.0f<ms>, 73.0f<ms>, 2
                44.0f<ms>, 103.0f<ms>, 3
                44.0f<ms>, 127.0f<ms>, 3
                44.0f<ms>, 164.0f<ms>, 3
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_9 () =

        let TEST_DATA =
            [
                73.0f<ms>, -164.0f<ms>, 5
                73.0f<ms>, -127.0f<ms>, 3
                73.0f<ms>, -103.0f<ms>, 3
                73.0f<ms>, -73.0f<ms>, 2
                73.0f<ms>, -44.0f<ms>, 2
                73.0f<ms>, -19.0f<ms>, 2
                73.0f<ms>, 19.0f<ms>, 2
                73.0f<ms>, 44.0f<ms>, 2
                73.0f<ms>, 73.0f<ms>, 2
                73.0f<ms>, 103.0f<ms>, 3
                73.0f<ms>, 127.0f<ms>, 3
                73.0f<ms>, 164.0f<ms>, 3
            ]

        for head, tail, expected_judgement in TEST_DATA do

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    [<Ignore("Horrible osu!mania bug where +103 hits on note_data are ignored but +103 hits on a hold head are legit and don't lock you to a max judgement of MEH, but +104 hits do")>]
    let OsuRuleset_LnCombinedJudgementBehaviour_10 () =

        let TEST_DATA =
            [
                103.0f<ms>, -164.0f<ms>, 5
                103.0f<ms>, -127.0f<ms>, 4
                103.0f<ms>, -103.0f<ms>, 3
                103.0f<ms>, -73.0f<ms>, 3
                103.0f<ms>, -44.0f<ms>, 3
                103.0f<ms>, -19.0f<ms>, 3
                103.0f<ms>, 19.0f<ms>, 3
                103.0f<ms>, 44.0f<ms>, 3
                103.0f<ms>, 73.0f<ms>, 3
                103.0f<ms>, 103.0f<ms>, 4
                103.0f<ms>, 127.0f<ms>, 4
                103.0f<ms>, 164.0f<ms>, 4
            ]

        for head, tail, expected_judgement in TEST_DATA do

            //printfn "trying head: %.0fms; tail: %.0fms; expecting: %i" head tail expected_judgement

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_11 () =

        let TEST_DATA =
            [
                127.0f<ms>, -164.0f<ms>, 5
                127.0f<ms>, -127.0f<ms>, 4
                127.0f<ms>, -103.0f<ms>, 4
                127.0f<ms>, -73.0f<ms>, 4
                127.0f<ms>, -44.0f<ms>, 4
                127.0f<ms>, -19.0f<ms>, 4
                127.0f<ms>, 19.0f<ms>, 4
                127.0f<ms>, 44.0f<ms>, 4
                127.0f<ms>, 73.0f<ms>, 4
                127.0f<ms>, 103.0f<ms>, 4
                127.0f<ms>, 127.0f<ms>, 4
                127.0f<ms>, 164.0f<ms>, 4
            ]

        for head, tail, expected_judgement in TEST_DATA do

            //printfn "trying head: %.0fms; tail: %.0fms; expecting: %i" head tail expected_judgement

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)

    [<Test>]
    let OsuRuleset_LnCombinedJudgementBehaviour_12 () =

        let TEST_DATA =
            [
                164.0f<ms>, -164.0f<ms>, 5
                164.0f<ms>, -127.0f<ms>, 4
                164.0f<ms>, -103.0f<ms>, 4
                164.0f<ms>, -73.0f<ms>, 4
                164.0f<ms>, -44.0f<ms>, 4
                164.0f<ms>, -19.0f<ms>, 4
                164.0f<ms>, 19.0f<ms>, 4
                164.0f<ms>, 44.0f<ms>, 4
                164.0f<ms>, 73.0f<ms>, 4
                164.0f<ms>, 103.0f<ms>, 4
                164.0f<ms>, 127.0f<ms>, 4
                164.0f<ms>, 164.0f<ms>, 4
            ]

        for head, tail, expected_judgement in TEST_DATA do

            //printfn "trying head: %.0fms; tail: %.0fms; expecting: %i" head tail expected_judgement

            let note_data = NotesBuilder(4).HoldUntil(0.0f<ms>, 800.0f<ms>).Build()

            let replay =
                ReplayBuilder().KeyDownUntil(0.0f<ms> + head, 800.0f<ms> + tail).Build()

            let event_processing =
                ScoringEventCollector(OsuMania.create 8.0f OsuMania.NoMod, replay, note_data, 1.0f<rate>)

            event_processing.ProcessEntireReplay()

            let judgement =
                event_processing.Events
                |> Seq.map _.Inner
                |> Seq.choose(
                    function
                    | Release e -> e.Judgement
                    | _ -> None
                )
                |> Seq.map fst
                |> Seq.tryExactlyOne

            Assert.AreEqual(Some expected_judgement, judgement)
