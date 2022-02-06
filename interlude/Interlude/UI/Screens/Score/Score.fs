namespace Interlude.UI.Screens.Score

open Prelude.Common
open Prelude.Scoring
open Prelude.Scoring.Grading
open Prelude.Data.Scores
open Interlude
open Interlude.Utils
open Interlude.Content
open Interlude.Graphics
open Interlude.UI
open Interlude.UI.Components

type EventCounts =
    {
        Notes: int * int
        Holds: int * int
        Releases: int * int

        Mean: Time
        StandardDeviation: Time
        EarlyMean: Time
        LateMean: Time

        JudgementCount: int
    }

module Helpers =

    let mutable watchReplay : ReplayData -> unit = ignore

    let countEvents(events: HitEvent<HitEventGuts> seq) : EventCounts =
        let inc (x: int ref) = x.Value <- x.Value + 1
        let (++) (x: Time ref) (t: Time) = x.Value <- x.Value + t

        let sum = ref 0.0f<ms>
        let sumOfSq = ref 0.0f<ms>
        let earlySum = ref 0.0f<ms>
        let lateSum = ref 0.0f<ms>

        let judgementCount = ref 0
        
        let notesHit = ref 0
        let notesCount = ref 0
        let holdsHeld = ref 0
        let holdsCount = ref 0
        let releasesReleased = ref 0
        let releasesCount = ref 0

        let earlyHitCount = ref 0
        let lateHitCount = ref 0

        for ev in events do
            match ev.Guts with
            | Hit e ->
                if e.IsHold then
                    if not e.Missed then inc holdsHeld
                    inc holdsCount
                else
                    if not e.Missed then inc notesHit
                    inc notesCount
                if e.Judgement.IsSome then
                    if e.Delta < 0.0f<ms> then
                        earlySum ++ e.Delta
                        inc earlyHitCount
                    else
                        lateSum ++ e.Delta
                        inc lateHitCount
                    sum ++ e.Delta
                    sumOfSq ++ e.Delta * float32 e.Delta
                    inc judgementCount
            | Release e ->
                if not e.Missed then inc releasesReleased
                inc releasesCount
                if e.Judgement.IsSome then
                    if e.Delta < 0.0f<ms> then
                        earlySum ++ e.Delta
                        inc earlyHitCount
                    else
                        lateSum ++ e.Delta
                        inc lateHitCount
                    sum ++ e.Delta
                    sumOfSq ++ e.Delta * float32 e.Delta
                    inc judgementCount

        let judgementCount = match judgementCount.Value with 0 -> 1 | n -> n
        let mean = sum.Value / float32 judgementCount
        {
            Notes = notesHit.Value, notesCount.Value
            Holds = holdsHeld.Value, holdsCount.Value
            Releases = releasesReleased.Value, releasesCount.Value

            Mean = mean
            EarlyMean = earlySum.Value / float32 earlyHitCount.Value
            LateMean = lateSum.Value / float32 lateHitCount.Value
            StandardDeviation = System.MathF.Sqrt( ((sumOfSq.Value / float32 judgementCount * 1.0f<ms>) - mean * mean) |> float32 ) * 1.0f<ms>

            JudgementCount = judgementCount
        }


type Screen(scoreData: ScoreInfoProvider, pbs: BestFlags) as this =
    inherit Screen.T()

    let mutable pbs = pbs
    let mutable gradeAchieved = Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
    let mutable lampAchieved = Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
    let mutable eventCounts = Helpers.countEvents scoreData.Scoring.HitEvents
    let graph = new ScoreGraph(scoreData)

    let originalRulesets = Options.options.Rulesets.Value
    let mutable rulesets = originalRulesets

    let refresh() =
        eventCounts <- Helpers.countEvents scoreData.Scoring.HitEvents
        gradeAchieved <- Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
        lampAchieved <- Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
        pbs <- BestFlags.Default
        graph.Refresh()

    do
        // banner text
        new TextBox(K <| scoreData.Chart.Header.Artist + " - " + scoreData.Chart.Header.Title, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        |> this.Add
        new TextBox(K <| scoreData.Chart.Header.DiffName, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 90.0f, 0.0f, 0.0f, 1.0f, 145.0f, 0.0f)
        |> this.Add
        new TextBox(K <| sprintf "From %s" scoreData.Chart.Header.SourcePack, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 140.0f, 0.0f, 0.0f, 1.0f, 180.0f, 0.0f)
        |> this.Add
        new TextBox(K <| scoreData.ScoreInfo.time.ToString(), K (Color.White, Color.Black), 1.0f)
        |> positionWidget(0.0f, 0.0f, 90.0f, 0.0f, -20.0f, 1.0f, 150.0f, 0.0f)
        |> this.Add

        // graph & under graph
        graph
        |> positionWidget(20.0f, 0.0f, -270.0f, 1.0f, -20.0f, 1.0f, -70.0f, 1.0f)
        |> this.Add

        new TextBox((fun () -> sprintf "Mean: %.1fms (%.1f - %.1fms)" eventCounts.Mean eventCounts.EarlyMean eventCounts.LateMean), K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, -65.0f, 1.0f, 620.0f, 0.0f, -15.0f, 1.0f)
        |> this.Add
        new TextBox((fun () -> sprintf "Stdev: %.1fms" eventCounts.StandardDeviation), K (Color.White, Color.Black), 0.0f)
        |> positionWidget(620.0f, 0.0f, -65.0f, 1.0f, 920.0f, 0.0f, -15.0f, 1.0f)
        |> this.Add

        new Button(ignore, "Graph settings")
        |> positionWidget(-420.0f, 1.0f, -65.0f, 1.0f, -220.0f, 1.0f, -15.0f, 1.0f)
        |> this.Add
        new Button((fun () -> Helpers.watchReplay scoreData.ReplayData), "Watch replay")
        |> positionWidget(-220.0f, 1.0f, -65.0f, 1.0f, -20.0f, 1.0f, -15.0f, 1.0f)
        |> this.Add

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds

        let halfh = (bottom + top) * 0.5f

        // accuracy - lamp - clear bars
        do
            let barh = (halfh - 195.0f) / 3.0f
            let bartop = top + 190.0f + 5.0f

            let infobar t color label text pb hint = 
                let box = Rect.create (left + 650.0f) t (right - halfh) (t + barh)
                let header = Rect.sliceLeft 200.0f box
                let body = Rect.trimLeft 200.0f box
                Draw.rect box (Color.FromArgb(80, color)) Sprite.Default
                Draw.rect (Rect.sliceBottom 35.0f header) (Color.FromArgb(120, color)) Sprite.Default
                Draw.rect (Rect.sliceBottom 5.0f body) (Color.FromArgb(120, color)) Sprite.Default
                Text.drawFillB(font, label, Rect.trimBottom 40.0f header, (Color.White, Color.Black), 0.5f)
                Text.drawFillB(font, text, body |> Rect.trimLeft 10.0f |> Rect.trimBottom 25.0f, (color, Color.Black), 0.0f)
                Text.drawFillB(font, hint, body |> Rect.trimLeft 10.0f |> Rect.sliceBottom 35.0f |> Rect.trimBottom 5.0f, (Color.White, Color.Black), 0.0f)
                if pb = PersonalBestType.None then
                    Text.drawFillB(font, "Best: --", Rect.sliceBottom 35.0f header, (Color.FromArgb(127, 200, 200, 200), Color.Black), 0.5f)
                else
                    Text.drawFillB(font, Icons.sparkle + " New record! ", Rect.sliceBottom 35.0f header, (themeConfig().PBColors.[int pb], Color.Black), 0.5f)

            infobar
                bartop 
                (scoreData.Ruleset.GradeColor gradeAchieved.Grade)
                "Score"
                (scoreData.Scoring.FormatAccuracy())
                pbs.Accuracy
                (
                    match gradeAchieved.AccuracyNeeded with
                    | Some v -> 
                        let nextgrade = scoreData.Ruleset.GradeName (gradeAchieved.Grade + 1)
                        sprintf "+%.2f%% for %s grade" (v * 100.0 + 0.004) nextgrade
                    | None -> ""
                )

            infobar
                (bartop + barh)
                (scoreData.Ruleset.LampColor lampAchieved.Lamp)
                "Lamp"
                (scoreData.Ruleset.LampName lampAchieved.Lamp)
                pbs.Lamp
                (
                    match lampAchieved.ImprovementNeeded with
                    | Some i -> 
                        let judgement = if i.Judgement < 0 then "cbs" else scoreData.Ruleset.Judgements.[i.Judgement].Name
                        let nextlamp = scoreData.Ruleset.LampName (lampAchieved.Lamp + 1)
                        sprintf "-%i %s for %s" i.LessNeeded judgement nextlamp
                    | None -> ""
                )

            infobar
                (bartop + barh * 2.0f)
                (Themes.clearToColor (not scoreData.HP.Failed))
                "HP"
                (if scoreData.HP.Failed then "FAIL" else "CLEAR")
                pbs.Clear
                ""

        // side panel
        do
            let panel = Rect.create (left + 20.0f) (top + 190.0f) (left + 650.0f) (bottom - 290.0f)
            Draw.rect (Rect.expand (5.0f, 0.0f) panel) (Color.FromArgb(127, Color.White)) Sprite.Default
            Screen.Background.draw (panel, (Color.FromArgb(50, 50, 50)), 2.0f)

            let title = Rect.sliceTop 100.0f panel |> Rect.expand (-20.0f, -20.0f)
            Draw.rect title (Color.FromArgb(127, Color.Black)) Sprite.Default
            Text.drawFillB(font, sprintf "%iK Results  •  %s" scoreData.Chart.Keys scoreData.Ruleset.Name, title, (Color.White, Color.Black), 0.5f)

            // accuracy info
            let counters = Rect.trimTop 70.0f panel |> Rect.expand (-20.0f, -20.0f) |> Rect.trimBottom 120.0f
            let struct (l, t, r, b) = counters
            Draw.rect counters (Color.FromArgb(127, Color.Black)) Sprite.Default

            let judgeCounts = scoreData.Scoring.State.Judgements
            let judgements = scoreData.Ruleset.Judgements |> Array.indexed
            let h = ((Rect.height counters) - 20.0f) / float32 judgements.Length
            let mutable y = t + 10.0f
            for i, j in judgements do
                let b = Rect.create (l + 10.0f) y (r - 10.0f) (y + h)
                Draw.rect b (Color.FromArgb(40, j.Color)) Sprite.Default
                Draw.rect (b |> Rect.sliceLeft ((r - l - 20.0f) * (float32 judgeCounts.[i] / float32 eventCounts.JudgementCount))) (Color.FromArgb(127, j.Color)) Sprite.Default
                Text.drawFill(font, sprintf "%s: %i" j.Name judgeCounts.[i], Rect.expand(-5.0f, -2.0f) b, Color.White, 0.0f)
                y <- y + h

            // stats
            let nhit, ntotal = eventCounts.Notes
            let hhit, htotal = eventCounts.Holds
            let rhit, rtotal = eventCounts.Releases
            let data = sprintf "Notes: %i/%i  •  Holds: %i/%i  •  Releases: %i/%i  •  Combo: %ix" nhit ntotal hhit htotal rhit rtotal scoreData.Scoring.State.BestCombo
            Text.drawFillB(font, data, panel |> Rect.sliceBottom 130.0f |> Rect.trimBottom 80.0f |> Rect.expand(-20.0f, -5.0f), (Color.White, Color.Black), 0.5f)
            Text.drawFillB(font, scoreData.Mods, panel |> Rect.sliceBottom 100.0f |> Rect.expand(-20.0f, -20.0f), (Color.White, Color.Black), 0.5f)

        // top banner
        Draw.rect (Rect.sliceTop 190.0f this.Bounds) (Style.accentShade(127, 0.5f, 0.0f)) Sprite.Default
        Draw.rect (Rect.create left (top + 190.0f) right (top + 195.0f)) (Color.FromArgb(127, Color.White)) Sprite.Default
        // bottom banner
        Draw.rect (Rect.sliceBottom 290.0f this.Bounds) (Style.accentShade(127, 0.5f, 0.0f)) Sprite.Default
        Draw.rect (Rect.create left (bottom - 295.0f) right (bottom - 290.0f)) (Color.FromArgb(127, Color.White)) Sprite.Default


        // right diamond
        do
            let padding = 110.0f
            let padding2 = 125.0f
            let size = (bottom - top)
            Draw.quad
                ( Quad.createv
                    (right - halfh, top + padding)
                    (right - padding, halfh)
                    (right - halfh, bottom - padding)
                    (right - size + padding, halfh)
                )
                (Quad.colorOf (scoreData.Ruleset.GradeColor gradeAchieved.Grade))
                Sprite.DefaultQuad
            Screen.Background.drawq ( 
                ( Quad.createv
                    (right - halfh, top + padding2)
                    (right - padding2, halfh)
                    (right - halfh, bottom - padding2)
                    (right - size + padding2, halfh)
                ), Color.FromArgb(60, 60, 60), 2.0f
            )
            Draw.quad
                ( Quad.createv
                    (right - halfh, top + padding2)
                    (right - padding2, halfh)
                    (right - halfh, bottom - padding2)
                    (right - size + padding2, halfh)
                )
                (Quad.colorOf (Color.FromArgb(40, (scoreData.Ruleset.GradeColor gradeAchieved.Grade))))
                Sprite.DefaultQuad

        // grade stuff
        let gradeBounds = Rect.createWH (right - halfh - 270.0f) (halfh - 305.0f) 540.0f 540.0f
        Text.drawFill(font, scoreData.Ruleset.GradeName gradeAchieved.Grade, Rect.expand (-100.0f, -100.0f) gradeBounds, scoreData.Ruleset.GradeColor gradeAchieved.Grade, 0.5f)
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (gradeAchieved.Grade, 0) <| getTexture "grade-base")
        if lampAchieved.Lamp >= 0 then Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (lampAchieved.Lamp, 0) <| getTexture "grade-lamp-overlay")
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (gradeAchieved.Grade, 0) <| getTexture "grade-overlay")

        // graph stuff
        Draw.rect (graph.Bounds |> Rect.expand (5.0f, 5.0f)) Color.White Sprite.Default
        Screen.Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 3.0f)

        base.Draw()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

        if Options.options.Hotkeys.Next.Value.Tapped() then
            rulesets <- Options.WatcherSelection.cycleForward rulesets
            Options.options.Rulesets.Value <- rulesets
            scoreData.Ruleset <- Options.getCurrentRuleset()
            refresh()
        elif Options.options.Hotkeys.Previous.Value.Tapped() then
            rulesets <- Options.WatcherSelection.cycleBackward rulesets
            Options.options.Rulesets.Value <- rulesets
            scoreData.Ruleset <- Options.getCurrentRuleset()
            refresh()

    override this.OnEnter prev =
        Screen.toolbar <- true

    override this.OnExit next =
        Options.options.Rulesets.Value <- originalRulesets
        scoreData.Ruleset <- Options.getCurrentRuleset()
        Screen.toolbar <- false