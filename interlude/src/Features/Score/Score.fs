namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Scoring
open Prelude.Scoring.Grading
open Prelude.Data.Scores
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features

// todo: good lord this file should be split

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

module ScoreScreenHelpers =

    let mutable watchReplay : float32 * ReplayData -> unit = ignore

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


type ScoreScreen(scoreData: ScoreInfoProvider, pbs: BestFlags) as this =
    inherit Screen()

    let mutable pbs = pbs
    let mutable gradeAchieved = Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
    let mutable lampAchieved = Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
    let mutable eventCounts = ScoreScreenHelpers.countEvents scoreData.Scoring.HitEvents
    let mutable existingBests = 
        if Gameplay.Chart.saveData.Value.Bests.ContainsKey Gameplay.rulesetId then 
            Some Gameplay.Chart.saveData.Value.Bests.[Gameplay.rulesetId]
        else None
    let graph = new ScoreGraph(scoreData, Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 90.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 150.0f })

    let originalRulesets = options.Rulesets.Value

    let refresh() =
        pbs <- BestFlags.Default
        gradeAchieved <- Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
        lampAchieved <- Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
        eventCounts <- ScoreScreenHelpers.countEvents scoreData.Scoring.HitEvents
        existingBests <- None
        graph.Refresh()

    let getPb ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) (textFunc: 'T -> string) =
        let rate = scoreData.ScoreInfo.rate
        if rate > r2 then sprintf "%s (%.2fx)" (textFunc p2) r2
        elif rate = r2 then textFunc p2
        elif rate <> r1 then sprintf "%s (%.2fx)" (textFunc p1) r1
        else textFunc p1

    do
        // banner text
        this
        |+ Text(scoreData.Chart.Header.Artist + " - " + scoreData.Chart.Header.Title,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 100.0f })
        |+ Text(scoreData.Chart.Header.DiffName,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 90.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 145.0f })
        |+ Text(sprintf "From %s" scoreData.Chart.Header.SourcePack,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 140.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 180.0f })
        |+ Text(scoreData.ScoreInfo.time.ToString(),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 90.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 150.0f })

        // graph & under graph
        |+ graph

        |+ Text((fun () -> sprintf "Mean: %.1fms (%.1f - %.1fms)" eventCounts.Mean eventCounts.EarlyMean eventCounts.LateMean),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 1.0f %- 65.0f; Right = 0.0f %+ 620.0f; Bottom = 1.0f %- 15.0f })
        |+ Text((fun () -> sprintf "Stdev: %.1fms" eventCounts.StandardDeviation),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 620.0f; Top = 1.0f %- 65.0f; Right = 0.0f %+ 920.0f; Bottom = 1.0f %- 15.0f })

        |+ Button("Graph settings", ignore, "none",
            Position = { Left = 1.0f %- 420.0f; Top = 1.0f %- 65.0f; Right = 1.0f %- 220.0f; Bottom = 1.0f %- 15.0f })
        |* Button("Watch replay",
            (fun () -> ScoreScreenHelpers.watchReplay (scoreData.ScoreInfo.rate, scoreData.ReplayData)),
            "none",
            Position = { Left = 1.0f %- 220.0f; Top = 1.0f %- 65.0f; Right = 1.0f %- 20.0f; Bottom = 1.0f %- 15.0f })

    override this.Draw() =
        let halfh = this.Bounds.CenterY
        let xadjust = 50.0f

        // accuracy - lamp - clear bars
        do
            let barh = (halfh - 195.0f) / 3.0f
            let bartop = this.Bounds.Top + 190.0f + 5.0f

            let infobar t color label text pb hint existingPb = 
                let box = Rect.Create(this.Bounds.Left + 650.0f, t, this.Bounds.Right - halfh, t + barh)
                let header = box.SliceLeft 200.0f
                let body = box.TrimLeft 200.0f
                Draw.rect box (Color.FromArgb(80, color))
                Draw.rect (header.SliceBottom 35.0f) (Color.FromArgb(80, color))
                Draw.rect (body.SliceBottom 5.0f) (Color.FromArgb(80, color))
                Text.drawFillB(font, label, header.TrimBottom 40.0f, (Color.White, Color.Black), 0.5f)
                Text.drawFillB(font, text, body.TrimLeft(10.0f).TrimBottom(25.0f), (color, Color.Black), 0.0f)
                Text.drawFillB(font, hint, body.TrimLeft(10.0f).SliceBottom(35.0f).TrimBottom(5.0f), (Color.White, Color.Black), 0.0f)
                if pb = PersonalBestType.None then
                    Text.drawFillB(font, existingPb, header.SliceBottom(35.0f), (Color.FromArgb(180, 180, 180, 180), Color.Black), 0.5f)
                else
                    Text.drawFillB(font, Icons.sparkle + " New record! ", header.SliceBottom(35.0f), (themeConfig().PBColors.[int pb], Color.Black), 0.5f)

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
                (
                    match existingBests with
                    | Some b -> getPb b.Accuracy (fun x -> sprintf "%.2f%%" (x * 100.0))
                    | None -> "--"
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
                (
                    match existingBests with
                    | Some b -> getPb b.Lamp scoreData.Ruleset.LampName
                    | None -> "--"
                )

            infobar
                (bartop + barh * 2.0f)
                (Themes.clearToColor (not scoreData.HP.Failed))
                "HP"
                (if scoreData.HP.Failed then "FAIL" else "CLEAR")
                pbs.Clear
                ""
                (
                    match existingBests with
                    | Some b -> getPb b.Clear (fun x -> if x then "CLEAR" else "FAIL")
                    | None -> "--"
                )

        // side panel
        do
            let panel = Rect.Create(this.Bounds.Left + 20.0f, this.Bounds.Top + 190.0f, this.Bounds.Left + 650.0f, this.Bounds.Bottom - 290.0f)
            Draw.rect (panel.Expand(5.0f, 0.0f)) (Color.FromArgb(127, Color.White))
            Background.draw (panel, (Color.FromArgb(80, 80, 80)), 2.0f)

            let title = panel.SliceTop(100.0f).Shrink(20.0f)
            Draw.rect title (Color.FromArgb(127, Color.Black))
            Text.drawFillB(font, sprintf "%iK Results  •  %s" scoreData.Chart.Keys scoreData.Ruleset.Name, title, (Color.White, Color.Black), 0.5f)

            // accuracy info
            let counters = panel.TrimTop(70.0f).Shrink(20.0f).TrimBottom(120.0f)
            Draw.rect counters (Color.FromArgb(127, Color.Black))

            let judgeCounts = scoreData.Scoring.State.Judgements
            let judgements = scoreData.Ruleset.Judgements |> Array.indexed
            let h = (counters.Height - 20.0f) / float32 judgements.Length
            let mutable y = counters.Top + 10.0f
            for i, j in judgements do
                let b = Rect.Create(counters.Left + 10.0f, y, counters.Right - 10.0f, y + h)
                Draw.rect b (Color.FromArgb(40, j.Color))
                Draw.rect (b.SliceLeft((counters.Width - 20.0f) * (float32 judgeCounts.[i] / float32 eventCounts.JudgementCount))) (Color.FromArgb(127, j.Color))
                Text.drawFill(font, sprintf "%s: %i" j.Name judgeCounts.[i], b.Shrink(5.0f, 2.0f), Color.White, 0.0f)
                y <- y + h

            // stats
            let nhit, ntotal = eventCounts.Notes
            let hhit, htotal = eventCounts.Holds
            let rhit, rtotal = eventCounts.Releases
            let data = sprintf "Notes: %i/%i  •  Holds: %i/%i  •  Releases: %i/%i  •  Combo: %ix" nhit ntotal hhit htotal rhit rtotal scoreData.Scoring.State.BestCombo
            Text.drawFillB(font, data, panel.SliceBottom(130.0f).TrimBottom(80.0f).Shrink(20.0f, 5.0f), (Color.White, Color.Black), 0.5f)
            Text.drawFillB(font, scoreData.Mods, panel.SliceBottom(100.0f).Shrink(20.0f), (Color.White, Color.Black), 0.5f)

        // top banner
        Draw.rect (this.Bounds.SliceTop 190.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceTop(195.0f).SliceBottom(5.0f)) (Color.FromArgb(127, Color.White))
        // bottom banner
        Draw.rect (this.Bounds.SliceBottom 290.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceBottom(295.0f).SliceTop(5.0f)) (Color.FromArgb(127, Color.White))

        // right diamond
        do
            let padding = 110.0f
            let padding2 = 125.0f
            let size = this.Bounds.Height
            Draw.quad
                ( Quad.createv
                    (this.Bounds.Right - halfh + xadjust, this.Bounds.Top + padding)
                    (this.Bounds.Right - padding + xadjust, halfh)
                    (this.Bounds.Right - halfh + xadjust, this.Bounds.Bottom - padding)
                    (this.Bounds.Right - size + padding + xadjust, halfh)
                )
                (Quad.colorOf (scoreData.Ruleset.GradeColor gradeAchieved.Grade))
                Sprite.DefaultQuad
            Background.drawq ( 
                ( Quad.createv
                    (this.Bounds.Right - halfh + xadjust, this.Bounds.Top + padding2)
                    (this.Bounds.Right - padding2 + xadjust, halfh)
                    (this.Bounds.Right - halfh + xadjust, this.Bounds.Bottom - padding2)
                    (this.Bounds.Right - size + padding2 + xadjust, halfh)
                ), Color.FromArgb(60, 60, 60), 2.0f
            )
            Draw.quad
                ( Quad.createv
                    (this.Bounds.Right - halfh + xadjust, this.Bounds.Top + padding2)
                    (this.Bounds.Right - padding2 + xadjust, halfh)
                    (this.Bounds.Right - halfh + xadjust, this.Bounds.Bottom - padding2)
                    (this.Bounds.Right - size + padding2 + xadjust, halfh)
                )
                (Quad.colorOf (Color.FromArgb(40, (scoreData.Ruleset.GradeColor gradeAchieved.Grade))))
                Sprite.DefaultQuad

        // grade stuff
        let gradeBounds = Rect.Box(this.Bounds.Right - halfh + xadjust - 270.0f, halfh - 305.0f, 540.0f, 540.0f)
        Text.drawFill(font, scoreData.Ruleset.GradeName gradeAchieved.Grade, gradeBounds.Shrink 100.0f, scoreData.Ruleset.GradeColor gradeAchieved.Grade, 0.5f)
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, gradeAchieved.Grade) <| getTexture "grade-base")
        if lampAchieved.Lamp >= 0 then Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, lampAchieved.Lamp) <| getTexture "grade-lamp-overlay")
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, gradeAchieved.Grade) <| getTexture "grade-overlay")

        // graph stuff
        Draw.rect (graph.Bounds.Expand(5.0f, 5.0f)) Color.White
        Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 3.0f)

        base.Draw()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

        if (!|"next").Tapped() then
            Setting.app WatcherSelection.cycleForward options.Rulesets
            scoreData.Ruleset <- getCurrentRuleset()
            refresh()
        elif (!|"previous").Tapped() then
            Setting.app WatcherSelection.cycleBackward options.Rulesets
            scoreData.Ruleset <- getCurrentRuleset()
            refresh()

    override this.OnEnter prev =
        Screen.Toolbar.hide()

    override this.OnExit next =
        options.Rulesets.Value <- originalRulesets
        scoreData.Ruleset <- getCurrentRuleset()
        Screen.Toolbar.show()