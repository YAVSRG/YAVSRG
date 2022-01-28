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
    let mutable eventCounts = Helpers.countEvents scoreData.Scoring.HitEvents
    let graph = new ScoreGraph(scoreData)

    let originalRulesets = Options.options.Rulesets.Value
    let mutable rulesets = originalRulesets

    let refresh() =
        eventCounts <- Helpers.countEvents scoreData.Scoring.HitEvents
        gradeAchieved <- Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
        pbs <- BestFlags.Default
        graph.Refresh()

    let pbLabel text colorFunc pb =
        { new TextBox(text, (fun () -> colorFunc(), Color.Black), 0.5f) with
            override this.Draw() =
                base.Draw()
                let struct (left, top, right, bottom) = this.Bounds
                let h = System.MathF.Min(bottom - top, right - left)
                let textW = Text.measure(font, text()) * h * 0.5f
                let mid = (right + left) * 0.5f
                let hmid = (top + bottom) * 0.5f
                let rect = Rect.createWH (mid + textW * 0.6f - h * 0.2f) (hmid - h * 0.4f) (h * 0.4f) (h * 0.4f)
                Text.drawFill(font, Icons.sparkle, rect, themeConfig().PBColors.[int (pb())], 0.5f)
        }

    do
        //banner text
        new TextBox(K <| scoreData.Chart.Header.Artist + " - " + scoreData.Chart.Header.Title, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 25.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        |> this.Add
        new TextBox(K <| sprintf "[%s] %s" scoreData.Chart.Header.DiffName scoreData.Mods, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 90.0f, 0.0f, 0.0f, 1.0f, 145.0f, 0.0f)
        |> this.Add
        new TextBox(K <| sprintf " From %s" scoreData.Chart.Header.SourcePack, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 140.0f, 0.0f, 0.0f, 1.0f, 180.0f, 0.0f)
        |> this.Add
        new TextBox(K <| scoreData.ScoreInfo.time.ToString(), K (Color.White, Color.Black), 1.0f)
        |> positionWidget(0.0f, 0.0f, 90.0f, 0.0f, -20.0f, 1.0f, 150.0f, 0.0f)
        |> this.Add

        //accuracy info text
        new TextBox((fun () -> sprintf "%s" scoreData.Scoring.Name), (K (Color.White, Color.Black)), 0.5f)
        |> positionWidget(40.0f, 0.0f, -310.0f, 0.5f, 740.0f, 0.0f, -260.0f, 0.5f)
        |> this.Add

        new TextBox(K "Accuracy", K Color.White, 0.5f)
        |> positionWidget(40.0f, 0.0f, -250.0f, 0.5f, 240.0f, 0.0f, -220.0f, 0.5f)
        |> this.Add
        pbLabel (fun () -> scoreData.Scoring.FormatAccuracy()) (fun () -> scoreData.Ruleset.GradeColor gradeAchieved.Grade) (fun () -> pbs.Grade)
        |> positionWidget(40.0f, 0.0f, -235.0f, 0.5f, 240.0f, 0.0f, -140.0f, 0.5f)
        |> this.Add
        
        new TextBox(K "Lamp", K Color.White, 0.5f)
        |> positionWidget(290.0f, 0.0f, -250.0f, 0.5f, 490.0f, 0.0f, -220.0f, 0.5f)
        |> this.Add
        pbLabel (fun () -> scoreData.Ruleset.LampName scoreData.Lamp) (fun () -> scoreData.Ruleset.LampColor scoreData.Lamp) (fun () -> pbs.Lamp)
        |> positionWidget(290.0f, 0.0f, -235.0f, 0.5f, 490.0f, 0.0f, -140.0f, 0.5f)
        |> this.Add
        
        pbLabel (fun () -> scoreData.Ruleset.GradeName gradeAchieved.Grade) (fun () -> scoreData.Ruleset.GradeColor gradeAchieved.Grade) (fun () -> pbs.Accuracy)
        |> positionWidget(540.0f, 0.0f, -225.0f, 0.5f, 740.0f, 0.0f, 190.0f, 0.5f)
        |> this.Add

        new TextBox (
            ( fun () ->
                let nhit, ntotal = eventCounts.Notes
                let hhit, htotal = eventCounts.Holds
                let rhit, rtotal = eventCounts.Releases
                sprintf "Notes: %i/%i  •  Holds: %i/%i  •  Releases: %i/%i  •  Combo: %ix" nhit ntotal hhit htotal rhit rtotal scoreData.Scoring.State.BestCombo ),
            K (Color.White, Color.Black), 0.5f )
        |> positionWidget(40.0f, 0.0f, 130.0f, 0.5f, 740.0f, 0.0f, 195.0f, 0.5f)
        |> this.Add

        // graph & under graph
        graph
        |> positionWidget(20.0f, 0.0f, -270.0f, 1.0f, -20.0f, 1.0f, -70.0f, 1.0f)
        |> this.Add

        pbLabel (fun () -> if scoreData.HP.Failed then "FAILED" else "CLEAR") (fun () -> Themes.clearToColor(not scoreData.HP.Failed)) (fun () -> pbs.Clear)
        |> positionWidget(20.0f, 0.0f, -70.0f, 1.0f, 220.0f, 0.0f, -20.0f, 1.0f)
        |> this.Add
        new TextBox((fun () -> sprintf "μ: %.1fms (%.1f - %.1fms)" eventCounts.Mean eventCounts.EarlyMean eventCounts.LateMean), K (Color.White, Color.Black), 0.0f)
        |> positionWidget(220.0f, 0.0f, -70.0f, 1.0f, 620.0f, 0.0f, -20.0f, 1.0f)
        |> this.Add
        new TextBox((fun () -> sprintf "σ: %.1fms" eventCounts.StandardDeviation), K (Color.White, Color.Black), 0.0f)
        |> positionWidget(620.0f, 0.0f, -70.0f, 1.0f, 920.0f, 0.0f, -20.0f, 1.0f)
        |> this.Add

        new Button(ignore, "Graph settings")
        |> positionWidget(-420.0f, 1.0f, -70.0f, 1.0f, -220.0f, 1.0f, -20.0f, 1.0f)
        |> this.Add
        new Button((fun () -> Helpers.watchReplay scoreData.ReplayData), "Watch replay")
        |> positionWidget(-220.0f, 1.0f, -70.0f, 1.0f, -20.0f, 1.0f, -20.0f, 1.0f)
        |> this.Add

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds

        let halfh = (bottom + top) * 0.5f

        //top banner
        Draw.rect (Rect.create left (top + 15.0f) right (top + 20.0f)) (Style.accentShade(255, 0.6f, 0.0f)) Sprite.Default
        Draw.rect (Rect.create left (top + 30.0f) right (top + 180.0f)) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default
        Draw.rect (Rect.create left (top + 190.0f) right (top + 195.0f)) (Style.accentShade(255, 0.6f, 0.0f)) Sprite.Default

        //accuracy info
        Draw.rect (Rect.create (left + 15.0f) (halfh - 255.0f) (left + 765f) (halfh + 205.0f)) (Style.accentShade(50, 1.0f, 0.6f)) Sprite.Default
        Draw.rect (Rect.create (left + 20.0f) (halfh - 250.0f) (left + 760f) (halfh + 200.0f)) (Color.FromArgb(160, 0, 0, 0)) Sprite.Default

        let judgeCounts = scoreData.Scoring.State.Judgements
        let judgements = scoreData.Ruleset.Judgements |> Array.indexed
        let h = (350.0f - 80.0f) / float32 judgements.Length
        let mutable y = halfh - 140.0f
        for i, j in judgements do
            let b = Rect.create (left + 40.0f) y (left + 530.0f) (y + h)
            Draw.rect b (Color.FromArgb(40, j.Color)) Sprite.Default
            Draw.rect (b |> Rect.sliceLeft (490.0f * (float32 judgeCounts.[i] / float32 eventCounts.JudgementCount))) (Color.FromArgb(127, j.Color)) Sprite.Default
            Text.drawFill(font, sprintf "%s: %i" j.Name judgeCounts.[i], Rect.expand(-5.0f, 0.0f) b, Color.White, 0.0f)
            y <- y + h

        //graph stuff
        Draw.rect (Rect.create (left + 15.0f) (bottom - 275.0f) (right - 15.0f) (bottom - 15.0f)) (Style.accentShade(50, 1.0f, 0.6f)) Sprite.Default
        Draw.rect (Rect.create (left + 20.0f) (bottom - 70.0f) (right - 20.0f) (bottom - 20.0f)) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default

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