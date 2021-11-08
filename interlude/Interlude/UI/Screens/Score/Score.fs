namespace Interlude.UI.Screens.Score

open System.Drawing
open Prelude.Common
open Prelude.Scoring
open Prelude.Data.ScoreManager
open Interlude
open Interlude.Utils
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

    let lampToColor (lampAchieved: Lamp) = Content.themeConfig().LampColors.[lampAchieved |> int]
    let gradeToColor (gradeAchieved: int) = Content.themeConfig().GradeColors.[gradeAchieved]
    let clearToColor (cleared: bool) = if cleared then Color.FromArgb(255, 127, 255, 180) else Color.FromArgb(255, 255, 160, 140)

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



type Screen(scoreData: ScoreInfoProvider, pbs) as this =
    inherit Screen.T()

    let mutable (lampPB, accuracyPB, clearPB) = pbs
    let mutable gradeAchieved = Grade.calculate (Content.themeConfig().GradeThresholds) scoreData.Scoring.State
    let graph = new ScoreGraph(scoreData)

    let mutable eventCounts = Helpers.countEvents scoreData.Scoring.HitEvents

    let refresh() =
        eventCounts <- Helpers.countEvents scoreData.Scoring.HitEvents
        gradeAchieved <- Grade.calculate (Content.themeConfig().GradeThresholds) scoreData.Scoring.State
        lampPB <- PersonalBestType.None
        accuracyPB <- PersonalBestType.None
        clearPB <- PersonalBestType.None
        graph.Refresh()

    let pbLabel text colorFunc pb =
        { new TextBox(text, (fun () -> colorFunc(), Color.Black), 0.5f) with
            override this.Draw() =
                base.Draw()
                let struct (left, top, right, bottom) = this.Bounds
                let h = System.MathF.Min(bottom - top, right - left)
                let textW = Text.measure(Content.font(), text()) * h * 0.5f
                let mid = (right + left) * 0.5f
                let hmid = (top + bottom) * 0.5f
                let rect = Rect.createWH (mid + textW * 0.6f - h * 0.2f) (hmid - h * 0.4f) (h * 0.4f) (h * 0.4f)
                Text.drawFill(Content.font(), "▲", rect, Content.themeConfig().PBColors.[int (pb())], 0.5f)
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
        new TextBox((fun () -> sprintf "%s ~ %s" scoreData.Scoring.Name scoreData.Scoring.HP.Name), (K (Color.White, Color.Black)), 0.5f)
        |> positionWidget(40.0f, 0.0f, -310.0f, 0.5f, 740.0f, 0.0f, -260.0f, 0.5f)
        |> this.Add

        new TextBox(K "Accuracy", K Color.White, 0.5f)
        |> positionWidget(40.0f, 0.0f, -250.0f, 0.5f, 240.0f, 0.0f, -220.0f, 0.5f)
        |> this.Add
        pbLabel (fun () -> scoreData.Scoring.FormatAccuracy()) (fun () -> Helpers.gradeToColor gradeAchieved) (fun () -> accuracyPB)
        |> positionWidget(40.0f, 0.0f, -235.0f, 0.5f, 240.0f, 0.0f, -140.0f, 0.5f)
        |> this.Add
        
        new TextBox(K "Lamp", K Color.White, 0.5f)
        |> positionWidget(290.0f, 0.0f, -250.0f, 0.5f, 490.0f, 0.0f, -220.0f, 0.5f)
        |> this.Add
        pbLabel (fun () -> scoreData.Lamp.ToString()) (fun () -> Helpers.lampToColor scoreData.Lamp) (fun () -> lampPB)
        |> positionWidget(290.0f, 0.0f, -235.0f, 0.5f, 490.0f, 0.0f, -140.0f, 0.5f)
        |> this.Add
        
        pbLabel (K "A+") (fun () -> Helpers.gradeToColor gradeAchieved) (fun () -> accuracyPB)
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

        pbLabel (fun () -> if scoreData.HP.Failed then "FAILED" else "CLEAR") (fun () -> Helpers.clearToColor(not scoreData.HP.Failed)) (fun () -> clearPB)
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

        let judgements = scoreData.Scoring.State.Judgements
        let judges = [JudgementType.MARVELLOUS; JudgementType.PERFECT; JudgementType.GREAT; JudgementType.GOOD; JudgementType.BAD; JudgementType.MISS]
        let judges = if judgements.[int JudgementType.RIDICULOUS] > 0 then JudgementType.RIDICULOUS :: judges else judges
        let h = (350.0f - 80.0f) / float32 judges.Length
        let mutable y = halfh - 140.0f
        for j in judges do
            let col = Content.themeConfig().JudgementColors.[int j]
            let name = Content.themeConfig().JudgementNames.[int j]
            let b = Rect.create (left + 40.0f) y (left + 530.0f) (y + h)
            Draw.rect b (Color.FromArgb(40, col)) Sprite.Default
            Draw.rect (b |> Rect.sliceLeft (490.0f * (float32 judgements.[int j] / float32 eventCounts.JudgementCount))) (Color.FromArgb(127, col)) Sprite.Default
            Text.drawFill(Content.font(), sprintf "%s: %i" name judgements.[int j], b, Color.White, 0.0f)
            y <- y + h

        //graph stuff
        Draw.rect (Rect.create (left + 15.0f) (bottom - 275.0f) (right - 15.0f) (bottom - 15.0f)) (Style.accentShade(50, 1.0f, 0.6f)) Sprite.Default
        Draw.rect (Rect.create (left + 20.0f) (bottom - 70.0f) (right - 20.0f) (bottom - 20.0f)) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default

        base.Draw()

    override this.OnEnter prev =
        Screen.toolbar <- true

    override this.OnExit next =
        Screen.toolbar <- false