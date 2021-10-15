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

module WatchReplay =
    let mutable func : ReplayData -> unit = ignore

module ScoreColor =
    let lampToColor (lampAchieved: Lamp) = Themes.themeConfig.LampColors.[lampAchieved |> int]
    let gradeToColor (gradeAchieved: int) = Themes.themeConfig.GradeColors.[gradeAchieved]
    let clearToColor (cleared: bool) = if cleared then Color.FromArgb(255, 127, 255, 180) else Color.FromArgb(255, 255, 160, 140)

type Screen(scoreData: ScoreInfoProvider, pbs) as this =
    inherit Screen.T()

    let mutable (lampPB, accuracyPB, clearPB) = pbs
    let mutable gradeAchieved = Grade.calculate Themes.themeConfig.GradeThresholds scoreData.Scoring.State
    let graph = new ScoreGraph(scoreData)
    let mutable mean = 0.0f<ms>
    let mutable standardDev = 0.0f<ms>
    let mutable earlyMean = 0.0f<ms>
    let mutable lateMean = 0.0f<ms>
    let mutable normalHitCount = 1
    let mutable specialHitCount = 1

    let collect() =
        mean <- 0.0f<ms>
        standardDev <- 0.0f<ms>
        earlyMean <- 0.0f<ms>
        lateMean <- 0.0f<ms>
        normalHitCount <- 0
        specialHitCount <- 0
        let mutable sumSq = 0.0f<ms*ms>
        let mutable earlyHitCount = 0
        let mutable lateHitCount = 0
        for ev in scoreData.Scoring.HitEvents do
            match ev.Guts with
            | HitEventGuts.Hit (_, delta, isHold) ->
                if delta < 0.0f<ms> then
                    earlyMean <- earlyMean + delta
                    earlyHitCount <- earlyHitCount + 1
                else
                    lateMean <- lateMean + delta
                    lateHitCount <- lateHitCount + 1
                mean <- mean + delta
                sumSq <- sumSq + delta * delta
                normalHitCount <- normalHitCount + 1
            | HitEventGuts.Release (_, delta, overhold, dropped) ->
                if delta < 0.0f<ms> then
                    earlyMean <- earlyMean + delta
                    earlyHitCount <- earlyHitCount + 1
                else
                    lateMean <- lateMean + delta
                    lateHitCount <- lateHitCount + 1
                mean <- mean + delta
                sumSq <- sumSq + delta * delta
                normalHitCount <- normalHitCount + 1
            | HitEventGuts.Hold ->
                specialHitCount <- specialHitCount + 1
            | HitEventGuts.Mine _ ->
                specialHitCount <- specialHitCount + 1
        mean <- mean / float32 normalHitCount
        earlyMean <- earlyMean / float32 earlyHitCount
        lateMean <- lateMean / float32 lateHitCount
        standardDev <- System.MathF.Sqrt( ((sumSq / float32 normalHitCount) - mean * mean) |> float32 ) * 1.0f<ms>

    let refresh() =
        collect()
        gradeAchieved <- Grade.calculate Themes.themeConfig.GradeThresholds scoreData.Scoring.State
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
                let textW = Text.measure(Themes.font(), text()) * h * 0.5f
                let mid = (right + left) * 0.5f
                let hmid = (top + bottom) * 0.5f
                let rect = Rect.createWH (mid + textW * 0.6f - h * 0.2f) (hmid - h * 0.4f) (h * 0.4f) (h * 0.4f)
                Text.drawFill(Themes.font(), "▲", rect, Themes.themeConfig.PBColors.[int (pb())], 0.5f)
        }

    do
        collect()
        //banner text
        new TextBox(K <| scoreData.Chart.Header.Artist + " - " + scoreData.Chart.Header.Title, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 20.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        |> this.Add
        new TextBox(K <| sprintf "[%s] %s" scoreData.Chart.Header.DiffName scoreData.Mods, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 90.0f, 0.0f, 0.0f, 1.0f, 150.0f, 0.0f)
        |> this.Add
        new TextBox(K <| sprintf " From %s" scoreData.Chart.Header.SourcePack, K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 145.0f, 0.0f, 0.0f, 1.0f, 180.0f, 0.0f)
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
        pbLabel (fun () -> scoreData.Scoring.FormatAccuracy()) (fun () -> ScoreColor.gradeToColor gradeAchieved) (fun () -> accuracyPB)
        |> positionWidget(40.0f, 0.0f, -235.0f, 0.5f, 240.0f, 0.0f, -140.0f, 0.5f)
        |> this.Add
        
        new TextBox(K "Lamp", K Color.White, 0.5f)
        |> positionWidget(290.0f, 0.0f, -250.0f, 0.5f, 490.0f, 0.0f, -220.0f, 0.5f)
        |> this.Add
        pbLabel (fun () -> scoreData.Lamp.ToString()) (fun () -> ScoreColor.lampToColor scoreData.Lamp) (fun () -> lampPB)
        |> positionWidget(290.0f, 0.0f, -235.0f, 0.5f, 490.0f, 0.0f, -140.0f, 0.5f)
        |> this.Add
        
        pbLabel (K "A+") (fun () -> ScoreColor.gradeToColor gradeAchieved) (fun () -> accuracyPB)
        |> positionWidget(540.0f, 0.0f, -225.0f, 0.5f, 740.0f, 0.0f, 190.0f, 0.5f)
        |> this.Add

        // graph & under graph
        graph
        |> positionWidget(20.0f, 0.0f, -270.0f, 1.0f, -20.0f, 1.0f, -70.0f, 1.0f)
        |> this.Add

        pbLabel (fun () -> if scoreData.HP.Failed then "FAILED" else "CLEAR") (fun () -> ScoreColor.clearToColor(not scoreData.HP.Failed)) (fun () -> clearPB)
        |> positionWidget(20.0f, 0.0f, -70.0f, 1.0f, 220.0f, 0.0f, -20.0f, 1.0f)
        |> this.Add
        new TextBox((fun () -> sprintf "μ: %.1fms (%.1f - %.1fms)" mean earlyMean lateMean), K (Color.White, Color.Black), 0.0f)
        |> positionWidget(220.0f, 0.0f, -70.0f, 1.0f, 620.0f, 0.0f, -20.0f, 1.0f)
        |> this.Add
        new TextBox((fun () -> sprintf "σ: %.1fms" standardDev), K (Color.White, Color.Black), 0.0f)
        |> positionWidget(620.0f, 0.0f, -70.0f, 1.0f, 920.0f, 0.0f, -20.0f, 1.0f)
        |> this.Add

        new Button(ignore, "Graph settings")
        |> positionWidget(-420.0f, 1.0f, -70.0f, 1.0f, -220.0f, 1.0f, -20.0f, 1.0f)
        |> this.Add
        new Button((fun () -> WatchReplay.func scoreData.ReplayData), "Watch replay")
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
        let normalJudges = [JudgementType.MARVELLOUS; JudgementType.PERFECT; JudgementType.GREAT; JudgementType.GOOD; JudgementType.BAD; JudgementType.MISS]
        let normalJudges = if judgements.[int JudgementType.RIDICULOUS] > 0 then JudgementType.RIDICULOUS :: normalJudges else normalJudges
        let h = (350.0f - 45.0f) / float32 (normalJudges.Length + 2)
        let mutable y = halfh - 140.0f
        for j in normalJudges do
            let col = Themes.themeConfig.JudgementColors.[int j]
            let b = Rect.create (left + 40.0f) y (left + 530.0f) (y + h)
            Draw.rect b (Color.FromArgb(40, col)) Sprite.Default
            Draw.rect (b |> Rect.sliceLeft (490.0f * (float32 judgements.[int j] / float32 normalHitCount))) (Color.FromArgb(127, col)) Sprite.Default
            Text.drawFill(Themes.font(), sprintf "%O: %i" j judgements.[int j], b, Color.White, 0.0f)
            y <- y + h
        y <- y + 15.0f
        for j in [JudgementType.OK; JudgementType.NG] do
            let col = Themes.themeConfig.JudgementColors.[int j]
            let b = Rect.create (left + 40.0f) y (left + 530.0f) (y + h)
            Draw.rect b (Color.FromArgb(40, col)) Sprite.Default
            Draw.rect (b |> Rect.sliceLeft (490.0f * (float32 judgements.[int j] / float32 specialHitCount))) (Color.FromArgb(127, col)) Sprite.Default
            Text.drawFill(Themes.font(), sprintf "%O: %i" j judgements.[int j], b, Color.White, 0.0f)
            y <- y + h
        // combo, combo breaks

        //graph stuff
        Draw.rect (Rect.create (left + 15.0f) (bottom - 275.0f) (right - 15.0f) (bottom - 15.0f)) (Style.accentShade(50, 1.0f, 0.6f)) Sprite.Default
        Draw.rect (Rect.create (left + 20.0f) (bottom - 70.0f) (right - 20.0f) (bottom - 20.0f)) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default

        base.Draw()

    override this.OnEnter prev =
        Screen.toolbar <- true

    override this.OnExit next =
        Screen.toolbar <- false