namespace Interlude.UI

open OpenTK
open Prelude.Gameplay.Score
open Prelude.Gameplay.Difficulty
open Prelude.Data.ScoreManager
open Interlude.Utils
open Interlude
open Interlude.UI.Components
open Interlude.Gameplay
open Interlude.Render

type ScoreGraph(data: ScoreInfoProvider) =
    inherit Widget()

    let fbo = FBO.create()
    let mutable refresh = true

    do
        fbo.Unbind()

    member this.Refresh() =
        refresh <- true

    member private this.Redraw() =
        refresh <- false
        let width = Rect.width this.Bounds
        let h = 0.5f * Rect.height this.Bounds
        let struct (left, top, right, bottom) = this.Bounds
        fbo.Bind()
        OpenTK.Graphics.OpenGL.GL.Clear(OpenTK.Graphics.OpenGL.ClearBufferMask.ColorBufferBit)

        Draw.rect(this.Bounds)(Color.FromArgb(127, 0, 0, 0))(Sprite.Default)
        Draw.rect(Rect.create left (top + h - 2.5f) right (top + h + 2.5f))(Color.FromArgb(127, 255, 255, 255))(Sprite.Default)
        
        //todo: graph stuff like hp/accuracy over time

        let w = (width - 10.0f) / float32 data.ScoreData.Length
        let mutable x = left + 5.0f
        for i in 0 .. data.ScoreData.Length - 1 do
            for k in 0 .. data.Score.keycount - 1 do
                let (_, delta, hit) = data.ScoreData.[i]
                let (y, col) =
                    match hit.[k] with
                    | HitStatus.SpecialNG -> (0.0f, otkColor Themes.themeConfig.JudgementColors.[JudgementType.NG |> int])
                    | HitStatus.NotHit -> (0.0f, otkColor Themes.themeConfig.JudgementColors.[JudgementType.MISS |> int])
                    | HitStatus.Hit -> (h + delta.[k] / MISSWINDOW * h, otkColor Themes.themeConfig.JudgementColors.[data.Accuracy.JudgeFunc(delta.[k] |> Prelude.Common.Time.Abs)|> int])
                    | _ -> (0.0f, Color.Transparent)
                if col.A > 0uy then
                    Draw.rect(Rect.create(x - 2.5f)(top + y - 2.5f)(x + 2.5f)(top + y + 2.5f))(col)(Sprite.Default)
            x <- x + w

        fbo.Unbind()

    override this.Draw() =
        if refresh then
            this.Redraw()
        Draw.rect(Render.bounds)(Color.White)(fbo.sprite)

    override this.Dispose() =
        base.Dispose()
        fbo.Dispose()

type ScreenScore(scoreData: ScoreInfoProvider, pbs) as this =
    inherit Screen()

    let mutable (lampPB, accuracyPB, clearPB) = pbs
    let mutable gradeAchieved = grade (scoreData.Accuracy.Value) (Themes.themeConfig.GradeThresholds)
    let mutable lampAchieved = lamp (scoreData.Accuracy.State) |> int
    let graph = new ScoreGraph(scoreData)

    let refresh() =
        gradeAchieved <- grade (scoreData.Accuracy.Value) (Themes.themeConfig.GradeThresholds)
        lampAchieved <- lamp (scoreData.Accuracy.State) |> int
        lampPB <- PersonalBestType.None
        accuracyPB <- PersonalBestType.None
        clearPB <- PersonalBestType.None
        graph.Refresh()

    do
        this.Add(new TextBox(K <| scoreData.Chart.Header.Artist + " - " + scoreData.Chart.Header.Title, K Color.White, 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f))
        this.Add(new TextBox(K <| sprintf "%s // %s // %s" scoreData.Chart.Header.DiffName scoreData.Mods scoreData.Chart.Header.SourcePack, K Color.White, 0.5f) |> positionWidget(0.0f, 0.0f, 80.0f, 0.0f, 0.0f, 1.0f, 150.0f, 0.0f))
        this.Add(new TextBox(scoreData.Lamp.ToString, (fun () -> otkColor Themes.themeConfig.LampColors.[lampAchieved]), 0.5f) |> positionWidget(-100.0f, 0.25f, 155.0f, 0.0f, 100.0f, 0.25f, 245.0f, 0.0f))
        this.Add(new TextBox(K <| "NEW RECORD", (fun () -> otkColor Themes.themeConfig.PBColors.[int lampPB]), 0.5f) |> positionWidget(-100.0f, 0.25f, 225.0f, 0.0f, 100.0f, 0.25f, 250.0f, 0.0f))
        this.Add(new TextBox(scoreData.Accuracy.Format, (fun () -> otkColor Themes.themeConfig.GradeColors.[gradeAchieved]), 0.5f) |> positionWidget(-100.0f, 0.5f, 155.0f, 0.0f, 100.0f, 0.5f, 245.0f, 0.0f))
        this.Add(new TextBox(K <| "NEW RECORD", (fun () -> otkColor Themes.themeConfig.PBColors.[int accuracyPB]), 0.5f) |> positionWidget(-100.0f, 0.5f, 225.0f, 0.0f, 100.0f, 0.5f, 250.0f, 0.0f))
        this.Add(new TextBox((fun () -> if scoreData.HP.Failed then "FAILED" else "CLEAR"), (fun () -> if scoreData.HP.Failed then Color.Red else Color.Green), 0.5f) |> positionWidget(-100.0f, 0.75f, 155.0f, 0.0f, 100.0f, 0.75f, 245.0f, 0.0f))
        this.Add(new TextBox(K <| "NEW RECORD", (fun () -> otkColor Themes.themeConfig.PBColors.[int clearPB]), 0.5f) |> positionWidget(-100.0f, 0.75f, 225.0f, 0.0f, 100.0f, 0.75f, 250.0f, 0.0f))
        //this.Add(new TextBox(K <| scoreData.Mods, K Color.White, 0.5f) |> positionWidget(0.0f, 0.0f, 250.0f, 0.0f, 0.0f, 1.0f, 280.0f, 0.0f))
        this.Add(graph |> positionWidget(10.0f, 0.0f, -250.0f, 1.0f, -10.0f, 1.0f, -10.0f, 1.0f))

    override this.Draw() =
        Draw.rect(Rect.sliceTop 150.0f this.Bounds)(Screens.accentShade(100, 0.8f, 0.0f))(Sprite.Default)
        Draw.rect(Rect.sliceTop 155.0f this.Bounds |> Rect.sliceBottom 5.0f)(Screens.accentShade(255, 0.8f, 0.0f))(Sprite.Default)
        Draw.rect(Rect.sliceTop 250.0f this.Bounds |> Rect.sliceBottom 100.0f)(Screens.accentShade(100, 0.6f, 0.0f))(Sprite.Default)

        let struct (left, top, right, bottom) = this.Bounds
        let w = (right + left) * 0.5f
        let h =  (bottom - 500.0f - top)
        let perfRect = Rect.create(w - h)(top + 175.0f + h * 0.5f)(w + h)(top + 325.0f + h * 0.5f)
        Draw.rect(perfRect)(Screens.accentShade(150, 0.4f, 0.0f))(Sprite.Default)
        Text.drawFill(Themes.font(), sprintf "%.2f" scoreData.Physical, perfRect, physicalColor scoreData.Physical |> otkColor, 0.0f)
        Text.drawFill(Themes.font(), sprintf "%.2f" scoreData.Technical, perfRect, technicalColor scoreData.Technical |> otkColor, 1.0f)
        Draw.quad(Quad.ofRect(Rect.create(w - h * 0.5f)(top + 250.0f)(w + h * 0.5f)(top + 250.0f + h)))(Quad.colorOf(if scoreData.HP.Failed then Color.Gray else Color.White))(Sprite.gridUV(gradeAchieved, 0)(Themes.getTexture("ranks")))
        let (judgements, pts, maxpts, combo, maxcombo, cbs) = scoreData.Accuracy.State
        for i in 1..(judgements.Length - 1) do
            Text.draw(Themes.font(), ((enum i): JudgementType).ToString() + ": " + judgements.[i].ToString(), 30.0f, 20.0f, 260.0f + 30.0f * float32 i, Color.White)
        base.Draw()

    override this.OnEnter(prev) =
        base.OnEnter(prev)
        Screens.setToolbarCollapsed(true)

    override this.OnExit(next) =
        base.OnExit(next)
        Screens.setToolbarCollapsed(false)