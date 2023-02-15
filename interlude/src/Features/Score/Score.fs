namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Scoring
open Prelude.Scoring.Grading
open Prelude.Data.Scores
open Prelude.Gameplay.Mods
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Components
open Interlude.Utils
open Interlude.Features

// todo: split this file

type ScoreScreenStats =
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
    static member Generate(events: HitEvent<HitEventGuts> seq) =
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

module ScoreScreenHelpers =

    let mutable watchReplay : ModChart * float32 * ReplayData -> unit = ignore

type TopBanner(data: ScoreInfoProvider) as this =
    inherit StaticContainer(NodeType.None)

    do
        this
        |+ Text(data.Chart.Header.Artist + " - " + data.Chart.Header.Title,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 100.0f })
        |+ Text(data.Chart.Header.DiffName,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 90.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 145.0f })
        |+ Text(sprintf "From %s" data.Chart.Header.SourcePack,
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 140.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 180.0f })
        |+ Text(data.ScoreInfo.time.ToString(),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 90.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 150.0f })
        |* Text((fun () -> "Current session: " + Stats.session_length()),
            Align = Alignment.RIGHT,
            Position = { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 140.0f; Right = 1.0f %- 20.0f; Bottom = 0.0f %+ 180.0f })

    override this.Draw() =

        Draw.rect (this.Bounds.TrimBottom 5.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceBottom 5.0f) (Color.FromArgb(127, Color.White))

        base.Draw()

type BottomBanner(stats: ScoreScreenStats ref, data: ScoreInfoProvider, graph: ScoreGraph, refresh: unit -> unit) as this =
    inherit StaticContainer(NodeType.None)
    
    do
        graph.Position <- { Left = 0.35f %+ 20.0f; Top = 0.0f %+ 20.0f; Right = 1.0f %- 20.0f; Bottom = 1.0f %- 70.0f }
        this
        |+ graph
        |+ Text((fun () -> sprintf "Mean: %.1fms (%.1f - %.1fms)" (!stats).Mean (!stats).EarlyMean (!stats).LateMean),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 20.0f; Top = 1.0f %- 55.0f; Right = 0.0f %+ 620.0f; Bottom = 1.0f %- 5.0f })
        |+ Text((fun () -> sprintf "Stdev: %.1fms" (!stats).StandardDeviation),
            Align = Alignment.LEFT,
            Position = { Left = 0.0f %+ 620.0f; Top = 1.0f %- 55.0f; Right = 0.0f %+ 920.0f; Bottom = 1.0f %- 5.0f })
    
        |+ StylishButton(
            ignore,
            sprintf "%s %s" Icons.edit (L"score.graph.settings") |> K,
            Style.main 100,
            Position = { Left = 0.55f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.7f %- 25.0f; Bottom = 1.0f %- 0.0f })
        |+ StylishButton(
            (fun () -> ScoreScreenHelpers.watchReplay (data.ModChart, data.ScoreInfo.rate, data.ReplayData)),
            sprintf "%s %s" Icons.preview (L"score.watch_replay") |> K,
            Style.dark 100,
            Position = { Left = 0.7f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.85f %- 25.0f; Bottom = 1.0f %- 0.0f })
        |* Rulesets.QuickSwitcher(
            options.SelectedRuleset
            |> Setting.trigger (fun _ -> data.Ruleset <- Rulesets.current; refresh()),
            Position = { Left = 0.85f %+ 0.0f; Top = 1.0f %- 50.0f; Right = 1.0f %- 0.0f; Bottom = 1.0f %- 0.0f })

    override this.Draw() =

        Draw.rect (this.Bounds.TrimTop 5.0f) (Style.color(127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceTop 5.0f) (Color.FromArgb(127, Color.White))

        // graph background
        Draw.rect (graph.Bounds.Expand(5.0f, 5.0f)) Color.White
        Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 3.0f)

        base.Draw()

type Sidebar(stats: ScoreScreenStats ref, data: ScoreInfoProvider) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect (this.Bounds.Expand(5.0f, 0.0f)) (Color.FromArgb(127, Color.White))
        Background.draw (this.Bounds, (Color.FromArgb(80, 80, 80)), 2.0f)

        let title = this.Bounds.SliceTop(100.0f).Shrink(5.0f, 20.0f)
        Draw.rect title (Color.FromArgb(127, Color.Black))
        Text.drawFillB(Style.baseFont, sprintf "%iK Results  •  %s" data.Chart.Keys data.Ruleset.Name, title, (Color.White, Color.Black), 0.5f)
        let mods = title.Translate(0.0f, 70.0f)
        Draw.rect mods (Color.FromArgb(127, Color.Black))
        Text.drawFillB(Style.baseFont, data.Mods, mods, (Color.White, Color.Black), 0.5f)

        // accuracy info
        let counters = Rect.Box(this.Bounds.Left + 5.0f, this.Bounds.Top + 160.0f, this.Bounds.Width - 10.0f, 310.0f)
        Draw.rect counters (Color.FromArgb(127, Color.Black))

        let judgeCounts = data.Scoring.State.Judgements
        let judgements = data.Ruleset.Judgements |> Array.indexed
        let h = (counters.Height - 20.0f) / float32 judgements.Length
        let mutable y = counters.Top + 10.0f
        for i, j in judgements do
            let b = Rect.Create(counters.Left + 10.0f, y, counters.Right - 10.0f, y + h)
            Draw.rect b (Color.FromArgb(40, j.Color))
            Draw.rect (b.SliceLeft((counters.Width - 20.0f) * (float32 judgeCounts.[i] / float32 (!stats).JudgementCount))) (Color.FromArgb(127, j.Color))
            Text.drawFill(Style.baseFont, sprintf "%s: %i" j.Name judgeCounts.[i], b.Shrink(5.0f, 2.0f), Color.White, 0.0f)
            y <- y + h

        // stats
        let nhit, ntotal = (!stats).Notes
        let hhit, htotal = (!stats).Holds
        let rhit, rtotal = (!stats).Releases
        let stats = sprintf "Notes: %i/%i  •  Holds: %i/%i  •  Releases: %i/%i  •  Combo: %ix" nhit ntotal hhit htotal rhit rtotal data.Scoring.State.BestCombo
        Text.drawFillB(Style.baseFont, stats, this.Bounds.SliceBottom(80.0f).Shrink(5.0f, 5.0f), (Color.White, Color.Black), 0.5f)
        
type Grade(grade: Grade.GradeResult ref, lamp: Lamp.LampResult ref, data: ScoreInfoProvider) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let x = this.Bounds.CenterX
        let y = this.Bounds.CenterY - 35.0f
        let size = (min this.Bounds.Height this.Bounds.Width) * 0.5f - 50.0f
        let borderSize = size + 15.0f
        Draw.quad
            ( Quad.createv
                (x, y - borderSize)
                (x + borderSize, y)
                (x, y + borderSize)
                (x - borderSize, y)
            )
            (Quad.colorOf (data.Ruleset.GradeColor (!grade).Grade))
            Sprite.DefaultQuad
        Background.drawq ( 
            ( Quad.createv
                (x, y - size)
                (x + size, y)
                (x, y + size)
                (x - size, y)
            ), Color.FromArgb(60, 60, 60), 2.0f
        )
        Draw.quad
            ( Quad.createv
                (x, y - size)
                (x + size, y)
                (x, y + size)
                (x - size, y)
            )
            (Quad.colorOf (Color.FromArgb(40, (data.Ruleset.GradeColor (!grade).Grade))))
            Sprite.DefaultQuad

        // grade stuff
        let gradeBounds = Rect.Box(x - 270.0f, y - 270.0f, 540.0f, 540.0f)
        Text.drawFill(Style.baseFont, data.Ruleset.GradeName (!grade).Grade, gradeBounds.Shrink 100.0f, data.Ruleset.GradeColor (!grade).Grade, 0.5f)
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, (!grade).Grade) <| getTexture "grade-base")
        if (!lamp).Lamp >= 0 then Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, (!lamp).Lamp) <| getTexture "grade-lamp-overlay")
        Draw.quad (Quad.ofRect gradeBounds) (Quad.colorOf Color.White) (Sprite.gridUV (0, (!grade).Grade) <| getTexture "grade-overlay")
        
type InfoBar(color: unit -> System.Drawing.Color, label: string, text: unit -> string, pb: unit -> PersonalBestType, hint: unit -> string, existingPb: unit -> string) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let color = color()
        let pb = pb()
        let header = this.Bounds.SliceLeft 200.0f
        let body = this.Bounds.TrimLeft 200.0f
        Draw.rect this.Bounds (Color.FromArgb(80, color))
        let header_card = header.SliceBottom (header.Height * 0.35f)
        Draw.rect header_card (Color.FromArgb(80, color))
        Draw.rect (body.SliceBottom 5.0f) (Color.FromArgb(80, color))
        Text.drawFillB(Style.baseFont, label, header.TrimBottom (header.Height * 0.35f), (Color.White, Color.Black), 0.5f)
        Text.drawFillB(Style.baseFont, text(), body.TrimLeft(10.0f).TrimBottom(header.Height * 0.3f), (color, Color.Black), 0.0f)
        Text.drawFillB(Style.baseFont, hint(), body.TrimLeft(10.0f).SliceBottom(header.Height * 0.35f).TrimBottom(5.0f), (Color.White, Color.Black), 0.0f)
        if pb = PersonalBestType.None then
            Text.drawFillB(Style.baseFont, existingPb(), header_card, (Color.FromArgb(180, 180, 180, 180), Color.Black), 0.5f)
        else
            Text.drawFillB(Style.baseFont, sprintf "%s %s " Icons.sparkle (L"score.new_record"), header_card, (themeConfig().PBColors.[int pb], Color.Black), 0.5f)

type ScoreScreen(scoreData: ScoreInfoProvider, pbs: BestFlags) as this =
    inherit Screen()

    let mutable personal_bests = pbs
    let grade = ref <| Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
    let lamp = ref <| Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
    let stats = ref <| ScoreScreenStats.Generate scoreData.Scoring.HitEvents
    let mutable previous_personal_bests = 
        if Gameplay.Chart.saveData.Value.Bests.ContainsKey Rulesets.current_hash then 
            Some Gameplay.Chart.saveData.Value.Bests.[Rulesets.current_hash]
        else None
    let originalRuleset = options.SelectedRuleset.Value

    let getPb ({ Best = p1, r1; Fastest = p2, r2 }: PersonalBests<'T>) (textFunc: 'T -> string) =
        let rate = scoreData.ScoreInfo.rate
        if rate > r2 then sprintf "%s (%.2fx)" (textFunc p2) r2
        elif rate = r2 then textFunc p2
        elif rate <> r1 then sprintf "%s (%.2fx)" (textFunc p1) r1
        else textFunc p1

    let graph = new ScoreGraph(scoreData)

    let refresh() =
        personal_bests <- BestFlags.Default
        grade := Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
        lamp := Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
        stats := ScoreScreenStats.Generate scoreData.Scoring.HitEvents
        previous_personal_bests <- None
        graph.Refresh()

    do
        // banner text
        this

        |+ Sidebar(stats, scoreData, Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 190.0f; Right = 0.35f %- 0.0f; Bottom = 0.75f %- 0.0f})
        |+ TopBanner(scoreData, Position = Position.SliceTop(195.0f))

        |+ InfoBar(
            (fun () -> scoreData.Ruleset.GradeColor (!grade).Grade),
            "Score",
            (fun () -> scoreData.Scoring.FormatAccuracy()),
            (fun () -> personal_bests.Accuracy),
            (fun () ->
                match (!grade).AccuracyNeeded with
                | Some v -> 
                    let nextgrade = scoreData.Ruleset.GradeName ((!grade).Grade + 1)
                    sprintf "+%.2f%% for %s grade" (v * 100.0 + 0.004) nextgrade
                | None -> ""
            ),
            (fun () ->
                match previous_personal_bests with
                | Some b -> getPb b.Accuracy (fun x -> sprintf "%.2f%%" (x * 100.0))
                | None -> "--"
            ),
            Position = { Left = 0.35f %+ 0.0f; Top = 0.0f %+ 190.0f; Right = 0.83f %- 0.0f; Bottom = (0.5f / 3.0f) %+ (190.0f * (2.0f / 3.0f)) }
            )

        |+ InfoBar(
            (fun () -> scoreData.Ruleset.LampColor (!lamp).Lamp),
            "Lamp",
            (fun () -> scoreData.Ruleset.LampName (!lamp).Lamp),
            (fun () -> personal_bests.Lamp),
            (fun () ->
                match (!lamp).ImprovementNeeded with
                | Some i -> 
                    let judgement = if i.Judgement < 0 then "cbs" else scoreData.Ruleset.Judgements.[i.Judgement].Name
                    let nextlamp = scoreData.Ruleset.LampName ((!lamp).Lamp + 1)
                    sprintf "-%i %s for %s" i.LessNeeded judgement nextlamp
                | None -> ""
            ),
            (fun () ->
                match previous_personal_bests with
                | Some b -> getPb b.Lamp scoreData.Ruleset.LampName
                | None -> "--"
            ),
            Position = { Left = 0.35f %+ 0.0f; Top = (0.5f / 3.0f) %+ (190.0f * (2.0f / 3.0f)); Right = 0.83f %- 0.0f; Bottom = (1.0f / 3.0f) %+ (190.0f / 3.0f) }
            )
        
        |+ InfoBar(
            (fun () -> Themes.clearToColor (not scoreData.HP.Failed)),
            "HP",
            (fun () -> if scoreData.HP.Failed then "FAIL" else "CLEAR"),
            (fun () -> personal_bests.Clear),
            K "",
            (fun () ->
                match previous_personal_bests with
                | Some b -> getPb b.Clear (fun x -> if x then "CLEAR" else "FAIL")
                | None -> "--"
            ),
            Position = { Left = 0.35f %+ 0.0f; Top = (1.0f / 3.0f) %+ (190.0f / 3.0f); Right = 0.83f %- 0.0f; Bottom = 0.5f %+ 0.0f }
            )
            
        |+ Grade(grade, lamp, scoreData, Position = { Position.Default with Left = 0.66f %+ 0.0f })

        |* BottomBanner(stats, scoreData, graph, refresh, Position = { Position.Default with Top = 0.75f %- 5.0f })

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

    override this.OnEnter prev =
        Screen.Toolbar.hide()

    override this.OnExit next =
        options.SelectedRuleset.Set originalRuleset
        scoreData.Ruleset <- Rulesets.current
        graph.Dispose()
        Screen.Toolbar.show()