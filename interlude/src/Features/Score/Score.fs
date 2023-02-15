namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Scoring
open Prelude.Scoring.Grading
open Prelude.Data.Scores
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Utils
open Interlude.Features

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
        this
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

        |+ Sidebar(stats, scoreData, Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 190.0f; Right = 0.35f %- 0.0f; Bottom = 0.75f %- 0.0f})
        |+ TopBanner(scoreData, Position = Position.SliceTop(195.0f))
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