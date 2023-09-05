namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Gameplay
open Prelude.Data.Scores
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online

type ScoreScreen(scoreData: ScoreInfoProvider, pbs: ImprovementFlags) as this =
    inherit Screen()

    let personal_bests = ref pbs
    let grade = ref <| Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
    let lamp = ref <| Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
    let stats = ref <| ScoreScreenStats.Generate scoreData.Scoring.HitEvents
    let previous_personal_bests = 
        if Gameplay.Chart.saveData.Value.PersonalBests.ContainsKey Rulesets.current_hash then 
            Some Gameplay.Chart.saveData.Value.PersonalBests.[Rulesets.current_hash]
        else None
        |> ref
    let originalRuleset = options.SelectedRuleset.Value

    let graph = new ScoreGraph(scoreData)

    let refresh() =
        personal_bests := ImprovementFlags.Default
        grade := Grade.calculateWithTarget scoreData.Ruleset.Grading.Grades scoreData.Scoring.State
        lamp := Lamp.calculateWithTarget scoreData.Ruleset.Grading.Lamps scoreData.Scoring.State
        stats := ScoreScreenStats.Generate scoreData.Scoring.HitEvents
        previous_personal_bests := None
        graph.Refresh()

    do
        this
        |+ Results(grade, lamp, personal_bests, previous_personal_bests, scoreData, 
            Position = { Position.Default with Top = 0.0f %+ 175.0f; Bottom = 0.75f %+ 0.0f })
        |+ TopBanner(scoreData, Position = Position.SliceTop(180.0f))
        |+ BottomBanner(stats, scoreData, graph, refresh, Position = { Position.Default with Top = 0.75f %- 0.0f })
        |* Sidebar(stats, scoreData, Position = { Left = 0.0f %+ 20.0f; Top = 0.0f %+ 215.0f; Right = 0.35f %- 0.0f; Bottom = 1.0f %- 60.0f})

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

    override this.OnEnter prev =
        Screen.Toolbar.hide()

    override this.OnExit next =
        options.SelectedRuleset.Set originalRuleset
        scoreData.Ruleset <- Rulesets.current
        graph.Dispose()
        Screen.Toolbar.show()

    override this.OnBack() =
        if Network.lobby.IsSome then Some Screen.Type.Lobby
        else Some Screen.Type.LevelSelect