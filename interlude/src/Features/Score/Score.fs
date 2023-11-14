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

#nowarn "3370"

type ScoreScreen(score_info: ScoreInfoProvider, pbs: ImprovementFlags) as this =
    inherit Screen()

    let personal_bests = ref pbs

    let grade =
        ref
        <| Grade.calculate_with_target score_info.Ruleset.Grading.Grades score_info.Accuracy

    let lamp =
        ref
        <| Lamp.calculateWithTarget score_info.Ruleset.Grading.Lamps score_info.Scoring.State

    let stats = ref <| ScoreScreenStats.Generate score_info.Scoring.HitEvents

    let previous_personal_bests =
        if Gameplay.Chart.SAVE_DATA.Value.PersonalBests.ContainsKey Rulesets.current_hash then
            Some Gameplay.Chart.SAVE_DATA.Value.PersonalBests.[Rulesets.current_hash]
        else
            None
        |> ref

    let original_ruleset = options.SelectedRuleset.Value

    let graph = new ScoreGraph(score_info)

    let refresh () =
        personal_bests := ImprovementFlags.Default

        grade
        := Grade.calculate_with_target score_info.Ruleset.Grading.Grades score_info.Accuracy

        lamp
        := Lamp.calculateWithTarget score_info.Ruleset.Grading.Lamps score_info.Scoring.State

        stats := ScoreScreenStats.Generate score_info.Scoring.HitEvents
        previous_personal_bests := None
        graph.Refresh()

    do
        this
        |+ Results(
            grade,
            lamp,
            personal_bests,
            previous_personal_bests,
            score_info,
            Position =
                { Position.Default with
                    Top = 0.0f %+ 175.0f
                    Bottom = 0.75f %+ 0.0f
                }
        )
        |+ TopBanner(score_info, Position = Position.SliceTop(180.0f))
        |+ BottomBanner(
            stats,
            score_info,
            graph,
            refresh,
            Position =
                { Position.Default with
                    Top = 0.75f %- 0.0f
                }
        )
        |* Sidebar(
            stats,
            score_info,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 215.0f
                    Right = 0.35f %- 0.0f
                    Bottom = 1.0f %- 60.0f
                }
        )

    override this.Update(elapsed_ms, moved) = base.Update(elapsed_ms, moved)

    override this.OnEnter prev =
        Toolbar.hide ()
        DiscordRPC.in_menus ("Admiring a score")

    override this.OnExit next =
        options.SelectedRuleset.Set original_ruleset
        score_info.Ruleset <- Rulesets.current
        graph.Dispose()
        Toolbar.show ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.LevelSelect
