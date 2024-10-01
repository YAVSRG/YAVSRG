namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Gameplay
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Online

#nowarn "3370"

type ScoreScreen(score_info: ScoreInfo, pbs: ImprovementFlags, played_just_now: bool) =
    inherit Screen()

    let personal_bests = ref pbs

    let grade =
        ref
        <| Grade.calculate_with_target score_info.Ruleset.Grades score_info.Accuracy

    let lamp =
        ref
        <| Lamp.calculate_with_target score_info.Ruleset.Lamps score_info.Scoring.JudgementCounts score_info.Scoring.ComboBreaks

    let stats = ref <| ScoreScreenStats.calculate score_info.Scoring GraphSettings.column_filter

    let previous_personal_bests =
        match SelectedChart.SAVE_DATA with
        | Some d when d.PersonalBests.ContainsKey Rulesets.current_hash ->
            Some d.PersonalBests.[Rulesets.current_hash]
        | _ -> None
        |> ref

    let original_ruleset = options.SelectedRuleset.Value

    let graph = new ScoreGraph(score_info, stats)

    let refresh () =
        personal_bests := ImprovementFlags.None

        grade
        := Grade.calculate_with_target score_info.Ruleset.Grades score_info.Accuracy

        lamp
        := Lamp.calculate_with_target score_info.Ruleset.Lamps score_info.Scoring.JudgementCounts score_info.Scoring.ComboBreaks

        stats := ScoreScreenStats.calculate score_info.Scoring GraphSettings.column_filter
        previous_personal_bests := None
        graph.Refresh()

    let bottom_info =
        BottomBanner(
            score_info,
            played_just_now,
            graph,
            refresh,
            Position =
                { Position.DEFAULT with
                    Top = 0.65f %- 0.0f
                }
        )

    override this.Init(parent) =
        this
        |+ Results(
            grade,
            lamp,
            personal_bests,
            previous_personal_bests,
            score_info,
            Position =
                { Position.DEFAULT with
                    Left = 0.35f %+ 0.0f
                    Top = 0.0f %+ 175.0f
                    Bottom = 0.65f %+ 0.0f
                }
        )
        |+ TopBanner(score_info, Position = Position.SliceT(180.0f))
        |+ Sidebar(
            stats,
            score_info,
            Position =
                {
                    Left = 0.0f %+ 20.0f
                    Top = 0.0f %+ 215.0f
                    Right = 0.35f %- 0.0f
                    Bottom = 1.0f %- 70.0f
                }
        )
        |+ bottom_info
        |* Confetti()
        ScoreScreenHelpers.animation_queue.Add (Animation.Delay 1000.0)
        base.Init parent

    override this.Update(elapsed_ms, moved) = 
        ScoreScreenHelpers.animation_queue.Update elapsed_ms
        base.Update(elapsed_ms, moved)

    override this.OnEnter prev =
        Toolbar.hide ()
        DiscordRPC.in_menus ("Admiring a score")

    override this.OnExit next =
        options.SelectedRuleset.Set original_ruleset
        score_info.Ruleset <- Rulesets.current
        (graph :> System.IDisposable).Dispose()
        Toolbar.show ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.LevelSelect

    override this.Draw() =

        Draw.rect (this.Bounds.ShrinkT(175.0f).SliceT(160.0f).ShrinkT(5.0f)) Colors.shadow_2.O2
        Draw.rect (this.Bounds.ShrinkT(175.0f).ShrinkT(160.0f).SliceT(5.0f)) Colors.white
        
        Draw.rect (bottom_info.Bounds.ShrinkT 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Draw.rect (bottom_info.Bounds.SliceT 5.0f) Colors.white.O2

        base.Draw()