namespace Interlude.Features.Score

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Gameplay
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Prelude.Data.User.Stats
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Online

#nowarn "3370"

// todo: refactor to a score-results object that is optional (played_just_now can be implied by it being Some)
type ScoreScreen(score_info: ScoreInfo, results: ImprovementFlags * SessionXPGain option, played_just_now: bool) =
    inherit Screen()

    let pbs, xp_gain = results
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

    let on_ruleset_changed = Rulesets.on_changed.Subscribe (fun _ -> GameThread.defer refresh)

    let bottom_info =
        BottomBanner(
            score_info,
            played_just_now,
            graph,
            refresh
        )
            .Position(Position.SlicePercentB(0.35f))

    override this.Init(parent) =
        this
        |+ Results(
            grade,
            lamp,
            personal_bests,
            previous_personal_bests,
            stats,
            score_info
        )
            .Position(
                { Position.DEFAULT with
                    Left = 0.35f %+ 0.0f
                    Top = 0.0f %+ 175.0f
                    Bottom = 0.65f %+ 0.0f
                }
            )
        |+ TopBanner(score_info)
            .Position(Position.SliceT(180.0f))
        |+ Sidebar(
            stats,
            score_info
        )
            .Position(
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

        //match xp_gain with
        //| Some x ->
        //    SessionScoreBar(x)
        //        .Position(Position.SlicePercentR(0.65f).ShrinkT(395.0f).SliceT(40.0f).ShrinkX(40.0f))
        //    |> this.Add
        //| None -> ()

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        ScoreScreenHelpers.animation_queue.Update elapsed_ms
        base.Update(elapsed_ms, moved)

    override this.OnEnter prev =
        Toolbar.hide ()
        DiscordRPC.in_menus ("Admiring a score")

    override this.OnExit next =
        score_info.Ruleset <- Rulesets.current
        (graph :> System.IDisposable).Dispose()
        on_ruleset_changed.Dispose()
        Toolbar.show ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some ScreenType.Lobby
        else
            Some ScreenType.LevelSelect

    override this.Draw() =

        Render.rect (this.Bounds.ShrinkT(175.0f).SliceT(160.0f).ShrinkT(5.0f)) Colors.shadow_2.O2
        Render.rect (this.Bounds.ShrinkT(175.0f).ShrinkT(160.0f).SliceT(5.0f)) Colors.white

        Render.rect (bottom_info.Bounds.ShrinkT 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Render.rect (bottom_info.Bounds.SliceT 5.0f) Colors.white.O2

        base.Draw()