namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Interlude.UI

type OverallTab() =
    inherit Container(NodeType.Leaf)

    let skill_breakdown = SkillBreakdown()
    let timeline = SkillTimeline()
    let content_panel = SwapContainer(Position = Position.SliceRPercent(0.6f).ShrinkB(80.0f).ShrinkT(150.0f).ShrinkX(40.0f))
    let overview =
        Overview(
            fun keymode source pattern ->
                match pattern with
                | None ->
                    timeline.Switch keymode
                    content_panel.Current <- timeline
                | Some p ->
                    skill_breakdown.Switch (keymode, source, p)
                    content_panel.Current <- skill_breakdown
        )

    do content_panel.Current <- overview

    let tabs =
        RadioButtons.create_tabs {
            Setting = Setting.make content_panel.set_Current content_panel.get_Current
            Options = [|
                overview, %"stats.overall.overview", K false
                timeline, %"stats.overall.timeline", K false
                skill_breakdown, %"stats.overall.breakdown", K false
            |]
            Height = 50.0f
        }

    override this.Init(parent) =

        tabs.Position <- Position.SliceRPercent(0.6f).ShrinkT(50.0f).SliceT(50.0f).ShrinkX(40.0f)
        this
        |+ tabs
        |+ OverallHeader(Position = Position.SliceLPercent(0.4f).ShrinkT(150.0f).SliceT(250.0f).ShrinkX(40.0f))
        |+ OverallTime(
            (fun () -> Stats.TOTAL_STATS.GameTime + Stats.CURRENT_SESSION.GameTime),
            (fun () -> Stats.TOTAL_STATS.PlayTime + Stats.CURRENT_SESSION.PlayTime),
            (fun () -> Stats.TOTAL_STATS.PracticeTime + Stats.CURRENT_SESSION.PracticeTime),
            Position = Position.SliceLPercent(0.4f).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f)
        )
        |+ PlayCount(
            (fun () -> Stats.TOTAL_STATS.PlaysStarted + Stats.CURRENT_SESSION.PlaysStarted),
            (fun () -> Stats.TOTAL_STATS.PlaysCompleted + Stats.CURRENT_SESSION.PlaysCompleted),
            (fun () -> Stats.TOTAL_STATS.PlaysRetried + Stats.CURRENT_SESSION.PlaysRetried),
            (fun () -> Stats.TOTAL_STATS.PlaysQuit + Stats.CURRENT_SESSION.PlaysQuit),
            Position = Position.SliceLPercent(0.4f).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f)
        )
        |* content_panel

        base.Init parent