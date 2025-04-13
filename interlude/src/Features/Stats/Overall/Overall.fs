namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI

type OverallTab =

    static let LEFT_SIDE_SPLIT = 0.4f
    static let selected_tab = Setting.simple 0

    static member Create() : Container =

        let content_panel = SwapContainer()

        let skill_breakdown = SkillBreakdown()
        let timeline = SkillTimeline()
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

        let tab_options : (Widget * string) array =
            [|
                overview, %"stats.overall.overview"
                timeline, %"stats.overall.timeline"
                skill_breakdown, %"stats.overall.breakdown"
            |]

        Container(NodeType.Leaf)
            .With(
                OverallHeader()
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(150.0f).SliceT(250.0f).ShrinkX(40.0f)),

                OverallTime(
                    (fun () -> TOTAL_STATS.GameTime + CURRENT_SESSION.GameTime),
                    (fun () -> TOTAL_STATS.PlayTime + CURRENT_SESSION.PlayTime),
                    (fun () -> TOTAL_STATS.PracticeTime + CURRENT_SESSION.PracticeTime)
                )
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f)),

                PlayCount(
                    (fun () -> TOTAL_STATS.PlaysStarted + CURRENT_SESSION.PlaysStarted),
                    (fun () -> TOTAL_STATS.PlaysCompleted + CURRENT_SESSION.PlaysCompleted),
                    (fun () -> TOTAL_STATS.PlaysRetried + CURRENT_SESSION.PlaysRetried),
                    (fun () -> TOTAL_STATS.PlaysQuit + CURRENT_SESSION.PlaysQuit)
                 )
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f)),

                TabButtons.CreatePersistent(tab_options, content_panel, selected_tab)
                    .Position(Position.ShrinkPercentL(LEFT_SIDE_SPLIT).ShrinkT(50.0f).SliceT(TabButtons.HEIGHT).ShrinkX(40.0f)),

                content_panel
                    .Position(Position.ShrinkPercentL(LEFT_SIDE_SPLIT).ShrinkB(80.0f).ShrinkT(150.0f).ShrinkX(40.0f))
            )