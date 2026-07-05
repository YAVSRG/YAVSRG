namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI
open Interlude.Content

type OverallTab =

    static let LEFT_SIDE_SPLIT = 0.4f
    static let selected_tab = Setting.simple 0

    static member Create() : Container =

        let content_panel = SwapContainer()

        let under_construction() = EmptyState(Icons.ALERT_TRIANGLE, "Under construction")

        let tab_options : (Widget * string) array =
            [|
                under_construction(), %"stats.overall.overview"
                under_construction(), %"stats.overall.timeline"
                under_construction(), %"stats.overall.breakdown"
            |]

        Container(NodeType.Leaf)
            .With(
                OverallHeader()
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(150.0f).SliceT(250.0f).ShrinkX(40.0f)),

                OverallTime(
                    (fun () -> Content.Stats.STATE.TotalStats.GameTime + Content.Stats.STATE.CurrentSession.GameTime),
                    (fun () -> Content.Stats.STATE.TotalStats.PlayTime + Content.Stats.STATE.CurrentSession.PlayTime),
                    (fun () -> Content.Stats.STATE.TotalStats.PracticeTime + Content.Stats.STATE.CurrentSession.PracticeTime)
                )
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f)),

                PlayCount(
                    (fun () -> Content.Stats.STATE.TotalStats.PlaysStarted + Content.Stats.STATE.CurrentSession.PlaysStarted),
                    (fun () -> Content.Stats.STATE.TotalStats.PlaysCompleted + Content.Stats.STATE.CurrentSession.PlaysCompleted),
                    (fun () -> Content.Stats.STATE.TotalStats.PlaysRetried + Content.Stats.STATE.CurrentSession.PlaysRetried),
                    (fun () -> Content.Stats.STATE.TotalStats.PlaysQuit + Content.Stats.STATE.CurrentSession.PlaysQuit)
                 )
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f)),

                TabButtons.CreatePersistent(tab_options, content_panel, selected_tab)
                    .Position(Position.ShrinkPercentL(LEFT_SIDE_SPLIT).ShrinkT(50.0f).SliceT(TabButtons.HEIGHT).ShrinkX(40.0f)),

                content_panel
                    .Position(Position.ShrinkPercentL(LEFT_SIDE_SPLIT).ShrinkB(80.0f).ShrinkT(150.0f).ShrinkX(40.0f))
            )