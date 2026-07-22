namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
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
                    (fun () -> Content.Stats.GameTime),
                    (fun () -> Content.Stats.PlayTime),
                    (fun () -> Content.Stats.PracticeTime)
                )
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(450.0f).SliceT(250.0f).ShrinkX(40.0f)),

                PlayCount(
                    (fun () -> Content.Stats.PlaysStarted),
                    (fun () -> Content.Stats.PlaysCompleted),
                    (fun () -> Content.Stats.PlaysRetried),
                    (fun () -> Content.Stats.PlaysQuit)
                 )
                    .Position(Position.SlicePercentL(LEFT_SIDE_SPLIT).ShrinkT(750.0f).SliceT(250.0f).ShrinkX(40.0f)),

                TabButtons.CreatePersistent(tab_options, content_panel, selected_tab)
                    .Position(Position.ShrinkPercentL(LEFT_SIDE_SPLIT).ShrinkT(50.0f).SliceT(TabButtons.HEIGHT).ShrinkX(40.0f)),

                content_panel
                    .Position(Position.ShrinkPercentL(LEFT_SIDE_SPLIT).ShrinkB(80.0f).ShrinkT(150.0f).ShrinkX(40.0f))
            )