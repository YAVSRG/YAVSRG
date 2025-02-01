namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type LeaderboardsTab() =
    inherit Container(NodeType.None)

    let sevenkey = Dummy()
    let fourkey = Dummy()
    let activity = ActivityLeaderboard()
    let content_panel = SwapContainer(activity, Position = Position.SlicePercentR(0.6f).ShrinkB(80.0f).ShrinkT(150.0f).ShrinkX(40.0f))

    let tabs =
        RadioButtons.create_tabs {
            Setting = Setting.make content_panel.set_Current content_panel.get_Current
            Options = [|
                activity, %"stats.leaderboards.activity", K false
                fourkey, "4K", K false
                sevenkey, "7K", K false
            |]
            Height = 50.0f
        }

    override this.Init(parent) =

        tabs.Position <- Position.SlicePercentR(0.6f).ShrinkT(50.0f).SliceT(50.0f).ShrinkX(40.0f)
        this
        |+ tabs
        |+ PageSetting(
            "Sort by playtime",
            Checkbox(activity.SortByPlaytime),
            Position = Position.SlicePercentL(0.4f).ShrinkT(150.0f).SliceT(50.0f).ShrinkX(40.0f))
        |+ PageSetting(
            "Monthly",
            Checkbox(activity.MonthlyLeaderboard),
            Position = Position.SlicePercentL(0.4f).ShrinkT(200.0f).SliceT(50.0f).ShrinkX(40.0f))
        |* content_panel

        base.Init parent