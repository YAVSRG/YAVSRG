namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type LeaderboardsTab =

    static member Create() : Container =

        let keymodes = KeymodesLeaderboard()
        let activity = ActivityLeaderboard()

        let content_panel = SwapContainer(activity)

        let options =
            NavigationContainer.Column()
                .With(
                    PageSetting("Leaderboard",
                        Selector(
                            [| activity :> Widget, %"stats.leaderboards.activity"; keymodes, %"stats.leaderboards.rating" |],
                            Setting.make content_panel.set_Current content_panel.get_Current
                        )
                    )
                        .Pos(0, 2, PageWidth.Full)
                )
                .WithConditional(
                    (fun () -> content_panel.Current = activity),

                    PageSetting(
                        "Time",
                        Selector([| false, "All-time"; true, "This month" |], activity.MonthlyLeaderboard)
                    )
                        .Pos(3, 2, PageWidth.Full)
                )
                .WithConditional(
                    (fun () -> content_panel.Current = keymodes),

                    PageSetting(
                        "Time",
                        Selector([| false, "All-time"; true, "This month" |], keymodes.MonthlyLeaderboard)
                    )
                        .Pos(3, 2, PageWidth.Full)
                        :> Widget,

                    PageSetting(
                        "Keymode",
                        Selector([| 4, "4K"; 7, "7K" |], keymodes.Keymode)
                    )
                        .Conditional(fun () -> content_panel.Current = keymodes)
                        .Pos(5, 2, PageWidth.Full)
                )

        Container(NodeType.Leaf)
            .With(
                options.Position(Position.SlicePercentL(0.4f).ShrinkT(150.0f).ShrinkX(40.0f)),
                content_panel.Position(Position.SlicePercentR(0.6f).Shrink(40.0f, 40.0f))
            )