namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay

type Leaderboard(display: Setting<Display>) =
    inherit Container(NodeType.None)

    let mutable count = 0
    let mutable load_if_visible = true

    let filter = Setting.simple Filter.None
    let sort = Setting.map enum int options.ScoreSortMode

    let container = FlowContainer.Vertical<LeaderboardCard>(75.0f, Spacing = Style.PADDING * 3.0f)

    let scroll_container =
        ScrollContainer(container, Position = Position.ShrinkT(50.0f), Margin = Style.PADDING)

    do
        OnlineScores.leaderboard_score_loaded.Add (fun score_info -> score_info |> LeaderboardCard |> container.Add; count <- count + 1)

    override this.Init(parent) =
        SelectedChart.on_chart_change_started.Add (fun _ -> container.Iter(fun s -> s.FadeOut()))
        SelectedChart.on_chart_change_finished.Add (fun _ -> container.Clear(); count <- 0; load_if_visible <- true)

        this
        |+ StylishButton(
            (fun () -> display.Set Display.Patterns),
            K <| %"levelselect.info.leaderboard",
            !%Palette.MAIN_100,
            Hotkey = "scoreboard_storage",
            TiltLeft = false,
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 0.0f
                    Right = 0.33f %- 25.0f
                    Bottom = 0.0f %+ 50.0f
                }
        )
            .Help(Help.Info("levelselect.info.mode", "scoreboard_storage"))
        |+ StylishButton
            .Selector(
                Icons.CHEVRONS_UP,
                [|
                    Sort.Accuracy, %"levelselect.info.scoreboard.sort.accuracy"
                |],
                sort,
                !%Palette.DARK_100,
                Hotkey = "scoreboard_sort",
                Position =
                    {
                        Left = 0.33f %+ 0.0f
                        Top = 0.0f %+ 0.0f
                        Right = 0.66f %- 25.0f
                        Bottom = 0.0f %+ 50.0f
                    }
            )
            .Help(Help.Info("levelselect.info.scoreboard.sort", "scoreboard_sort"))
        |+ StylishButton
            .Selector(
                Icons.FILTER,
                [|
                    Filter.None, %"levelselect.info.scoreboard.filter.none"
                |],
                filter,
                !%Palette.MAIN_100,
                Hotkey = "scoreboard_filter",
                TiltRight = false,
                Position =
                    {
                        Left = 0.66f %+ 0.0f
                        Top = 0.0f %+ 0.0f
                        Right = 1.0f %- 0.0f
                        Bottom = 0.0f %+ 50.0f
                    }
            )
            .Help(Help.Info("levelselect.info.scoreboard.filter", "scoreboard_filter"))
        |+ scroll_container
        |+ HotkeyAction(
            "scoreboard",
            fun () ->
                if container.Focused then
                    Selection.clear ()
                else
                    container.Focus false
        )
        |+ EmptyState(
            Icons.FLAG,
            %"levelselect.info.leaderboard.empty",
            Subtitle = %"levelselect.info.leaderboard.empty.subtitle"
        )
            .Conditional(fun () -> OnlineScores.state = OnlineScores.State.Loaded && count = 0)
        |+ EmptyState(Icons.CLOUD_OFF, %"levelselect.info.leaderboard.unavailable")
            .Conditional(fun () -> OnlineScores.state = OnlineScores.State.NoLeaderboard)
        |* EmptyState(Icons.GLOBE, %"misc.offline")
            .Conditional(fun () -> OnlineScores.state = OnlineScores.State.Offline)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        if load_if_visible then
            SelectedChart.if_loaded OnlineScores.load
            load_if_visible <- false
        base.Update(elapsed_ms, moved)
        OnlineScores.score_loader.Join()