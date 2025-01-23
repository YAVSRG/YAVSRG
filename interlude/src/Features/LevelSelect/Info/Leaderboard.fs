namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing.Difficulty
open Prelude.Gameplay
open Prelude.Gameplay.Mods
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Prelude.Data.Library
open Interlude
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Score
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

module Leaderboard =

    [<RequireQualifiedAccess>]
    type State =
        | Offline = -1
        | Loading = 0
        | NoLeaderboard = 1
        | EmptyLeaderboard = 2
        | Loaded = 3

    [<RequireQualifiedAccess>]
    type Sort =
        | Accuracy = 2

    [<RequireQualifiedAccess>]
    type Filter =
        | None = 0

    type LeaderboardScore = Charts.Scores.Leaderboard.Score

    type LeaderboardCard(score: LeaderboardScore, score_info: ScoreInfo) =
        inherit
            FrameContainer(
                NodeType.Button(
                    (fun () ->
                        Screen.change_new
                            (fun () -> new ScoreScreen(score_info, (ImprovementFlags.None, None), false) :> Screen)
                            Screen.Type.Score
                            Transitions.EnterGameplayNoFadeAudio
                        |> ignore
                    )
                )
            )

        let fade = Animation.Fade(0.0f, Target = 1.0f)
        let animation = Animation.seq [ Animation.Delay 150; fade ]

        override this.Init(parent) =
            this.Fill <-
                fun () ->
                    if this.Focused then
                        Colors.yellow_accent.O1a fade.Alpha
                    else
                        (!*Palette.DARK).O2a fade.Alpha

            this.Border <-
                fun () ->
                    if this.Focused then
                        Colors.yellow_accent.O4a fade.Alpha
                    else
                        (!*Palette.LIGHT).O2a fade.Alpha

            let text_color =
                fun () -> let a = fade.Alpha in (Colors.white.O4a a, Colors.shadow_1.O4a a)

            let text_subcolor =
                fun () -> let a = fade.Alpha in (Colors.grey_1.O4a a, Colors.shadow_2.O4a a)

            this
            |+ Text(
                K(sprintf "#%i %s  •  %s" score.Rank score.Username score_info.Scoring.FormattedAccuracy),
                Color = text_color,
                Align = Alignment.LEFT,
                Position =
                    {
                        Left = 0.0f %+ 5.0f
                        Top = 0.0f %+ 0.0f
                        Right = 0.8f %+ 0.0f
                        Bottom = 0.6f %+ 0.0f
                    }
            )

            |+ Text(
                K(
                    sprintf
                        "%s  •  %ix  •  %.2f"
                        (score_info.Ruleset.LampName score_info.Lamp)
                        score_info.Scoring.BestCombo
                        score_info.Physical
                ),
                Color = text_subcolor,
                Align = Alignment.LEFT,
                Position =
                    {
                        Left = 0.0f %+ 5.0f
                        Top = 0.6f %- 5.0f
                        Right = 0.5f %+ 0.0f
                        Bottom = 1.0f %- 2.0f
                    }
            )

            |+ Text(
                K(format_timespan (Timestamp.since score_info.TimePlayed)),
                Color = text_subcolor,
                Align = Alignment.RIGHT,
                Position =
                    {
                        Left = 0.5f %+ 0.0f
                        Top = 0.6f %- 5.0f
                        Right = 1.0f %- 5.0f
                        Bottom = 1.0f %- 2.0f
                    }
            )

            |+ Text(
                score_info.ModString(),
                Color = text_color,
                Align = Alignment.RIGHT,
                Position =
                    {
                        Left = 0.5f %+ 0.0f
                        Top = 0.0f %+ 0.0f
                        Right = 1.0f %- 5.0f
                        Bottom = 0.6f %+ 0.0f
                    }
            )

            |* Clickable.Focus(this, OnRightClick = (fun () -> ScoreContextMenu(score_info).Show()))

            base.Init parent

        member this.Data = score_info

        member this.FadeOut() = fade.Target <- 0.0f

        override this.OnFocus(by_mouse: bool) =
            base.OnFocus by_mouse
            Style.hover.Play()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)
            animation.Update elapsed_ms

            if Mouse.hover this.Bounds && (%%"delete").Tapped() then
                ScoreContextMenu.ConfirmDeleteScore(score_info, false)
            elif this.Focused && (%%"context_menu").Tapped() then
                ScoreContextMenu(score_info).Show()

    // todo: refactor to similar system as local scores
    module Loader =

        let state = Setting.simple State.NoLeaderboard

        type Request =
            {
                Ruleset: Ruleset
                ChartMeta: ChartMeta
                Chart: Chart
            }
            override this.ToString() = "<leaderboard calculation>"

        let container = FlowContainer.Vertical(75.0f, Spacing = Style.PADDING * 3.0f)

        let score_loader =
            { new Async.SwitchServiceSeq<Request, unit -> unit>() with
                member this.Process(req: Request) =
                    seq {
                        let mutable scores : LeaderboardScore array = [||]
                        let mutable target_state = State.Loading
                        Charts.Scores.Leaderboard.get_async (
                            req.ChartMeta.Hash,
                            function
                            | Some reply ->
                                if reply.Scores.Length > 0 then
                                    target_state <- State.Loaded
                                    scores <- reply.Scores
                                else
                                    target_state <- State.EmptyLeaderboard
                            | None ->
                                target_state <- State.NoLeaderboard
                        )
                        |> Async.RunSynchronously

                        yield fun () -> state.Set target_state

                        for score in scores do
                            let with_mods = Mods.apply score.Mods req.Chart
                            let replay_data = Replay.decompress_string score.Replay

                            let scoring =
                                ScoreProcessor.run
                                    req.Ruleset
                                    with_mods.Keys
                                    (StoredReplayProvider replay_data)
                                    with_mods.Notes
                                    score.Rate

                            let rating = DifficultyRating.calculate score.Rate with_mods.Notes

                            let score_info: ScoreInfo =
                                {
                                    ChartMeta = req.ChartMeta
                                    Chart = req.Chart
                                    WithMods = with_mods

                                    PlayedBy = ScorePlayedBy.Username score.Username
                                    TimePlayed = score.Timestamp
                                    Rate = score.Rate

                                    Replay = replay_data
                                    Scoring = scoring
                                    Lamp = Lamp.calculate req.Ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
                                    Grade = Grade.calculate req.Ruleset.Grades scoring.Accuracy

                                    Rating = rating
                                    Physical = Performance.calculate rating with_mods.Keys scoring |> fst

                                    ImportedFromOsu = false
                                    IsFailed = false
                                }

                            let lc = LeaderboardCard(score, score_info)
                            yield fun () -> container.Add lc
                    }

                member this.Handle(action) = action ()
            }

        let load (cc: ChartMeta) (chart: Chart) =
            if Network.status <> Network.Status.LoggedIn then
                state.Set State.Offline
            else
                state.Set State.Loading
                container.Clear()
                score_loader.Request
                    {
                        Ruleset = Content.Rulesets.current
                        ChartMeta = cc
                        Chart = chart
                    }

open Leaderboard

type Leaderboard(display: Setting<Display>) =
    inherit Container(NodeType.None)

    let mutable last_loading = ""
    let mutable last_loaded = ""
    let mutable scoring = ""

    let filter = Setting.simple Filter.None
    let sort = Setting.map enum int options.ScoreSortMode

    let scroll_container =
        ScrollContainer(Loader.container, Margin = Style.PADDING, Position = Position.ShrinkT(55.0f))

    override this.Init(parent) =
        SelectedChart.on_chart_change_started.Add(fun info ->
            if info.ChartMeta.Hash <> last_loading then
                Loader.container.Iter(fun s -> s.FadeOut())
                last_loading <- info.ChartMeta.Hash
                last_loaded <- ""
        )
        Gameplay.leaderboard_rank_changed.Add(fun score_info ->
            last_loaded <- score_info.ChartMeta.Hash
            scoring <- Content.Rulesets.current_hash
            Loader.load score_info.ChartMeta score_info.Chart
        )

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
                if Loader.container.Focused then
                    Selection.clear ()
                else
                    Loader.container.Focus false
        )
        |+ EmptyState(
            Icons.FLAG,
            %"levelselect.info.leaderboard.empty",
            Subtitle = %"levelselect.info.leaderboard.empty.subtitle"
        )
            .Conditional(fun () -> Loader.state.Value = State.EmptyLeaderboard)
        |+ EmptyState(Icons.CLOUD_OFF, %"levelselect.info.leaderboard.unavailable")
            .Conditional(fun () -> Loader.state.Value = State.NoLeaderboard)
        |* EmptyState(Icons.GLOBE, %"misc.offline")
            .Conditional(fun () -> Loader.state.Value = State.Offline)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        Loader.score_loader.Join()

    member this.OnChartUpdated(info: LoadedChartInfo) =
        if info.ChartMeta.Hash <> last_loaded || scoring <> Content.Rulesets.current_hash then
            last_loaded <- info.ChartMeta.Hash
            scoring <- Content.Rulesets.current_hash
            Loader.load info.ChartMeta info.Chart

    member this.Refresh() = SelectedChart.when_loaded false this.OnChartUpdated