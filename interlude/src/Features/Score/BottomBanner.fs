namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.UserDatabase
open Interlude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Online
open Interlude.Features.Score

type BottomBanner(score_info: ScoreInfo, played_just_now: bool, graph: ScoreGraph, refresh: unit -> unit) =
    inherit Container(NodeType.None)

    override this.Init(parent: Widget) : unit =

        if Network.lobby.IsNone && played_just_now then
            this.Add(
                AngledButton(
                    sprintf "%s %s" Icons.PLAY %"score.continue",
                    Gameplay.continue_endless_mode >> ignore,
                    Palette.MAIN.O2
                )
                    .LeanRight(false)
                    .Floating()
                    .Position(Position.BorderT(AngledButton.HEIGHT).SliceR(300.0f)),
                AngledButton(sprintf "%s %s" Icons.REPEAT %"score.retry", Gameplay.retry, Palette.DARK.O2)
                    .Floating()
                    .Position(Position.BorderT(AngledButton.HEIGHT).SliceR(300.0f).Translate(-325.0f, 0.0f)),
                HotkeyListener("retry", Gameplay.retry),
                HotkeyListener("select", Gameplay.continue_endless_mode >> ignore)
            )

        this.Add(
            graph,
            Text(Updates.version + "  : :  www.yavsrg.net")
                .Color(Colors.text_subheading)
                .Align(Alignment.CENTER)
                .Position(Position.SlicePercentL(0.35f).ShrinkX(20.0f).SliceB(15.0f, 40.0f)),
            NavigationContainer
                .Row()
                .Position(Position.ShrinkPercentL(0.35f).ShrinkX(20.0f).ShrinkL(10.0f).SliceB(10.0f, 50.0f))
                .With(
                    InlaidButton(
                        %"score.graph.settings",
                        fun () -> ScoreGraphSettingsPage(score_info.WithMods.Keys, graph.ApplyColumnFilter).Show()
                    )
                        .Icon(Icons.EDIT_2)
                        .Position(Position.GridX(1, 4, 30.0f)),

                    InlaidButton(%"score.chart_actions", fun () -> ScoreChartContextMenu(score_info).Show())
                        .Icon(Icons.SETTINGS)
                        .Hotkey("context_menu")
                        .Position(Position.GridX(2, 4, 30.0f)),

                    InlaidButton(%"score.watch_replay", fun () -> Gameplay.watch_replay(score_info))
                        .Icon(Icons.FILM)
                        .Position(Position.GridX(3, 4, 30.0f)),

                    ScoreScreenRulesetSwitcher(
                        Setting.simple rulesets.Selected
                        |> Setting.trigger(fun selection ->
                            score_info.RulesetInfo <- rulesets.GetCurrent(score_info.ChartMeta, selection)

                            refresh()
                        ),
                        (fun ruleset ->
                            score_info.RulesetInfo <- ruleset
                            refresh()
                        ),
                        score_info
                    )
                        .Position(Position.GridX(4, 4, 30.0f))
                ),
            HotkeyListener("like", fun () -> CollectionActions.toggle_liked score_info.ChartMeta)
        )

        base.Init(parent)
