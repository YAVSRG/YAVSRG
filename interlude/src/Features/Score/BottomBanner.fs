namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Prelude.Charts.Processing
open Interlude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Rulesets
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Online

type RulesetSwitcher(setting: Setting<string>, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.BorderT(min d.Height 500.0f).Shrink(Style.PADDING, 0.0f).Translate(0.0f, -10.0f))

    override this.Init(parent: Widget) =
        this
        |+ InlaidButton(
            (fun () -> score_info.Ruleset.Name),
            (fun () -> this.ToggleDropdown()),
            "",
            Hotkey = "ruleset_switch",
            HoverText = "Switch ruleset"
        )
        |* dropdown_wrapper

        base.Init parent

    member this.ToggleDropdown() =
        RulesetSwitcher.make_dropdown setting dropdown_wrapper

type BottomBanner(score_info: ScoreInfo, played_just_now: bool, graph: ScoreGraph, refresh: unit -> unit) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        
        if Network.lobby.IsNone && played_just_now then
            this
            |+ StylishButton(
                Gameplay.continue_endless_mode >> ignore,
                K (sprintf "%s %s" Icons.PLAY %"score.continue"),
                !%Palette.MAIN.O2,
                TiltRight = false,
                Position = Position.BorderT(50.0f).SliceR(300.0f),
                Floating = true
            )
            |+ StylishButton(
                Gameplay.retry,
                K (sprintf "%s %s" Icons.REPEAT %"score.retry"),
                !%Palette.DARK.O2,
                Position = Position.BorderT(50.0f).SliceR(300.0f).Translate(-325.0f, 0.0f),
                Floating = true
            )
            |+ HotkeyAction("retry", Gameplay.retry)
            |* HotkeyAction("select", Gameplay.continue_endless_mode >> ignore)

        this
        |+ graph
        |+ Text(
            Updates.version + "  : :  www.yavsrg.net",
            Position = { Position.SliceB(40.0f) with Right = 0.35f %+ 0.0f }.ShrinkX(20.0f).TranslateY(-15.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.CENTER
        )
        |+ (
            GridFlowContainer<Widget>(50.0f, 4, Spacing = (30.0f, 0.0f), Position = { Position.SliceB(65.0f) with Left = 0.35f %+ 30.0f; Right = 1.0f %- 20.0f }.Translate(0.0f, 5.0f))
            |+ InlaidButton(
                %"score.graph.settings",
                (fun () -> ScoreGraphSettingsPage(graph).Show()
                ),
                Icons.EDIT_2
            )
            |+ InlaidButton(
                %"score.chart_actions",
                (fun () -> ScoreChartContextMenu(score_info).Show()),
                Icons.SETTINGS,
                Hotkey = "context_menu"
            )
            |+ InlaidButton(
                %"score.watch_replay",
                (fun () ->
                    Gameplay.watch_replay (score_info, NoteColors.apply Content.NoteskinConfig.NoteColors score_info.WithMods)
                ),
                Icons.FILM
            )
            |+ RulesetSwitcher(
                Setting.simple Rulesets.selected_id.Value
                |> Setting.trigger (fun id ->
                    score_info.Ruleset <- Rulesets.by_id id
                    refresh ()
                ),
                score_info
            )
        )
        |+ HotkeyAction("like", fun () -> CollectionActions.like_chart score_info.ChartMeta)
        |* HotkeyAction("unlike", fun () -> CollectionActions.unlike_chart score_info.ChartMeta)

        base.Init parent