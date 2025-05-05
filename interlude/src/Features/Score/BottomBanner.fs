namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.User
open Prelude.Skins.Noteskins
open Interlude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Rulesets
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Online

type RulesetSwitcher(setting: Setting<string>, set_ruleset_direct: Ruleset -> unit, score_info: ScoreInfo) =
    inherit Container(NodeType.None)

    let mutable dropdown_height = 500.0f

    let dropdown_wrapper = DropdownWrapper(fun d -> dropdown_height <- min d.Height 500.0f; Position.BorderT(dropdown_height).ShrinkX(Style.PADDING).TranslateY(-10.0f))

    let switch_to_native_ruleset () =
        match Rulesets.get_native_ruleset score_info.ChartMeta.Origins with
        | Some ruleset ->
            // Use user's version (custom colors, names, etc) if available
            Rulesets.by_hash (Ruleset.hash ruleset)
            |> Option.defaultValue ruleset
            |> set_ruleset_direct
            Style.click.Play()
        | None ->
            Notifications.system_feedback(Icons.HELP_CIRCLE, %"notification.no_native_ruleset.title", %"notification.no_native_ruleset.body")

    override this.Init(parent: Widget) =
        this
            .Add(
                InlaidButton(
                    (fun () -> score_info.Ruleset.Name),
                    (fun () -> this.ToggleDropdown())
                )
                    .Hotkey("ruleset_switch")
                    .HoverText(%"score.switch_ruleset"),

                HotkeyListener("native_ruleset", switch_to_native_ruleset),

                dropdown_wrapper
            )

        base.Init parent

    override this.Draw() =
        base.Draw()
        if dropdown_wrapper.Active && not score_info.ChartMeta.Origins.IsEmpty then
            Text.draw_aligned_b(Style.font, sprintf "%O: Use original ruleset" (%%"native_ruleset"), 20.0f, this.Bounds.Right - 10.0f, this.Bounds.Top - dropdown_height - 50.0f, Colors.text_cyan_2, Alignment.RIGHT)

    member this.ToggleDropdown() =
        RulesetSwitcher.make_dropdown setting dropdown_wrapper

type BottomBanner(score_info: ScoreInfo, played_just_now: bool, graph: ScoreGraph, refresh: unit -> unit) =
    inherit Container(NodeType.None)

    override this.Init(parent) =

        if Network.lobby.IsNone && played_just_now then
            this
            |+ AngledButton(
                sprintf "%s %s" Icons.PLAY %"score.continue",
                Gameplay.continue_endless_mode >> ignore,
                Palette.MAIN.O2
            )
                .LeanRight(false)
                .Floating()
                .Position(Position.BorderT(AngledButton.HEIGHT).SliceR(300.0f))
            |+ AngledButton(
                sprintf "%s %s" Icons.REPEAT %"score.retry",
                Gameplay.retry,
                Palette.DARK.O2
            )
                .Floating()
                .Position(Position.BorderT(AngledButton.HEIGHT).SliceR(300.0f).Translate(-325.0f, 0.0f))
            |+ HotkeyListener("retry", Gameplay.retry)
            |* HotkeyListener("select", Gameplay.continue_endless_mode >> ignore)

        this
        |+ graph
        |+ Text(Updates.version + "  : :  www.yavsrg.net")
            .Color(Colors.text_subheading)
            .Align(Alignment.CENTER)
            .Position(
                Position
                    .SlicePercentL(0.35f)
                    .ShrinkX(20.0f)
                    .SliceB(15.0f, 40.0f)
            )
        |+ NavigationContainer.Row()
            .Position(Position.ShrinkPercentL(0.35f).ShrinkX(20.0f).ShrinkL(10.0f).SliceB(10.0f, 50.0f))
            .With(
                InlaidButton(%"score.graph.settings", fun () ->
                    ScoreGraphSettingsPage(score_info.WithMods.Keys, graph.ApplyColumnFilter).Show()
                )
                    .Icon(Icons.EDIT_2)
                    .Position(Position.GridX(1, 4, 30.0f)),

                InlaidButton(%"score.chart_actions", fun () ->
                    ScoreChartContextMenu(score_info).Show()
                )
                    .Icon(Icons.SETTINGS)
                    .Hotkey("context_menu")
                    .Position(Position.GridX(2, 4, 30.0f)),

                InlaidButton(%"score.watch_replay", fun () ->
                    Gameplay.watch_replay (score_info, NoteColors.apply Content.NoteskinConfig.NoteColors score_info.WithMods)
                )
                    .Icon(Icons.FILM)
                    .Position(Position.GridX(3, 4, 30.0f)),

                RulesetSwitcher(
                    Setting.simple Rulesets.selected_id.Value
                    |> Setting.trigger (fun id ->
                        score_info.Ruleset <- Rulesets.by_id id
                        refresh ()
                    ),
                    (fun ruleset -> score_info.Ruleset <- ruleset; refresh()),
                    score_info
                )
                    .Position(Position.GridX(4, 4, 30.0f))
            )
        |* HotkeyListener("like", fun () -> CollectionActions.toggle_liked score_info.ChartMeta)

        base.Init parent