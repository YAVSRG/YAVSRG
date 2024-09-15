namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Charts.Processing
open Interlude
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Rulesets
open Interlude.Features.Gameplay
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Online

type ScoreChartContextMenu(cc: ChartMeta) =
    inherit Page()

    override this.Content() =
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).SliceL(PRETTYWIDTH))

            |+ PageButton(
                %"chart.add_to_collection",
                (fun () -> AddToCollectionPage(cc).Show()),
                Icon = Icons.FOLDER_PLUS
            )
            |+ PageButton(
                %"chart.delete",
                fun () -> 
                    let chart_name = sprintf "%s [%s]" cc.Title cc.DifficultyName
                    ConfirmPage(
                        [ chart_name ] %> "misc.confirmdelete",
                        fun () -> ChartDatabase.delete cc Content.Charts
                    )
                        .Show()
                , Icon = Icons.TRASH
            )

        match Content.Table with
        | Some table ->
            if Network.status = Network.Status.LoggedIn && cc.Keys = table.Info.Keymode then
                content
                |* PageButton(
                    %"chart.suggest_for_table",
                    (fun () -> SuggestChartPage(table, cc.Hash).Show()),
                    Icon = Icons.SIDEBAR
                )
        | _ -> ()

        content

    override this.Title = cc.Title
    override this.OnClose() = ()

type RulesetSwitcher(setting: Setting<string>) =
    inherit Container(NodeType.None)

    let dropdown_wrapper = DropdownWrapper(fun d -> Position.BorderT(min d.Height 500.0f).Shrink(Style.PADDING, 0.0f).Translate(0.0f, -10.0f))

    override this.Init(parent: Widget) =
        this
        |+ InlaidButton(
            (fun () -> Rulesets.current.Name),
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
        |* (
            GridFlowContainer<Widget>(50.0f, 4, Spacing = (30.0f, 0.0f), Position = { Position.SliceB(65.0f) with Left = 0.35f %+ 30.0f; Right = 1.0f %- 20.0f }.Translate(0.0f, 5.0f))
            |+ InlaidButton(
                %"score.graph.settings",
                (fun () -> ScoreGraphSettingsPage(graph).Show()
                ),
                Icons.EDIT_2
            )
            |+ InlaidButton(
                %"score.chart_actions",
                (fun () -> ScoreChartContextMenu(score_info.ChartMeta).Show()),
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
                options.SelectedRuleset
                |> Setting.trigger (fun _ ->
                    score_info.Ruleset <- Rulesets.current
                    refresh ()
                )
            )
        )

        base.Init parent