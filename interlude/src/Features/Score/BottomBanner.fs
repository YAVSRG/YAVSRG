namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data
open Prelude.Data.Library.Caching
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Features
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Online

type ScoreChartContextMenu(cc: CachedChart) as this =
    inherit Page()

    do
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))

            |+ PageButton(
                "chart.add_to_collection",
                (fun () -> AddToCollectionPage(cc).Show()),
                Icon = Icons.FOLDER_PLUS
            )

        match Content.Table with
        | Some table ->
            if Network.status = Network.Status.LoggedIn && cc.Keys = table.Info.Keymode then
                content
                |* PageButton(
                    "chart.suggest_for_table",
                    (fun () -> SuggestChartPage(table, cc).Show()),
                    Icon = Icons.SIDEBAR
                )
        | _ -> ()

        this.Content content

    override this.Title = cc.Title
    override this.OnClose() = ()

type RulesetSwitcher(setting: Setting<string>) =
    inherit Container(NodeType.None)

    override this.Init(parent: Widget) =
        this
        |* InlaidButton(
            (fun () -> Rulesets.current.Name),
            (fun () -> this.ToggleDropdown()),
            "",
            Hotkey = "ruleset_switch",
            HoverText = "Switch ruleset"
        )

        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let rulesets = Rulesets.list ()

            let d =
                Dropdown
                    {
                        Items = rulesets |> Seq.map (fun (id, rs) -> id, rs.Name)
                        ColorFunc = K Colors.text
                        OnClose = fun () -> this.Dropdown <- None
                        Setting = setting
                    }

            d.Position <-
                Position
                    .BorderTop(min d.Height 500.0f)
                    .Margin(Style.PADDING, 0.0f)
                    .Translate(0.0f, -10.0f)

            d.Init this
            this.Dropdown <- Some d

    member val Dropdown: Dropdown<string> option = None with get, set

    override this.Draw() =
        base.Draw()

        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        match this.Dropdown with
        | Some d -> d.Update(elapsed_ms, moved)
        | None -> ()

type BottomBanner(stats: ScoreScreenStats ref, score_info: ScoreInfo, graph: ScoreGraph, refresh: unit -> unit) as this
    =
    inherit Container(NodeType.None)

    do
        graph.Position <-
            {
                Left = 0.35f %+ 30.0f
                Top = 0.0f %+ 25.0f
                Right = 1.0f %- 20.0f
                Bottom = 1.0f %- 65.0f
            }

        this
        |+ graph
        |+ Text(
            version + "  : :  www.yavsrg.net",
            Position = { Position.SliceBottom(50.0f) with Right = 0.35f %+ 0.0f }.Margin(20.0f, 5.0f),
            Color = K Colors.text_subheading,
            Align = Alignment.CENTER
        )
        |* (
            GridFlowContainer<Widget>(65.0f, 4, Spacing = (30.0f, 0.0f), Position = { Position.SliceBottom(65.0f) with Left = 0.35f %+ 30.0f; Right = 1.0f %- 20.0f }.Translate(0.0f, 5.0f))
            |+ InlaidButton(
                %"score.graph.settings",
                (fun () ->
                    { new ScoreGraphSettingsPage() with
                        override this.OnClose() = graph.Refresh()
                    }
                        .Show()
                ),
                Icons.EDIT_2
            )
            |+ InlaidButton(
                %"score.chart_actions",
                (fun () -> ScoreChartContextMenu(score_info.CachedChart).Show()),
                Icons.SETTINGS,
                Hotkey = "context_menu"
            )
            |+ InlaidButton(
                %"score.watch_replay.name",
                (fun () ->
                    ScoreScreenHelpers.watch_replay (score_info, Gameplay.Chart.color_this_chart (score_info.WithMods))
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

    override this.Draw() =

        Draw.rect (this.Bounds.TrimTop 5.0f) (Palette.color (127, 0.5f, 0.0f))
        Draw.rect (this.Bounds.SliceTop 5.0f) Colors.white.O2

        // graph background
        Draw.rect (graph.Bounds.Expand(5.0f, 5.0f)) Color.White
        Background.draw (graph.Bounds, Color.FromArgb(127, 127, 127), 1.0f)

        base.Draw()