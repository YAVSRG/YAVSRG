﻿namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Gameplay.Mods
open Interlude.Features
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Utils
open Interlude.Features.Gameplay

type private ModSelector(id, states: string[], current_state: unit -> int, action: unit -> unit) =
    inherit
        StaticContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                action ()
            )
        )

    let TOP_HEIGHT = 70.0f

    override this.Init(parent) =
        this
        |+ Clickable.Focus this
        |+ Text(
            ModState.get_mod_name id,
            Color = (fun () -> (if this.Focused then Colors.yellow_accent else Colors.white), Colors.shadow_1),
            Position = Position.SliceTop(TOP_HEIGHT).Margin(20.0f, 0.0f),
            Align = Alignment.LEFT
        )
        |* Text(
            ModState.get_mod_desc id,
            Color = (fun () -> (if this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2),
            Position = Position.TrimTop(TOP_HEIGHT - 2f).Margin(20.0f, 0.0f),
            Align = Alignment.LEFT
        )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let state = current_state ()
        Draw.rect (this.Bounds.SliceTop(TOP_HEIGHT)) (if state >= 0 then Colors.pink.O3 else Colors.shadow_2.O2)

        Draw.rect
            (this.Bounds.TrimTop(TOP_HEIGHT))
            (if state >= 0 then
                 Colors.pink_shadow.O3
             else
                 Colors.shadow_1.O3)

        if state >= 0 then
            Text.fill_b (
                Style.font,
                states.[state],
                this.Bounds.SliceTop(TOP_HEIGHT).Shrink(20.0f, 0.0f),
                Colors.text,
                Alignment.RIGHT
            )

        base.Draw()

type private ModSelectPage(on_close) as this =
    inherit Page()

    let grid =
        GridFlowContainer<Widget>(
            100.0f,
            3,
            Spacing = (30f, 30f),
            Position = Position.Margin(100.0f, 200.0f),
            WrapNavigation = false
        )

    do
        grid
        |* ModSelector(
            "auto",
            [| Icons.CHECK |],
            (fun _ -> if autoplay then 0 else -1),
            (fun _ -> autoplay <- not autoplay)
        )

        for id in available_mods.Keys do
            grid
            |* ModSelector(
                id,
                [| Icons.CHECK |],
                (fun _ ->
                    if selected_mods.Value.ContainsKey id then
                        selected_mods.Value.[id]
                    else
                        -1
                ),
                (fun _ -> Setting.app (ModState.cycle id) selected_mods)
            )

        grid
        |* ModSelector(
            "pacemaker",
            [| Icons.CHECK |],
            (fun _ -> if options.EnablePacemaker.Value then 0 else -1),
            (fun _ -> Setting.app not options.EnablePacemaker)
        )

    do
        this.Content(
            NavigationContainer.Column<Widget>()
            |+ grid
            |+ PageButton("gameplay.pacemaker", (fun () -> PacemakerOptionsPage().Show()))
                .Pos(500.0f)
                .Tooltip(Tooltip.Info("gameplay.pacemaker"))
            |+ Conditional(
                options.AdvancedRecommendations.Get,
                PageSetting("gameplay.endless_mode", Selector<_>.FromBool(endless_mode))
                    .Pos(570.0f)
                    .Tooltip(Tooltip.Info("gameplay.endless_mode"))
            )
        )

    override this.Title = %"mods.name"
    override this.OnClose() = on_close ()

type ModSelect(on_close) =
    inherit
        StylishButton(
            (fun () -> ModSelectPage(on_close).Show()),
            K(sprintf "%s %s" Icons.ZAP (%"levelselect.mods.name")),
            (fun () -> Palette.color (100, 0.5f, 0.0f)),
            Hotkey = "mods"
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%"autoplay").Tapped() then
            autoplay <- not autoplay
