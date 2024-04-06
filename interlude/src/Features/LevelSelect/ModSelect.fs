namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Gameplay.Mods
open Prelude
open Interlude.Features
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type private ModSelector(id, current_state: unit -> int option, action: unit -> unit) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                action ()
            )
        )

    let TOP_HEIGHT = 70.0f

    override this.Init(parent) =
        this |* Clickable.Focus this
        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let state = current_state ()
        Draw.rect (this.Bounds.SliceTop(TOP_HEIGHT)) (if state.IsSome then Colors.pink.O3 else Colors.shadow_2.O2)

        Draw.rect
            (this.Bounds.TrimTop(TOP_HEIGHT))
            (if state.IsSome then
                 Colors.pink_shadow.O3
             else
                 Colors.shadow_1.O3)

        if state.IsSome then
            Text.fill_b (
                Style.font,
                Icons.CHECK,
                this.Bounds.SliceTop(TOP_HEIGHT).Shrink(20.0f, 0.0f),
                Colors.text,
                Alignment.RIGHT
            )

        Text.fill_b (
            Style.font,
            Mods.name id state,
            this.Bounds.SliceTop(TOP_HEIGHT).Shrink(20.0f, 0.0f),
            (if this.Focused then Colors.text_yellow_2 else Colors.text), 
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            Mods.desc id state,
            this.Bounds.TrimTop(TOP_HEIGHT - 2.0f).Shrink(20.0f, 0.0f),
            (if this.Focused then Colors.text_yellow_2 else Colors.text_subheading), 
            Alignment.LEFT
        )

        base.Draw()

type private ModSelectPage(change_rate: float32 -> unit, on_close: unit -> unit) as this =
    inherit Page()

    do
        let mod_grid =
            GridFlowContainer<Widget>(
                100.0f,
                3,
                Spacing = (30f, 30f),
                Position = pretty_pos (5, PAGE_BOTTOM - 5, PageWidth.Full),
                WrapNavigation = false
            )
            |+ ModSelector(
                "auto",
                (fun _ -> if autoplay then Some 0 else None),
                (fun _ -> autoplay <- not autoplay)
            )
            |+ seq {
                for id in Mods.MENU_DISPLAY_ORDER do
                    yield ModSelector(
                        id,
                        (fun _ ->
                            if selected_mods.Value.ContainsKey id then
                                Some selected_mods.Value.[id]
                            else
                                None
                        ),
                        (fun _ -> Setting.app (Mods.cycle id) selected_mods)
                    )
            }
            |+ ModSelector(
                "pacemaker",
                (fun _ -> if options.EnablePacemaker.Value then Some 0 else None),
                (fun _ -> Setting.app not options.EnablePacemaker)
            )

        page_container()
        |+ PageSetting("gameplay.rate",
            Slider(rate |> Setting.map id (fun v -> round (v / 0.05f) * 0.05f), 
            Format = sprintf "%.02fx")
        )
            .Pos(0)
            .Tooltip(Tooltip.Info("gameplay.rate"))
        |+ Text([(%%"uprate").ToString(); (%%"downrate").ToString()] %> "gameplay.rate.hotkey_hint_i", 
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = pretty_pos(2, 1, PageWidth.Full).TrimLeft(PRETTYTEXTWIDTH))
        |+ Text(%"gameplay.rate.hotkey_hint_ii", 
            Color = K Colors.text_subheading,
            Align = Alignment.LEFT,
            Position = pretty_pos(3, 1, PageWidth.Full).TrimLeft(PRETTYTEXTWIDTH))

        |+ mod_grid

        |+ PageButton("gameplay.pacemaker", (fun () -> PacemakerOptionsPage().Show()))
            .Pos(17)
            .Tooltip(Tooltip.Info("gameplay.pacemaker"))
        |+ PageSetting("gameplay.endless_mode", Selector<_>.FromBool(endless_mode))
            .Pos(19)
            .Tooltip(Tooltip.Info("gameplay.endless_mode"))

        |> this.Content

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%"autoplay").Tapped() then
            autoplay <- not autoplay
        else
            change_rate_hotkeys change_rate

    override this.Title = %"mods.name"
    override this.OnClose() = on_close ()

type ModSelect(change_rate: float32 -> unit, on_menu_close: unit -> unit) =
    inherit
        StylishButton(
            (fun () -> ModSelectPage(change_rate, on_menu_close).Show()),
            K(sprintf "%s %s" Icons.ZAP (%"levelselect.mods.name")),
            (fun () -> Palette.color (100, 0.5f, 0.0f)),
            Hotkey = "mods"
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%"autoplay").Tapped() then
            autoplay <- not autoplay
        else
            change_rate_hotkeys change_rate
