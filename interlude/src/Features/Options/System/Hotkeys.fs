namespace Interlude.Features.OptionsMenu.System

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.UI.Menu

type private Keybinder(hotkey: Hotkey) as this =
    inherit Container(NodeType.FocusTrap)

    let set = fun v -> Hotkeys.set hotkey v

    let rec input_callback (b) =
        match b with
        | Key(k, (ctrl, _, shift)) ->
            set <| Key(k, (ctrl, false, shift))
            this.Focus false
            Style.key.Play()
        | _ -> Input.listen_to_next_key input_callback

    do
        this
        |+ Text(
            (fun () -> (%%hotkey).ToString()),
            Color =
                (fun () ->
                    (if this.Selected then Colors.pink_accent
                        elif this.Focused then Colors.yellow_accent
                        else Colors.white),
                    Colors.shadow_1
                ),
            Align = Alignment.LEFT,
            Position = Position.TrimLeft 20.0f
        )
        |* Clickable.Focus(
            this,
            OnHover =
                fun b ->
                    if b && not this.Focused then
                        this.Focus true
                    elif not b && this.FocusedByMouse && not this.Selected then
                        Selection.up true
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.OnSelected(by_mouse: bool) =
        base.OnSelected by_mouse
        Style.click.Play()
        Input.listen_to_next_key input_callback

    override this.OnDeselected(by_mouse: bool) =
        base.OnDeselected by_mouse
        Input.remove_listener ()

type HotkeysPage() =
    inherit Page()

    override this.Content() = 
        let hotkey_editor hk =
            NavigationContainer.Row<Widget>()
            |+ Keybinder(hk, Position = Position.TrimRight PRETTYHEIGHT)
            |+ Button(Icons.REFRESH_CCW, (fun () -> Hotkeys.reset hk), Position = Position.SliceRight PRETTYHEIGHT)

        let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

        let scroll_container =
            ScrollContainer(container, Position = Position.Margin(100.0f, 200.0f))

        container.Add(
            PageButton(
                "system.hotkeys.reset",
                (fun () -> ConfirmPage(%"system.hotkeys.reset.confirm", Hotkeys.reset_all).Show()),
                Icon = Icons.REFRESH_CCW
            )
        )

        for hk in Hotkeys.hotkeys.Keys do
            if hk <> "none" then
                container.Add(
                    PageSetting(sprintf "hotkeys.%s" hk, hotkey_editor hk)
                        .Tooltip(Tooltip.Info(sprintf "hotkeys.%s" hk))
                )

        scroll_container

    override this.Title = %"system.hotkeys"
    override this.OnClose() = ()