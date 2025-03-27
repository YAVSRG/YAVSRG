namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude

type TextEntry(setting: Setting<string>, hotkey: Hotkey, focus_trap: bool) as this =
    inherit Container(if focus_trap then NodeType.FocusTrap else NodeType.Leaf)

    let CURSOR_BLINK_INTERVAL_MS = 600.0
    let cursor_blink = Animation.Counter(CURSOR_BLINK_INTERVAL_MS)

    let toggle () =
        if this.Selected then
            this.Focus false
        else
            this.Select false

    member val Clickable = true with get, set
    member val TextColor : unit -> Color * Color =
        fun () ->
            Colors.white,
            (if this.Selected then
                 Colors.pink_shadow
             else
                 Colors.shadow_1) with get, set

    override this.Init(parent) =
        this
            .With(
                Text(fun () -> setting.Get() + if this.Selected && cursor_blink.Loops % 2 = 0 then "_" else "")
                    .Align(Alignment.LEFT)
                    .Color(this.TextColor),
                HotkeyListener(hotkey, toggle)
            )
            .AddConditional(
                this.Clickable,
                MouseListener()
                    .SelectOnClick(this)
                    .OnHover(fun now_focused ->
                        if now_focused && not this.Focused then
                                this.Focus true
                            elif not now_focused && not focus_trap && this.FocusedByMouse then
                                Selection.up true
                    )
                    .OnRightClick(fun () -> setting.Set "")
            )

        base.Init parent

    override this.OnSelected(by_mouse: bool) =
        base.OnSelected by_mouse
        Style.text_open.Play()

        Input.listen_to_text (
            setting |> Setting.trigger (fun v -> Style.key.Play()),
            not by_mouse,
            fun () ->
                if this.Selected then
                    this.Focus true
        )

    override this.OnDeselected(by_mouse: bool) =
        base.OnDeselected by_mouse
        Style.text_close.Play()
        Input.remove_listener ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        cursor_blink.Update(elapsed_ms)