namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics

type private HUDEditorButton(label: string, hotkey: Bind, action: unit -> unit) =
    inherit
        StaticWidget(
            NodeType.Button(fun () ->
                Style.click.Play()
                action()
            )
        )

    override this.Draw() : unit =
        Render.rect this.Bounds (if this.Focused then Colors.yellow_accent.O2 else Colors.shadow_2.O2)
        Render.rect (this.Bounds.BorderR(10.0f).TranslateY(10.0f)) Colors.black.O3
        Render.rect (this.Bounds.BorderB(10.0f).TranslateX(10.0f)) Colors.black.O3

        Text.fill_b(
            Style.font,
            hotkey.ToString(),
            this.Bounds.Shrink(20.0f, 25.0f).TranslateY(22.0f),
            Colors.text_cyan,
            Alignment.CENTER
        )

        Text.fill_b(
            Style.font,
            label,
            this.Bounds.Shrink(20.0f, 15.0f).TranslateY(-10.0f),
            Colors.text,
            Alignment.CENTER
        )

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        if Mouse.hover this.Bounds then
            if not this.Focused && Mouse.moved_recently() then
                this.Focus(true)

            if Mouse.left_clicked() then
                this.Select(true)
        elif this.Focused then
            Selection.up true

        if hotkey.Pressed() then
            this.Select(false)

        base.Update(elapsed_ms, moved)

    override this.OnFocus(by_mouse: bool) : unit =
        Style.hover.Play()
        base.OnFocus(by_mouse)
