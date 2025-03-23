namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Common
open Prelude
open Interlude.UI
open Interlude.Features.Skins

type private HUDEditorButton(label: string, hotkey: Bind, action: unit -> unit) =
    inherit StaticWidget(NodeType.Button(fun () -> Style.click.Play(); action()))

    override this.Draw() =
        Render.rect this.Bounds (if this.Focused then Colors.yellow_accent.O2 else Colors.shadow_2.O2)
        Render.rect (this.Bounds.BorderR(10.0f).TranslateY(10.0f)) Colors.black.O3
        Render.rect (this.Bounds.BorderB(10.0f).TranslateX(10.0f)) Colors.black.O3
        Text.fill_b(Style.font, hotkey.ToString(), this.Bounds.Shrink(20.0f, 25.0f).TranslateY(22.0f), Colors.text_cyan, Alignment.CENTER)
        Text.fill_b(Style.font, label, this.Bounds.Shrink(20.0f, 15.0f).TranslateY(-10.0f), Colors.text, Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        if Mouse.hover this.Bounds then
            if not this.Focused && Mouse.moved_recently() then
                this.Focus true
            if Mouse.left_click() then
                this.Select true
        elif this.Focused then
            Selection.up true
        if hotkey.Tapped() then
            this.Select false
        base.Update(elapsed_ms, moved)

    override this.OnFocus by_mouse =
        Style.hover.Play()
        base.OnFocus by_mouse

type private HUDEditorControls(ctx: PositionerContext) =
    inherit Container(NodeType.None)

    let fade = Animation.Fade(1.0f)
    let mutable auto_show_timer = 0.0

    override this.Init(parent) =
        ctx.OnElementMoved.Publish.Add(
            fun () ->
                fade.Target <- 0.0f
                auto_show_timer <- 1000.0
        )

        this
        |+ Text(
            Icons.MOVE + " " + %"hud.editor",
            Position = Position.SliceT(90.0f).SliceL(500.0f).TranslateY(10.0f),
            Align = Alignment.CENTER
        )
        |+ HUDEditorButton(
            sprintf "%s %s" Icons.PLUS_CIRCLE (%"hud.add_more_elements"),
            %%"options",
            (fun () -> EditHUDPage(ctx).Show()),
            Position = Position.SliceT(65.0f).SliceL(500.0f).ShrinkX(25.0f).TranslateY(105.0f).Expand(Style.PADDING)
        )

        |+ Text(
            "Click an element to move/configure",
            Position = Position.SliceT(50.0f).ShrinkX(25.0f).TranslateY(10.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsNone)

        |+ Text(
            (fun () -> match ctx.Selected with Some e -> HudElement.name e | None -> ""),
            Position = Position.SliceT(90.0f).SliceR(500.0f).TranslateY(10.0f),
            Align = Alignment.CENTER
        )
        |+ HUDEditorButton(
            sprintf "%s %s" Icons.SETTINGS (%"hud.configure"),
            %%"context_menu",
            (fun () -> let v = ctx.Selected.Value in show_menu v (fun () -> ctx.Recreate v)),
            Position = Position.SliceT(65.0f).SliceR(500.0f).ShrinkX(25.0f).TranslateY(105.0f).Expand(Style.PADDING)
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ HUDEditorButton(
            sprintf "%s %s" Icons.ANCHOR (%"hud.anchor"),
            %%"hud_anchor",
            (fun () -> AnchorPage(ctx).Show()),
            Position = Position.SliceT(65.0f).SliceR(500.0f).ShrinkX(25.0f).TranslateY(200.0f).Expand(Style.PADDING)
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ HUDEditorButton(
            sprintf "%s %s" Icons.REFRESH_CW (%"hud.reset_position"),
            %%"hud_reset_position",
            ctx.ResetCurrentPosition,
            Position = Position.SliceT(65.0f).SliceR(500.0f).ShrinkX(25.0f).TranslateY(295.0f).Expand(Style.PADDING)
        )
            .Conditional(fun () -> ctx.Selected.IsSome)

        |+ Text(
            "Move: Arrow keys/Drag with mouse",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(385.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ Text(
            "Hold Ctrl or drag corners to resize",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(425.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ Text(
            "Hold Shift for symmetrical resizes",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(465.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ Text(
            "Hold Alt for smaller adjustments",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(505.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ Text(
            sprintf "Remove this element: %O" %%"delete",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(565.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ Text(
            sprintf "Mirror horizontally: %O" %%"hud_flip_horizontal",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(605.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |+ Text(
            sprintf "Mirror vertically: %O" %%"hud_flip_vertical",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(645.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)
        |* Text(
            sprintf "Undo: %O" %%"undo",
            Position = Position.SliceT(40.0f).ShrinkX(25.0f).TranslateY(685.0f),
            Color = K Colors.text_cyan,
            Align = Alignment.RIGHT
        )
            .Conditional(fun () -> ctx.Selected.IsSome)

        base.Init parent

    override this.Draw() =
        if fade.Alpha > 0 then
            let old_m = Render.alpha_multiplier_begin fade.Value
            base.Draw()
            Render.alpha_multiplier_restore old_m

    override this.Update(elapsed_ms, moved) =
        if fade.Alpha > 10 then
            base.Update(elapsed_ms, moved)
        else
            if (%%"options").Tapped() then
                EditHUDPage(ctx).Show()
            elif (%%"context_menu").Tapped() then
                let v = ctx.Selected.Value in show_menu v (fun () -> ctx.Recreate v)
            elif (%%"hud_anchor").Tapped() then
                AnchorPage(ctx).Show()
            elif (%%"hud_reset_position").Tapped() then
                ctx.ResetCurrentPosition()

        if (%%"undo").Tapped() then
            ctx.Undo()
        elif (%%"delete").Tapped() then
            ctx.RemoveElement()
        elif (%%"hud_flip_horizontal").Tapped() then
            ctx.HorizontalFlip()
        elif (%%"hud_flip_vertical").Tapped() then
            ctx.VerticalFlip()
        elif (%%"hud_flip_horizontal_all").Tapped() then
            ctx.HorizontalFlipAll()
        elif (%%"hud_flip_vertical_all").Tapped() then
            ctx.VerticalFlipAll()

        if fade.Target = 0.0f then
            auto_show_timer <- auto_show_timer - elapsed_ms

            if auto_show_timer <= 0.0 then
                fade.Target <- 1.0f

        fade.Update elapsed_ms