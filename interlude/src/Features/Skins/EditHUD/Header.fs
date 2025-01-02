namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

[<RequireQualifiedAccess>]
type private Transition =
    | In
    | Shown
    | Out
    | Hidden

type private EditHUDHeader(show_meta: Setting<bool>) as this =
    inherit Container(NodeType.Container(fun () -> Some this.Buttons))

    let HEIGHT = 90.0f

    let scaled_margins =
        let pc = (PRETTY_MARGIN_X - 5.0f) / 480.0f
        let offset = 5.0f - pc * 1440.0f
        { Position.DEFAULT with Left = pc %+ offset; Right = (1.0f - pc) %- offset }

    let tab_buttons =
        DynamicFlowContainer.LeftToRight(Spacing = 10.0f, Position = scaled_margins.SliceY(60.0f))
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.MOVE (%"hud.layout"),
            200.0f,
            (fun () -> show_meta.Set false),
            IsHighlighted = (show_meta.get_Value >> not),
            Keybind = Bind.mk Keys.D1
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.SETTINGS (%"hud.advanced"),
            200.0f,
            (fun () -> show_meta.Set true),
            IsHighlighted = (show_meta.get_Value),
            Keybind = Bind.mk Keys.D2
        )

    let transition_timer = Animation.Delay(400.0)
    let mutable transition = Transition.In

    member private this.Buttons = tab_buttons

    override this.Init(parent) =
        this.Position <- Position.SliceT(HEIGHT)
        this
        |* tab_buttons
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        match transition with
        | Transition.In ->
            if transition_timer.Complete then transition <- Transition.Shown
            transition_timer.Update elapsed_ms
        | Transition.Out ->
            if transition_timer.Complete then transition <- Transition.Hidden
            transition_timer.Update elapsed_ms
        | _ -> ()

    override this.Draw() =
        let before_alpha = Render.alpha_multiplier_begin 1.0f
        match transition with
        | Transition.In ->
            Render.stencil_create false
            let pc = (transition_timer.Time / transition_timer.Interval |> float32)
            let pc2 = pc * pc
            StripeWipe.draw_left_to_right 0.0f pc (this.Bounds.Expand(0.0f, Style.PADDING)) Color.Transparent

            Render.stencil_begin_draw()
            Render.rect this.Bounds Colors.shadow_2.O1
            Render.rect (this.Bounds.BorderB Style.PADDING) Colors.green_accent.O2
            base.Draw()
            StripeWipe.draw_left_to_right (pc2 - 0.05f) pc this.Bounds Colors.green

            Render.stencil_finish()

        | Transition.Out ->
            Render.stencil_create false
            let pc = (transition_timer.Time / transition_timer.Interval |> float32)
            let pc2 = (2.0f - pc) * pc
            StripeWipe.draw_left_to_right pc 1.0f (this.Bounds.Expand(0.0f, Style.PADDING)) Color.Transparent

            Render.stencil_begin_draw()
            Render.rect this.Bounds Colors.shadow_2.O1
            Render.rect (this.Bounds.BorderB Style.PADDING) Colors.green_accent.O2
            base.Draw()
            StripeWipe.draw_left_to_right (pc - 0.05f) pc2 this.Bounds Colors.green

            Render.stencil_finish()

        | Transition.Shown ->
            Render.rect this.Bounds Colors.shadow_2.O1
            Render.rect (this.Bounds.BorderB Style.PADDING) Colors.green_accent.O2
            base.Draw()

        | Transition.Hidden -> ()
        Render.alpha_multiplier_restore before_alpha

    member this.Hide() =
        transition_timer.Reset()
        transition <- Transition.Out

    member this.Show() =
        transition_timer.Reset()
        transition <- Transition.In