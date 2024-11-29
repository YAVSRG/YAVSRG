namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.OptionsMenu.Search

[<RequireQualifiedAccess>]
type private Transition =
    | In
    | Shown
    | Out
    | Hidden

[<RequireQualifiedAccess>]
type private OptionsMenuTab =
    | System
    | Gameplay
    | Library
    | Noteskins
    | SearchResults of content: Widget

module private State =

    let mutable recent_tab = OptionsMenuTab.System

type private OptionsMenuHeader(current_tab: Setting<OptionsMenuTab>) as this =
    inherit Container(NodeType.Container(fun () -> Some this.Buttons))

    let HEIGHT = 90.0f

    let scaled_margins =
        let pc = (PRETTY_MARGIN_X - 5.0f) / 480.0f
        let offset = 5.0f - pc * 1440.0f
        { Position.DEFAULT with Left = pc %+ offset; Right = (1.0f - pc) %- offset }

    let tab_buttons =
        DynamicFlowContainer.LeftToRight(Spacing = 10.0f, Position = scaled_margins.SliceY(60.0f))
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.AIRPLAY (%"system"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.System),
            IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.System),
            Keybind = Bind.mk Keys.D1
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.SLIDERS (%"gameplay"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.Gameplay),
            IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.Gameplay),
            Keybind = Bind.mk Keys.D2
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.IMAGE (%"skins"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.Noteskins),
            IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.Noteskins),
            Keybind = Bind.mk Keys.D3
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.ARCHIVE (%"library"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.Library),
            IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.Library),
            Keybind = Bind.mk Keys.D4
        )

    let search_box =
        { new SearchBox(
                Setting.simple "", 
                (fun query ->
                    if query = "" then
                        current_tab.Set State.recent_tab
                    else
                        current_tab.Set (OptionsMenuTab.SearchResults <| SearchResults.get query)
                ),
                Position = { scaled_margins with Left = fst scaled_margins.Left * 2.0f, snd scaled_margins.Left * 2.0f }.SliceY(60.0f).ShrinkL(900.0f).Shrink(Style.PADDING, 0.0f),
                Fill = K Colors.cyan.O3,
                Border = K Colors.cyan_accent,
                TextColor = K Colors.text_cyan) with
            override this.OnFocus by_mouse =
                base.OnFocus by_mouse
                if not by_mouse then defer (fun () -> this.Select false)
        }

    let transition_timer = Animation.Delay(400.0)
    let mutable transition = Transition.In

    member private this.Buttons = tab_buttons
        
    override this.Init(parent) =
        this.Position <- Position.SliceT(HEIGHT)
        this
        |+ search_box
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
            Draw.rect this.Bounds Colors.shadow_2.O1
            Draw.rect (this.Bounds.BorderB Style.PADDING) Colors.cyan_accent.O2
            base.Draw()
            StripeWipe.draw_left_to_right (pc2 - 0.05f) pc this.Bounds Colors.cyan

            Render.stencil_finish()

        | Transition.Out ->
            Render.stencil_create false
            let pc = (transition_timer.Time / transition_timer.Interval |> float32)
            let pc2 = (2.0f - pc) * pc
            StripeWipe.draw_left_to_right pc 1.0f (this.Bounds.Expand(0.0f, Style.PADDING)) Color.Transparent

            Render.stencil_begin_draw()
            Draw.rect this.Bounds Colors.shadow_2.O1
            Draw.rect (this.Bounds.BorderB Style.PADDING) Colors.cyan_accent.O2
            base.Draw()
            StripeWipe.draw_left_to_right (pc - 0.05f) pc2 this.Bounds Colors.cyan

            Render.stencil_finish()

        | Transition.Shown ->
            Draw.rect this.Bounds Colors.shadow_2.O1
            Draw.rect (this.Bounds.BorderB Style.PADDING) Colors.cyan_accent.O2
            base.Draw()

        | Transition.Hidden -> ()
        Render.alpha_multiplier_restore before_alpha

    member this.Hide() =
        transition_timer.Reset()
        transition <- Transition.Out

    member this.Show() =
        transition_timer.Reset()
        transition <- Transition.In

type private OptionsMenuFooter() as this =
    inherit Container(NodeType.Container(fun () -> Some this.Items))

    let HEIGHT = 70.0f

    let items = 
        NavigationContainer.Row()
        |+ InlaidButton(
            %"menu.back",
            Menu.Back,
            Icons.ARROW_LEFT_CIRCLE,
            Position = Position.Box(0.0f, 1.0f, 10.0f, -HEIGHT + 7.5f, 180.0f, InlaidButton.HEIGHT)
        )
        |+ (
            FlowContainer.RightToLeft(300.0f, Spacing = 20.0f, Position = Position.SliceB(HEIGHT + 10.0f).Translate(-30.0f, -20.0f))
            |+ Presets.preset_buttons 3 options.Preset3
            |+ Presets.preset_buttons 2 options.Preset2
            |+ Presets.preset_buttons 1 options.Preset1
        )

    member this.Items = items

    override this.Init(parent) =
        this |* items
        this.Position <- Position.SliceB HEIGHT
        base.Init parent

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O1
        Draw.rect (this.Bounds.BorderT Style.PADDING) Colors.cyan_accent.O2
        base.Draw()