namespace Interlude.Features.OptionsMenu

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu.Search

type private OptionsMenuButton(label: string, width: float32, on_click: unit -> unit, is_selected: unit -> bool) =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            )
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Init(parent) =
        this |* Clickable.Focus this
        base.Init(parent)

    override this.Draw() =
        let is_selected = is_selected()
        let trim_color =
            if is_selected then Colors.pink_accent
            elif this.Focused then Colors.black
            else Colors.shadow_1
            
        let color =
            if is_selected then Colors.pink_accent.O2
            elif this.Focused then Colors.shadow_2.O3
            else Colors.shadow_2.O2

        let text_color =
            if this.Focused then Colors.text_yellow_2
            else Colors.text

        Draw.rect this.Bounds color
        Draw.rect (this.Bounds.BorderBottom Style.PADDING) trim_color

        Text.fill_b (Style.font, label, this.Bounds.Shrink(Style.PADDING * 2.0f), text_color, Alignment.CENTER)

    interface IWidth with
        member this.Width = width

[<RequireQualifiedAccess>]
type private Transition =
    | In
    | Shown
    | Out
    | Hidden

[<RequireQualifiedAccess>]
type private OptionsMenuTab =
    | Home
    | System
    | Gameplay
    | Library
    | Noteskins
    | SearchResults of content: Widget

type private OptionsMenuHeader(current_tab: Setting<OptionsMenuTab>) as this =
    inherit Container(NodeType.Container(fun () -> Some this.Buttons))

    let HEIGHT = 100.0f

    let search_text = Setting.simple ""

    let search_box =
        { new SearchBox(
                search_text, 
                (fun () ->
                    if search_text.Value = "" then
                        current_tab.Set OptionsMenuTab.Home
                    else
                        current_tab.Set (OptionsMenuTab.SearchResults <| SearchResults.get search_text.Value)
                ),
                Position = Position.CenterY(60.0f).Margin(PRETTY_MARGIN_X, 0.0f).SliceRight(600.0f),
                Fill = K Colors.cyan.O3,
                Border = K Colors.cyan_accent,
                TextColor = K Colors.text_cyan) with
            override this.OnFocus by_mouse =
                base.OnFocus by_mouse
                if not by_mouse then defer (fun () -> this.Select false)
        }

    let tab_buttons =
        DynamicFlowContainer.LeftToRight(Spacing = 10.0f, Position = Position.SliceBottom(60.0f).Margin(PRETTY_MARGIN_X, 0.0f))
        |+ OptionsMenuButton(
            Icons.HOME,
            60.0f,
            (fun () -> current_tab.Set OptionsMenuTab.Home),
            (fun () -> current_tab.Value = OptionsMenuTab.Home)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.AIRPLAY (%"system"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.System),
            (fun () -> current_tab.Value = OptionsMenuTab.System)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.SLIDERS (%"gameplay"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.Gameplay),
            (fun () -> current_tab.Value = OptionsMenuTab.Gameplay)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.ARCHIVE (%"library"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.Library),
            (fun () -> current_tab.Value = OptionsMenuTab.Library)
        )
        |+ OptionsMenuButton(
            sprintf "%s %s" Icons.IMAGE (%"noteskins"),
            200.0f,
            (fun () -> current_tab.Set OptionsMenuTab.Noteskins),
            (fun () -> current_tab.Value = OptionsMenuTab.Noteskins)
        )

    let transition_timer = Animation.Delay(400.0)
    let mutable transition = Transition.In

    member private this.Buttons = tab_buttons
        
    override this.Init(parent) =
        this.Position <- Position.SliceTop(HEIGHT)
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
        match transition with
        | Transition.In ->
            Stencil.start_stencilling false
            let pc = (transition_timer.Time / transition_timer.Interval |> float32)
            let pc2 = pc * pc
            StripeWipe.draw_left_to_right 0.0f pc (this.Bounds.Expand(0.0f, Style.PADDING)) Color.Transparent

            Stencil.start_drawing()
            Draw.rect this.Bounds Colors.shadow_2.O2
            Draw.rect (this.Bounds.BorderBottom Style.PADDING) Colors.shadow_2.O3
            base.Draw()
            StripeWipe.draw_left_to_right (pc2 - 0.05f) pc this.Bounds Colors.cyan

            Stencil.finish()

        | Transition.Out ->
            Stencil.start_stencilling false
            let pc = (transition_timer.Time / transition_timer.Interval |> float32)
            let pc2 = (2.0f - pc) * pc
            StripeWipe.draw_left_to_right pc 1.0f (this.Bounds.Expand(0.0f, Style.PADDING)) Color.Transparent

            Stencil.start_drawing()
            Draw.rect this.Bounds Colors.shadow_2.O2
            Draw.rect (this.Bounds.BorderBottom Style.PADDING) Colors.shadow_2.O3
            base.Draw()
            StripeWipe.draw_left_to_right (pc - 0.05f) pc2 this.Bounds Colors.cyan

            Stencil.finish()

        | Transition.Shown ->
            Draw.rect this.Bounds Colors.shadow_2.O2
            Draw.rect (this.Bounds.BorderBottom Style.PADDING) Colors.shadow_2.O3
            base.Draw()

        | Transition.Hidden -> ()

    member this.Hide() =
        transition_timer.Reset()
        transition <- Transition.Out

    member this.Show() =
        transition_timer.Reset()
        transition <- Transition.In