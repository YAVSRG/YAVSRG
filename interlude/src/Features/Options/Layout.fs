namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.Options
open Interlude.UI
open Interlude.Features.Skins
open Interlude.Features.OptionsMenu.Search
open Interlude.Features.OptionsMenu.Presets

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
        let pc = (PAGE_MARGIN_X - 5.0f) / 480.0f
        let offset = 5.0f - pc * 1440.0f
        { Position.DEFAULT with Left = pc %+ offset; Right = (1.0f - pc) %- offset }

    let tab_buttons =
        DynamicFlowContainer.LeftToRight()
            .Spacing(Style.PADDING * 2.0f)
            .Position(scaled_margins.SliceY(SearchBox.HEIGHT))
            .With(
                OptionsMenuButton(
                    sprintf "%s %s" Icons.SLIDERS (%"gameplay"),
                    200.0f,
                    (fun () -> current_tab.Set OptionsMenuTab.Gameplay),
                    IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.Gameplay),
                    Keybind = Bind.mk Keys.D1
                ),
                OptionsMenuButton(
                    sprintf "%s %s" Icons.AIRPLAY (%"system"),
                    200.0f,
                    (fun () -> current_tab.Set OptionsMenuTab.System),
                    IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.System),
                    Keybind = Bind.mk Keys.D2
                ),
                OptionsMenuButton(
                    sprintf "%s %s" Icons.IMAGE (%"skins"),
                    200.0f,
                    (fun () -> current_tab.Set OptionsMenuTab.Noteskins),
                    IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.Noteskins),
                    Keybind = Bind.mk Keys.D3
                ),
                OptionsMenuButton(
                    sprintf "%s %s" Icons.ARCHIVE (%"library"),
                    200.0f,
                    (fun () -> current_tab.Set OptionsMenuTab.Library),
                    IsHighlighted = (fun () -> current_tab.Value = OptionsMenuTab.Library),
                    Keybind = Bind.mk Keys.D4
                )
            )

    let search_box =
        SearchBox(fun query ->
            if query = "" then current_tab.Set State.recent_tab
            else current_tab.Set (OptionsMenuTab.SearchResults <| SearchResults.get query)
        )
            .Fill(Colors.cyan.O3)
            .Border(Colors.cyan_accent)
            .TextColor(Colors.text_cyan)
            .KeyboardAutoSelect()
            .Position(
                { scaled_margins with Left = fst scaled_margins.Left * 2.0f, snd scaled_margins.Left * 2.0f }
                    .SliceY(SearchBox.HEIGHT)
                    .ShrinkL(900.0f)
                    .Shrink(Style.PADDING, 0.0f)
            )

    let transition_timer = Animation.Delay(400.0)
    let mutable transition = Transition.In

    member private this.Buttons = tab_buttons

    override this.Init(parent) =
        this
            .Position(Position.SliceT(HEIGHT))
            .Add(search_box, tab_buttons)

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
            let pc = float32 transition_timer.Progress
            let pc2 = pc * pc
            StripeWipe.draw_left_to_right 0.0f pc (this.Bounds.Expand(0.0f, Style.PADDING)) Color.Transparent

            Render.stencil_begin_draw()
            Render.rect this.Bounds Colors.shadow_2.O1
            Render.rect (this.Bounds.BorderB Style.PADDING) Colors.cyan_accent.O2
            base.Draw()
            StripeWipe.draw_left_to_right (pc2 - 0.05f) pc this.Bounds Colors.cyan

            Render.stencil_finish()

        | Transition.Out ->
            Render.stencil_create false
            let pc = float32 transition_timer.Progress
            let pc2 = (2.0f - pc) * pc
            StripeWipe.draw_left_to_right pc 1.0f (this.Bounds.Expand(0.0f, Style.PADDING)) Color.Transparent

            Render.stencil_begin_draw()
            Render.rect this.Bounds Colors.shadow_2.O1
            Render.rect (this.Bounds.BorderB Style.PADDING) Colors.cyan_accent.O2
            base.Draw()
            StripeWipe.draw_left_to_right (pc - 0.05f) pc2 this.Bounds Colors.cyan

            Render.stencil_finish()

        | Transition.Shown ->
            Render.rect this.Bounds Colors.shadow_2.O1
            Render.rect (this.Bounds.BorderB Style.PADDING) Colors.cyan_accent.O2
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

    let presets =
        NavigationContainer.Row(WrapNavigation = false)
            .With(
                PresetSlotControls.Create(1, options.Preset1)
                    .Position(Position.GridX(1, 3, 20.0f)),
                PresetSlotControls.Create(2, options.Preset2)
                    .Position(Position.GridX(2, 3, 20.0f)),
                PresetSlotControls.Create(3, options.Preset3)
                    .Position(Position.GridX(3, 3, 20.0f))
            )
            .Position(Position.SlicePercentR(0.5f).SliceB(15.0f, HEIGHT).ShrinkX(25.0f))

    let content =
        NavigationContainer.Row()
            .With(
                InlaidButton(%"menu.back", Menu.Back)
                    .Icon(Icons.ARROW_LEFT_CIRCLE)
                    .Position(Position.SliceB(HEIGHT).SliceY(InlaidButton.HEIGHT).SliceL(10.0f, 180.0f)),

                InlaidButton(%"noteskin.edit", Skinning.edit_or_extract_noteskin)
                    .Icon(Icons.IMAGE)
                    .Position(
                        Position
                            .SliceB(HEIGHT)
                            .SliceY(InlaidButton.HEIGHT)
                            .ShrinkPercentR(0.5f)
                            .ShrinkL(210.0f)
                            .GridX(1, 2, 20.0f)
                    ),

                InlaidButton(%"hud.edit", fun () ->
                    Skinning.edit_hud ignore
                )
                    .Icon(Icons.ZAP)
                    .Position(
                        Position
                            .SliceB(HEIGHT)
                            .SliceY(InlaidButton.HEIGHT)
                            .ShrinkPercentR(0.5f)
                            .ShrinkL(210.0f)
                            .GridX(2, 2, 20.0f)
                    ),

                presets
            )

    member this.Items = content

    override this.Init(parent) =
        this.With(content).Position <- Position.SliceB HEIGHT
        base.Init parent

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O1
        Render.rect (this.Bounds.BorderT Style.PADDING) Colors.cyan_accent.O2
        base.Draw()