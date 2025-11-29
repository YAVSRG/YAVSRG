namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Features.Skins

type ElementCard(element: HudElement, on_select: unit -> unit) =
    inherit Container(NodeType.None)

    let mutable hover = false
    let enabled = HudElement.enabled_setting element
    let color = Animation.Color(if enabled.Value then Colors.pink else Colors.shadow_2)
    let enabled = enabled |> Setting.trigger (fun b -> color.Target <- if b then Colors.pink else Colors.shadow_2)

    override this.Init (parent: Widget) =
        this
            .Add(
                Text(HudElement.name element)
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceT(50.0f).ShrinkR(50.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING))
                    .Conditional(not << enabled.get_Value),
                Button(Icons.PLUS_CIRCLE, fun () -> enabled.Value <- true)
                    .Align(Alignment.RIGHT)
                    .Position(Position.SliceT(50.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING))
                    .Conditional(not << enabled.get_Value),

                Button(HudElement.name element, on_select)
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceT(50.0f).ShrinkR(50.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING))
                    .Conditional(enabled.get_Value),
                Button(Icons.TRASH, fun () -> enabled.Value <- false)
                    .Align(Alignment.RIGHT)
                    .Position(Position.SliceT(50.0f).SliceR(50.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING))
                    .TextColor(Colors.text_red)
                    .Conditional(enabled.get_Value),

                Button(
                    sprintf "%s %s" Icons.SETTINGS %"hud.configure",
                    (fun () -> show_menu element ignore)
                )
                    .Disabled(not (HudElement.can_configure element))
                    .Position(Position.SliceB(50.0f).Shrink(Style.PADDING * 2.0f, Style.PADDING))
            )

        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        let was_hover = hover
        hover <- Mouse.hover this.Bounds
        base.Update(elapsed_ms, moved)

        color.Update elapsed_ms

        if hover && not was_hover then
            Style.hover.Play()

        if hover && Mouse.left_clicked() then
            on_select()

    override this.Draw() =

        Render.rect (this.Bounds.SliceT(50.0f)) color.Value.O2
        Render.rect (this.Bounds.SliceB(50.0f)) color.Value.O1

        base.Draw()

module ElementGrid =

    let create (select_element: HudElement -> unit) : GridFlowContainer<ElementCard> =

        let columns =
            let w = Render.width()
            if w < 1600.0f then 3
            elif w < 1760.0f then 4
            else 5

        let grid = GridFlowContainer(100.0f, columns, Spacing = (20.0f, 20.0f))
        for element in HudElement.LIST_ORDER do
            grid.Add(ElementCard(element, fun () -> select_element element))
        grid