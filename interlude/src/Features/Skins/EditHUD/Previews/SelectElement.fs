namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI
open Interlude.Features.Skins

type ElementCard(element: HudElement, on_select: unit -> unit) =
    inherit Container(NodeType.None)

    let mutable hover = false
    let enabled = HudElement.enabled_setting element
    let color = Animation.Color(if enabled.Value then Colors.pink else Colors.shadow_2)
    let enabled = enabled |> Setting.trigger (fun b -> color.Target <- if b then Colors.pink else Colors.shadow_2)

    let preview_size_override, draw_preview = SelectPreviews.create Content.HUD element
    let preview_size_x, preview_size_y =
        match preview_size_override with
        | Some s -> s
        | None ->
            let position : HudPosition = HudElement.position_setting(element).Value
            let width = fst position.Right - fst position.Left
            let height = fst position.Bottom - fst position.Top

            let scale =
                if width > 200.0f || height > 200.0f then
                    min (200.0f / width) (200.0f / height)
                else 1.0f
            width * scale, height * scale
    let mutable draw_preview = draw_preview

    override this.Init (parent: Widget) =
        this
        |+ Text(HudElement.name element)
            .Color(fun () -> if hover then Colors.text_yellow_2 else Colors.text)
            .Position(Position.SliceT(50.0f))
        |+ Button(
            (fun () ->
                if enabled.Value then sprintf "%s %s" Icons.CHECK_CIRCLE %"hud.editor.enabled"
                else sprintf "%s %s" Icons.CIRCLE %"hud.editor.disabled"
            ),
            (fun () -> enabled.Value <- not enabled.Value)
        )
            .Position(Position.ShrinkB(50.0f).SliceB(50.0f))
        |* Button(
            sprintf "%s %s" Icons.SETTINGS %"hud.configure",
            (fun () -> show_menu element (fun () -> draw_preview <- snd (SelectPreviews.create Content.HUD element)))
        )
            .Disabled(not (HudElement.can_configure element))
            .Position(Position.SliceB(50.0f))

        base.Init parent

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

        Render.rect (this.Bounds.SliceT(50.0f)) color.Value
        Render.rect (this.Bounds.ShrinkT(50.0f).ShrinkB(100.0f)) color.Value.O1
        Render.rect (this.Bounds.SliceB(100.0f)) color.Value

        let preview_bounds = this.Bounds.SliceX(preview_size_x).SliceY(preview_size_y).TranslateY(-25.0f)
        draw_preview preview_bounds

        base.Draw()

module ElementGrid =

    let create (select_element: HudElement -> unit) : GridFlowContainer<ElementCard> =

        let columns =
            let w = Render.width()
            if w < 1600.0f then 3
            elif w < 1760.0f then 4
            else 5

        let grid = GridFlowContainer(400.0f, columns, Spacing = (20.0f, 20.0f))
        for element in HudElement.FULL_LIST do
            grid.Add(ElementCard(element, fun () -> select_element element))
        grid