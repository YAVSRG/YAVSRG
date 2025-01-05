namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.UI
open Interlude.Features.Skins

type ElementCard(element: HudElement, on_select: unit -> unit) =
    inherit Container(NodeType.None)

    let enabled = HudElement.enabled_setting element

    override this.Init (parent: Widget) =
        this
        |+ Text(HudElement.name element, Position = Position.SliceT(50.0f))
        |+ Button(
            (fun () ->
                if enabled.Value then sprintf "%s %s" Icons.CHECK_CIRCLE %"hud.editor.enabled"
                else sprintf "%s %s" Icons.CIRCLE %"hud.editor.disabled"
            ),
            (fun () -> enabled.Value <- not enabled.Value),
            Position = Position.ShrinkB(50.0f).SliceB(50.0f))
        |* Button(
            sprintf "%s %s" Icons.SETTINGS %"hud.configure",
            (fun () -> show_menu element ignore),
            Disabled = K (not (HudElement.can_configure element)),
            Position = Position.SliceB(50.0f)
        )

        base.Init parent

    override this.Draw() =
        Render.rect (this.Bounds.SliceT(50.0f)) Colors.shadow_2
        Render.rect (this.Bounds.ShrinkT(50.0f).ShrinkB(100.0f)) Colors.shadow_2.O2
        Render.rect (this.Bounds.SliceB(100.0f)) Colors.shadow_2

        base.Draw()

module ElementGrid =

    let create () =
        let grid = GridFlowContainer(400.0f, (if Render.width() >= 1680.0f then 4 else 3), Spacing = (20.0f, 20.0f))
        for element in HudElement.FULL_LIST do
            grid.Add(ElementCard(element, ignore))
        grid