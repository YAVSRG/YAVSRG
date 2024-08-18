namespace Interlude.Features.Rulesets.Edit

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

type private ColoredButton(label, color, action) =
    inherit
        Container(
            NodeType.Button(fun _ ->
                Style.click.Play()
                action ()
            )
        )

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(
                sprintf "%s  >" label
            ),
            Color =
                (fun () ->
                    if this.Focused then Colors.text_yellow_2 else (color, Colors.shadow_2)
                ),
            Align = Alignment.LEFT,
            Position = Position.Shrink(Style.PADDING)
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()