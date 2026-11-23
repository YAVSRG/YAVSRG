namespace Interlude.Features.Collections

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics

type private CollectionButton(icon: string, name: string, action: unit -> unit) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()

                if not this.Disabled then
                    action()
            )
        )

    member val Disabled = false with get, set

    override this.Init(parent: Widget) : unit =
        this
            .With(
                Text(sprintf "%s %s" icon name)
                    .Align(Alignment.LEFT)
                    .Color(if this.Disabled then Colors.text_greyout else Colors.text)
                    .Position(Position.Shrink(20.0f, 15.0f))
            )
            .AddConditional(not this.Disabled, MouseListener().Button(this))

        base.Init(parent)

    override this.OnFocus(by_mouse: bool) : unit =
        base.OnFocus(by_mouse)
        Style.hover.Play()

    override this.Draw() : unit =
        let color = if this.Focused then Colors.pink_accent else Colors.shadow_1
        let color = if this.Disabled then color.O3 else color

        Render.rect this.Bounds color.O3
        Render.rect (this.Bounds.Expand(0.0f, 5.0f).SliceB(5.0f)) color

        base.Draw()
