namespace Interlude.UI

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

type PageSetting(localised_text, widget: Widget) as this =
    inherit Container(NodeType.Container(fun _ -> Some this.Child))

    let mutable widget = widget

    member this.Child
        with get () = widget
        and set (w: Widget) =
            let old_widget = widget
            widget <- w
            w.Position <- Position.ShrinkL(PAGE_LABEL_WIDTH).Shrink(Style.PADDING)

            if this.Initialised then
                w.Init this

                if old_widget.Focused then
                    w.Focus false

    member this.Label = localised_text

    override this.Init(parent) =
        this
        |* Text(localised_text + ":")
            .Color(fun () -> if widget.Focused then Colors.text_yellow_2 else Colors.text)
            .Align(Alignment.LEFT)
            .Position(
                Position
                    .Box(0.0f, 0.0f, 0.0f, 0.0f, PAGE_LABEL_WIDTH - 10.0f, PAGE_ITEM_HEIGHT)
                    .Shrink(Style.PADDING)
            )

        base.Init parent
        widget
            .Position(Position.ShrinkL(PAGE_LABEL_WIDTH).Shrink(Style.PADDING))
            .Init(this)

    override this.Draw() =
        if widget.Selected then
            Render.rect (widget.Bounds.Expand(15.0f, Style.PADDING)) Colors.pink_accent.O2
        elif widget.Focused then
            Render.rect (widget.Bounds.Expand(15.0f, Style.PADDING)) Colors.yellow_accent.O1

        base.Draw()
        widget.Draw()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        widget.Update(elapsed_ms, moved)