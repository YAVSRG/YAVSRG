namespace Percyqaz.Flux.UI

open Percyqaz.Common
open Percyqaz.Flux.Input

type TextEntry(setting: Setting<string>, bind) as this =
    inherit StaticContainer(NodeType.Leaf)

    let color = Animation.Fade(0.5f)

    let toggle() = if this.Selected then this.Focus() else this.Select()

    do
        this
        |+ Text(setting.Get, Align = Alignment.LEFT, Color = fun () -> (Style.color(255, 1.0f, color.Value), System.Drawing.Color.Black))
        |+ Clickable(this.Select)
        |* HotkeyAction(bind, toggle)

    override this.OnSelected() =
        base.OnSelected()
        color.Target <- 1.0f
        Input.setTextInput(setting, fun () -> if this.Selected then this.Focus())

    override this.OnDeselected() =
        base.OnDeselected()
        color.Target <- 0.5f
        Input.removeInputMethod()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        color.Update(elapsedTime) |> ignore

