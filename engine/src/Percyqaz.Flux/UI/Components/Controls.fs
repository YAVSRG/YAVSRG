namespace Percyqaz.Flux.UI

open System.Drawing
open Percyqaz.Common
open Percyqaz.Flux.Input

type TextEntry(setting: Setting<string>, bind: Hotkey) as this =
    inherit StaticContainer(NodeType.Leaf)

    let color = Animation.Fade(0.5f)
    let ticker = Animation.Counter(600.0)

    let toggle() = if this.Selected then this.Focus() else this.Select()

    do
        this
        |+ Text(
            (fun () -> setting.Get() + if this.Selected && ticker.Loops % 2 = 0 then "_" else ""),
            Align = Alignment.LEFT, 
            Color = fun () -> (Style.highlight(255, color.Value), Color.Black))
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
        ticker.Update(elapsedTime) |> ignore

