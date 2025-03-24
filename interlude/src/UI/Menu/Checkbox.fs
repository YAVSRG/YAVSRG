namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.UI

type Checkbox(setting: Setting<bool>) =
    inherit Container(NodeType.Button(fun () -> setting.Value <- not setting.Value; Style.click.Play()))

    override this.Init(parent: Widget) =
        this
            .Add(
                Text(fun () -> if setting.Value then Icons.CHECK_CIRCLE else Icons.CIRCLE)
                    .Align(Alignment.LEFT)
                    .Color(fun () -> if this.Focused then Colors.text_yellow_2 else Colors.text),
                MouseListener()
                    .Button(this)
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()