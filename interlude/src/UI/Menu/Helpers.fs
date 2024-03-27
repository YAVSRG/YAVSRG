namespace Interlude.UI.Menu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Interlude.Utils
open Interlude.UI

[<AutoOpen>]
module PageLayout =

    let PRETTYTEXTWIDTH = 425.0f
    let PRETTYHEIGHT = 70.0f
    let PRETTYWIDTH = 1080.0f
    let PRETTY_MARGIN_Y = (1080.0f - (10.0f * PRETTYHEIGHT)) * 0.5f
    let PRETTY_MARGIN_X = 100.0f

    let pretty_pos(start: int, height: int, width: float32) =
        Position.Box(0.0f, 0.0f, 0.0f, float32 start * 0.5f * PRETTYHEIGHT, width, float32 height * 0.5f * PRETTYHEIGHT)

type Tooltip(content: Callout) =
    inherit StaticWidget(NodeType.None)

    let content = content.Icon(Icons.INFO)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Mouse.hover this.Bounds then
            Notifications.tooltip_available <- true

            if (%%"tooltip").Tapped() then
                Notifications.tooltip ((%%"tooltip"), this, content)

    override this.Draw() = ()

    static member Info(feature: string) =
        Callout.Normal
            .Title(%(sprintf "%s.name" feature))
            .Body(%(sprintf "%s.tooltip" feature))

    static member Info(feature: string, hotkey: Hotkey) =
        Callout.Normal
            .Title(%(sprintf "%s.name" feature))
            .Body(%(sprintf "%s.tooltip" feature))
            .Hotkey(hotkey)
