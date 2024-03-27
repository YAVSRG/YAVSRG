namespace Interlude.UI.Menu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Interlude.Utils
open Interlude.UI

[<AutoOpen>]
module PageLayout =

    [<Struct>]
    [<RequireQualifiedAccess>]
    type PageWidth =
        | Normal
        | Full
        | Custom of float32

    let PRETTYHEIGHT = 65.0f
    let PRETTYTEXTWIDTH = PRETTYHEIGHT * 6.0f
    let PRETTYWIDTH = 1080.0f

    let PAGE_BOTTOM = 23

    let PRETTY_MARGIN_Y = (1080.0f - (float32 PAGE_BOTTOM * 0.5f * PRETTYHEIGHT)) * 0.5f
    let PRETTY_MARGIN_X = 100.0f

    let pretty_pos(start: int, height: int, width: PageWidth) =
        match width with
        | PageWidth.Normal ->
            Position.Box(0.0f, 0.0f, 0.0f, float32 start * 0.5f * PRETTYHEIGHT, PRETTYWIDTH, float32 height * 0.5f * PRETTYHEIGHT)
        | PageWidth.Full ->
            Position.Row(float32 start * 0.5f * PRETTYHEIGHT, float32 height * 0.5f * PRETTYHEIGHT)
        | PageWidth.Custom w ->
            Position.Box(0.0f, 0.0f, 0.0f, float32 start * 0.5f * PRETTYHEIGHT, w, float32 height * 0.5f * PRETTYHEIGHT)
            
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
