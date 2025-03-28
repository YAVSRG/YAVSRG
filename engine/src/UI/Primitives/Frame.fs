namespace Percyqaz.Flux.UI

open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics

[<Sealed>]
type Frame() =
    inherit StaticWidget(NodeType.None)

    member val Fill : unit -> Color = !%Palette.DARK with get, set
    member val Border : unit -> Color = !%Palette.LIGHT with get, set

    override this.Draw() =
        let border = this.Border()

        if border.A > 0uy then

            let r = this.Bounds.Expand Style.PADDING
            Render.rect (r.SliceL Style.PADDING) border
            Render.rect (r.SliceR Style.PADDING) border

            let r = this.Bounds.Expand(0.0f, Style.PADDING)
            Render.rect (r.SliceT Style.PADDING) border
            Render.rect (r.SliceB Style.PADDING) border

        let fill = this.Fill()

        if fill.A > 0uy then

            Render.rect base.Bounds fill

[<Extension>]
type FrameExtensions() =

    [<Extension>]
    static member inline Fill(container: Frame, color: Color) : Frame =
        container.Fill <- K color
        container

    [<Extension>]
    static member inline Fill(container: Frame, color: unit -> Color) : Frame =
        container.Fill <- color
        container

    [<Extension>]
    static member inline Border(container: Frame, color: Color) : Frame =
        container.Border <- K color
        container

    [<Extension>]
    static member inline Border(container: Frame, color: unit -> Color) : Frame =
        container.Border <- color
        container