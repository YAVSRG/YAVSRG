namespace Percyqaz.Flux.UI

open System.Drawing
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Graphics

// todo: generalise this concept into a container + a custom bit of rendering beneath all children
type FrameContainer(node_type: NodeType) =
    inherit Container(node_type)

    member val Fill = !%Palette.DARK with get, set
    member val Border = !%Palette.LIGHT with get, set

    override this.Draw() =
        let border = this.Border()

        if border.A > 0uy then

            Render.border Style.PADDING base.Bounds border

        let fill = this.Fill()

        if fill.A > 0uy then

            Render.rect base.Bounds fill

        base.Draw()

    static member Create(child: Widget) =
        FrameContainer(NodeType.Container(K (Some child))).With(child)

[<Extension>]
type FrameContainerExtensions() =

    [<Extension>]
    static member inline Fill(container: #FrameContainer, color: Color) : #FrameContainer =
        container.Fill <- K color
        container

    [<Extension>]
    static member inline Fill(container: #FrameContainer, color: unit -> Color) : #FrameContainer =
        container.Fill <- color
        container

    [<Extension>]
    static member inline Border(container: #FrameContainer, color: Color) : #FrameContainer =
        container.Border <- K color
        container

    [<Extension>]
    static member inline Border(container: #FrameContainer, color: unit -> Color) : #FrameContainer =
        container.Border <- color
        container