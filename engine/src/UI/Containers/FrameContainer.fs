namespace Percyqaz.Flux.UI

open Percyqaz.Flux.Graphics

// todo: generalise this concept into a container + a custom bit of rendering beneath all children
type FrameContainer(nt: NodeType) =
    inherit Container(nt)

    member val Fill = !%Palette.DARK with get, set
    member val Border = !%Palette.LIGHT with get, set

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

        base.Draw()