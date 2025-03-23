namespace Interlude.Features.Play

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.Options

module Lanecover =

    let draw (bounds: Rect) : unit =

        let bounds = bounds.Expand(0.0f, 2.0f)
        let fade_length = options.LaneCover.FadeLength.Value

        let upper (amount: float32) =
            Render.rect (bounds.SliceT(amount - fade_length)) options.LaneCover.Color.Value

            Render.rect_c
                (bounds.SliceT(amount).SliceB(fade_length))
                (Quad.gradient_top_to_bottom options.LaneCover.Color.Value (options.LaneCover.Color.Value.O4a 0))

        let lower (amount: float32) =
            Render.rect (bounds.SliceB(amount - fade_length)) options.LaneCover.Color.Value

            Render.rect_c
                (bounds.SliceB(amount).SliceT(fade_length))
                (Quad.gradient_top_to_bottom (options.LaneCover.Color.Value.O4a 0) options.LaneCover.Color.Value)

        let height = bounds.Height

        let sudden = options.LaneCover.Sudden.Value * height
        let hidden = options.LaneCover.Hidden.Value * height

        if options.Upscroll.Value then
            upper hidden
            lower sudden
        else
            lower hidden
            upper sudden

// this component is inserted into the widget draw order when OVER RECEPTORS
// for under receptors the playfield itself calls Lanecover.draw()
type LanecoverOverReceptors() =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        if options.LaneCover.Enabled.Value && not options.LaneCover.DrawUnderReceptors.Value then
            Lanecover.draw this.Bounds