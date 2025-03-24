namespace Interlude.UI

open System
open OpenTK.Mathematics
open Prelude
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

module Beam =

    let private draw (direction_deg: float32) (x: float32, y: float32) (r1: float32) (r2: float32) (col: Color) (time: float32) : unit =
        // angle is measured clockwise from vertical
        let cos = MathF.Cos(direction_deg / 180.0f * MathF.PI)
        let sin = MathF.Sin(direction_deg / 180.0f * MathF.PI)

        let o = Vector2(x, y)
        let d = Vector2(sin, -cos)
        let perp = Vector2(cos * 2.5f, sin * 2.5f)
        let startpoint = r1 + (r2 - r1) * (time * time)
        let endpoint = r1 + (r2 - r1) * (time * (2.0f - time))

        Render.quad_vecs
            (o + d * startpoint + perp)
            (o + d * endpoint + perp)
            (o + d * endpoint - perp)
            (o + d * startpoint - perp)
            col

module Glint =

    let COLOR = Colors.white.O4a 26
    let RECIPROCAL_GRADIENT = 0.35f

    let draw (percent: float32) (bounds: Rect) (color: Color) : unit =
        if percent <= 0.0f || percent >= 1.0f then () else

        let stripe_width = bounds.Height * RECIPROCAL_GRADIENT
        let travel_distance = bounds.Width + stripe_width * 4.5f

        let start = bounds.Left - stripe_width * 4.5f + travel_distance * percent

        Render.quad_points
            (start + stripe_width, bounds.Top)
            (start + stripe_width * 3.0f, bounds.Top)
            (start + stripe_width * 2.0f, bounds.Bottom)
            (start, bounds.Bottom)
            color

        Render.quad_points
            (start + 3.5f * stripe_width, bounds.Top)
            (start + 4.5f * stripe_width, bounds.Top)
            (start + 3.5f * stripe_width, bounds.Bottom)
            (start + 2.5f * stripe_width, bounds.Bottom)
            color

    let draw_stencilled (percent: float32) (bounds: Rect) (color: Color) : unit =
        if percent <= 0.0f || percent >= 1.0f then () else

        Render.stencil_create false
        Render.rect bounds Color.Transparent

        Render.stencil_begin_draw ()

        draw percent bounds color

        Render.stencil_finish()

    let spot_draw (x: float32, y: float32) (r1: float32) (r2: float32) (col: Color) (time: float32) : unit =
        let t = 4.0f * (time - time * time)
        let size = r1 * t
        let direction_deg = 65.0f - time * 10.0f
        let cos = MathF.Cos(direction_deg / 180.0f * MathF.PI)
        let sin = MathF.Sin(direction_deg / 180.0f * MathF.PI)

        let o = Vector2(x, y)
        let d = Vector2(sin, -cos)

        let cos = MathF.Cos((45.0f + direction_deg) / 180.0f * MathF.PI)
        let sin = MathF.Sin((45.0f + direction_deg) / 180.0f * MathF.PI)
        let p = Vector2(-sin * size, cos * size)
        let p2 = Vector2(-cos * size, -sin * size)

        let q = Quad.from_vectors(o, o - p, o + r2 * d, o + p2)

        let q2 =
            Quad.from_vectors(o, o - p, o + r2 * 0.6f * d, o + p2)
            |> Quad.rotate_about o 90.0

        let color = Color.FromArgb(Math.Clamp(t * 255.0f |> int, 0, 255), col)

        Render.quad q color
        Render.quad q2 color
        Render.quad (Quad.rotate_about o 180.0 q) color
        Render.quad (Quad.rotate_about o 180.0 q2) color

module Wedge =

    let draw (x: float32, y: float32) (r1: float32) (r2: float32) (a1: float) (a2: float) (col: Color) : unit =
        let segments = int ((a2 - a1) / 0.10) |> max 1
        let segsize = (a2 - a1) / float segments

        let centre = new Vector2(x, y)

        for i = 1 to segments do
            let a2 = a1 + float i * segsize
            let a1 = a1 + float (i - 1) * segsize
            let ang1 = Vector2(Math.Sin a1 |> float32, -Math.Cos a1 |> float32)
            let ang2 = Vector2(Math.Sin a2 |> float32, -Math.Cos a2 |> float32)

            Render.quad_vecs
                (centre + ang1 * r2)
                (centre + ang2 * r2)
                (centre + ang2 * r1)
                (centre + ang1 * r1)
                col

    let draw_centered = draw (Render.width() * 0.5f, Render.height() * 0.5f)

    let variant_1 (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) : unit =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = Math.Pow(pos, 0.5) * Math.PI * 2.0
        let tail = Math.Pow(pos, 2.0) * Math.PI * 2.0
        draw_centered r1 r2 tail head col

    let variant_2 (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) : unit =
        let pos = (amount - lo) / (hi - lo)
        let head = float (Math.Clamp(pos * 2.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        let tail = float (Math.Clamp(pos * 2.0f - 1.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        draw_centered r1 r2 tail head col

module Bubble =

    let draw (x: float32, y: float32) (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) : unit =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = float32 (Math.Pow(pos, 0.5)) * (r2 - r1) + r1
        let tail = float32 (Math.Pow(pos, 2.0)) * (r2 - r1) + r1
        Wedge.draw (x, y) tail head 0.0 (Math.PI * 2.0) col

module DiamondsWipe =

    let SCALE = 150.0f

    let draw (inbound: bool) (amount: float32) (bounds: Rect) : unit =
        let width = bounds.Width
        let height = bounds.Height

        let radius x =
            let f =
                Math.Clamp(
                    ((4.0f * SCALE + width) * (if inbound then amount else 1.0f - amount) - x)
                    / (4.0f * SCALE),
                    0.0f,
                    1.0f
                )

            if inbound then f * SCALE * 0.5f else (1.0f - f) * SCALE * 0.5f

        let draw_diamond (x: float32, y: float32) =
            let r = radius x

            Render.quad_points
                (x - r, y)
                (x, y - r)
                (x + r, y)
                (x, y + r)
                Color.Transparent

        for x in 0 .. (width / SCALE |> float |> Math.Ceiling |> int) do
            for y in 0 .. (height / SCALE |> float |> Math.Ceiling |> int) do
                draw_diamond (SCALE * float32 x, SCALE * float32 y)
                draw_diamond (0.5f * SCALE + SCALE * float32 x, 0.5f * SCALE + SCALE * float32 y)

module TriangleWipe =

    let draw_upward (inbound: bool) (amount: float32) (bounds: Rect) : unit =
        let height = bounds.Height
        let center = bounds.CenterX

        let y = if inbound then bounds.Bottom - amount * height * 2.0f else bounds.Bottom - (1.0f - amount) * height * 2.0f

        if inbound then

            Render.quad_points
                (bounds.Left, y + height * 2.0f)
                (bounds.Left, y + height)
                (center, y)
                (center, y + height * 2.0f)
                Color.Transparent

            Render.quad_points
                (bounds.Right, y + height * 2.0f)
                (bounds.Right, y + height)
                (center, y)
                (center, y + height * 2.0f)
                Color.Transparent

        else

            Render.quad_points
                (bounds.Left, y - height)
                (bounds.Left, y + height)
                (center, y)
                (center, y - height)
                Color.Transparent

            Render.quad_points
                (bounds.Right, y - height)
                (bounds.Right, y + height)
                (center, y)
                (center, y - height)
                Color.Transparent

    let draw_downward (inbound: bool) (amount: float32) (bounds: Rect) : unit =
        let height = bounds.Height
        let center = bounds.CenterX

        let y = if inbound then bounds.Top + amount * height * 2.0f else bounds.Top + (1.0f - amount) * height * 2.0f

        if inbound then

            Render.quad_points
                (bounds.Left, y - height * 2.0f)
                (bounds.Left, y - height)
                (center, y)
                (center, y - height * 2.0f)
                Color.Transparent

            Render.quad_points
                (bounds.Right, y - height * 2.0f)
                (bounds.Right, y - height)
                (center, y)
                (center, y - height * 2.0f)
                Color.Transparent

        else

            Render.quad_points
                (bounds.Left, y + height)
                (bounds.Left, y - height)
                (center, y)
                (center, y + height)
                Color.Transparent

            Render.quad_points
                (bounds.Right, y + height)
                (bounds.Right, y - height)
                (center, y)
                (center, y + height)
                Color.Transparent

module StripeWipe =

    let RECIPROCAL_GRADIENT = 0.35f

    let draw_left_to_right (left: float32) (right: float32) (bounds: Rect) (color: Color) : unit =
        let stripe_width = bounds.Height * RECIPROCAL_GRADIENT
        let travel_distance = bounds.Width + stripe_width * 2.0f

        let left = bounds.Left - stripe_width + travel_distance * left
        let right = bounds.Left - stripe_width + travel_distance * right

        Render.quad_points
            (left + stripe_width, bounds.Top)
            (right, bounds.Top)
            (right - stripe_width, bounds.Bottom)
            (left, bounds.Bottom)
            color

module LoadingAnimation =

    let draw_border_piece (bounds: Rect) (start: float32) (length: float32) (color: Color) : unit =
        let perimeter = (bounds.Width + bounds.Height) * 2.0f
        let a = start % 1.0f
        let b = a + length

        let corner_1 = bounds.Width / perimeter
        let corner_2 = (bounds.Width + bounds.Height) / perimeter
        let corner_3 = corner_1 + corner_2

        if b > 1.0f || a < corner_1 then
            Render.rect_edges
                (if b > 1.0f then
                        bounds.Left
                    else
                        bounds.Left + a * perimeter)
                bounds.Top
                (bounds.Left + (b % 1.0f) * perimeter |> min bounds.Right)
                (bounds.Top + Style.PADDING)
                color

        if b > corner_1 && a < corner_2 then
            Render.rect_edges
                (bounds.Right - Style.PADDING)
                (bounds.Top + (a - corner_1) * perimeter |> max bounds.Top)
                bounds.Right
                (bounds.Top + (b - corner_1) * perimeter |> min bounds.Bottom)
                color

        if b > corner_2 && a < corner_3 then
            Render.rect_edges
                (bounds.Right - (a - corner_2) * perimeter |> min bounds.Right)
                (bounds.Bottom - Style.PADDING)
                (bounds.Right - (b - corner_2) * perimeter |> max bounds.Left)
                bounds.Bottom
                color

        if b > corner_3 && a < 1.0f then
            Render.rect_edges
                bounds.Left
                (bounds.Bottom - (a - corner_3) * perimeter |> min bounds.Bottom)
                (bounds.Left + Style.PADDING)
                (bounds.Bottom - (b - corner_3) * perimeter |> max bounds.Top)
                color

    let draw_border (bounds: Rect) (offset: float32) (color: Color) : unit =
        draw_border_piece bounds offset 0.1f color
        draw_border_piece bounds (offset + 0.333f) 0.1f color
        draw_border_piece bounds (offset + 0.666f) 0.1f color