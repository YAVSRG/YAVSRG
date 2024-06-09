namespace Interlude.UI

open System
open OpenTK.Mathematics
open Prelude
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

module Beam =

    let private draw (direction_deg: float32) (x, y) (r1: float32) (r2: float32) (col: Color) (time: float32) =
        // angle is measured clockwise from vertical
        let cos = MathF.Cos(direction_deg / 180.0f * MathF.PI)
        let sin = MathF.Sin(direction_deg / 180.0f * MathF.PI)

        let o = Vector2(x, y)
        let d = Vector2(sin, -cos)
        let perp = Vector2(cos * 2.5f, sin * 2.5f)
        let startpoint = r1 + (r2 - r1) * (time * time)
        let endpoint = r1 + (r2 - r1) * (time * (2.0f - time))

        Draw.untextured_quad
            (Quad.create
                (o + d * startpoint + perp)
                (o + d * endpoint + perp)
                (o + d * endpoint - perp)
                (o + d * startpoint - perp))
            col.AsQuad

module Glint =

    let draw (percent: float32) (bounds: Rect) =
        if percent <= 0.0f || percent >= 1.0f then () else

        let stripe_width = bounds.Height * 0.35f
        let travel_distance = bounds.Width + stripe_width * 4.5f
        
        let start = bounds.Left - stripe_width * 4.5f + travel_distance * percent

        Stencil.start_stencilling false
        Draw.rect bounds Color.Transparent

        Stencil.start_drawing ()

        Draw.untextured_quad
        <| Quad.createv
            (start + stripe_width, bounds.Top)
            (start + stripe_width * 3.0f, bounds.Top)
            (start + stripe_width * 2.0f, bounds.Bottom)
            (start, bounds.Bottom)
        <| Colors.white.O4a(26).AsQuad
        
        Draw.untextured_quad
        <| Quad.createv
            (start + 3.5f * stripe_width, bounds.Top)
            (start + 4.5f * stripe_width, bounds.Top)
            (start + 3.5f * stripe_width, bounds.Bottom)
            (start + 2.5f * stripe_width, bounds.Bottom)
        <| Colors.white.O4a(26).AsQuad 
        Stencil.finish()

    let spot_draw (x, y) (r1: float32) (r2: float32) (col: Color) (time: float32) =
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

        let q = Quad.create o (o - p) (o + r2 * d) (o + p2)

        let q2 =
            Quad.create o (o - p) (o + r2 * 0.6f * d) (o + p2) |> Quad.rotate_about o 90.0

        let c = Color.FromArgb(Math.Clamp(t * 255.0f |> int, 0, 255), col)

        Draw.untextured_quad q c.AsQuad
        Draw.untextured_quad q2 c.AsQuad
        Draw.untextured_quad (Quad.rotate_about o 180.0 q) c.AsQuad
        Draw.untextured_quad (Quad.rotate_about o 180.0 q2) c.AsQuad

module private Wedge =

    let draw (centre: Vector2) (r1: float32) (r2: float32) (a1: float) (a2: float) (col: Color) =
        let segments = int ((a2 - a1) / 0.10)
        let segsize = (a2 - a1) / float segments

        for i = 1 to segments do
            let a2 = a1 + float i * segsize
            let a1 = a1 + float (i - 1) * segsize
            let ang1 = Vector2(Math.Sin a1 |> float32, -Math.Cos a1 |> float32)
            let ang2 = Vector2(Math.Sin a2 |> float32, -Math.Cos a2 |> float32)

            Draw.untextured_quad
                (Quad.create (centre + ang1 * r2) (centre + ang2 * r2) (centre + ang2 * r1) (centre + ang1 * r1))
                col.AsQuad

    let draw_centered =
        draw <| new Vector2(Viewport.vwidth * 0.5f, Viewport.vheight * 0.5f)

    let variant_1 (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = Math.Pow(pos, 0.5) * Math.PI * 2.0
        let tail = Math.Pow(pos, 2.0) * Math.PI * 2.0
        draw_centered r1 r2 tail head col

    let variant_2 (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = (amount - lo) / (hi - lo)
        let head = float (Math.Clamp(pos * 2.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        let tail = float (Math.Clamp(pos * 2.0f - 1.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        draw_centered r1 r2 tail head col

module Bubble =

    let draw (x, y) (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = float32 (Math.Pow(pos, 0.5)) * (r2 - r1) + r1
        let tail = float32 (Math.Pow(pos, 2.0)) * (r2 - r1) + r1
        Wedge.draw (new Vector2(x, y)) tail head 0.0 (Math.PI * 2.0) col

module DiamondsWipe =

    let draw inbound amount (bounds: Rect) =
        let SCALE = 150.0f
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

        let draw_diamond x y =
            let r = radius x

            Draw.untextured_quad
                (Quad.createv
                    (x - r, y)
                    (x, y - r)
                    (x + r, y)
                    (x, y + r)
                 )
                Color.Transparent.AsQuad


        for x in 0 .. (width / SCALE |> float |> Math.Ceiling |> int) do
            for y in 0 .. (height / SCALE |> float |> Math.Ceiling |> int) do
                draw_diamond (SCALE * float32 x) (SCALE * float32 y)
                draw_diamond (0.5f * SCALE + SCALE * float32 x) (0.5f * SCALE + SCALE * float32 y)

module TriangleWipe =

    let draw_upward (inbound: bool) (amount: float32) (bounds: Rect) =
        let height = bounds.Height
        let center = bounds.CenterX
        
        let y = if inbound then bounds.Bottom - amount * height * 2.0f else bounds.Bottom - (1.0f - amount) * height * 2.0f 

        if inbound then

            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Left, y + height * 2.0f)
                    (bounds.Left, y + height)
                    (center, y)
                    (center, y + height * 2.0f)
                )
                Color.Transparent.AsQuad
            
            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Right, y + height * 2.0f)
                    (bounds.Right, y + height)
                    (center, y)
                    (center, y + height * 2.0f)
                )
                Color.Transparent.AsQuad

        else

            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Left, y - height)
                    (bounds.Left, y + height)
                    (center, y)
                    (center, y - height)
                )
                Color.Transparent.AsQuad
            
            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Right, y - height)
                    (bounds.Right, y + height)
                    (center, y)
                    (center, y - height)
                )
                Color.Transparent.AsQuad
        
    let draw_downward (inbound: bool) (amount: float32) (bounds: Rect) =
        let height = bounds.Height
        let center = bounds.CenterX
        
        let y = if inbound then bounds.Top + amount * height * 2.0f else bounds.Top + (1.0f - amount) * height * 2.0f 

        if inbound then

            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Left, y - height * 2.0f)
                    (bounds.Left, y - height)
                    (center, y)
                    (center, y - height * 2.0f)
                )
                Color.Transparent.AsQuad
            
            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Right, y - height * 2.0f)
                    (bounds.Right, y - height)
                    (center, y)
                    (center, y - height * 2.0f)
                )
                Color.Transparent.AsQuad

        else

            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Left, y + height)
                    (bounds.Left, y - height)
                    (center, y)
                    (center, y + height)
                )
                Color.Transparent.AsQuad
            
            Draw.untextured_quad 
                (Quad.createv
                    (bounds.Right, y + height)
                    (bounds.Right, y - height)
                    (center, y)
                    (center, y + height)
                )
                Color.Transparent.AsQuad

module StripesWipe =

    let draw inbound amount (bounds: Rect) =
        let SCALE = 100.0f

        let number_of_stripes =
            MathF.Ceiling((Viewport.vwidth + Viewport.vheight) / SCALE) |> int

        let stripe_size (stripe_no: int) =
            let lo = float32 stripe_no / float32 number_of_stripes
            let region = float32 (number_of_stripes - stripe_no) / float32 number_of_stripes

            if inbound then
                Math.Clamp((amount - lo) / region, 0.0f, 1.0f)
            else
                1.0f - Math.Clamp(((1.0f - amount) - lo) / region, 0.0f, 1.0f)

        let draw_stripe n =
            let h = Viewport.vheight
            let f = stripe_size n
            let x = -h + float32 n * SCALE

            let v = new Vector2(h, -h) * f

            if n % 2 = 0 then
                Draw.untextured_quad
                    (Quad.create
                     <| new Vector2(x, h) + v
                     <| new Vector2(x + SCALE, h) + v
                     <| new Vector2(x + SCALE, h)
                     <| new Vector2(x, h))
                    Color.Transparent.AsQuad
            else
                Draw.untextured_quad
                    (Quad.create
                     <| new Vector2(x + h, 0.0f)
                     <| new Vector2(x + h + SCALE, 0.0f)
                     <| new Vector2(x + h + SCALE, 0.0f) - v
                     <| new Vector2(x + h, 0.0f) - v)
                    Color.Transparent.AsQuad

        for i = 0 to number_of_stripes - 1 do
            draw_stripe i