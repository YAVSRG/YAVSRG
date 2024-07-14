﻿namespace Interlude.UI

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

    let COLOR = Colors.white.O4a(26)
    let RECIPROCAL_GRADIENT = 0.35f

    let draw (percent: float32) (bounds: Rect) (color: Color) =
        if percent <= 0.0f || percent >= 1.0f then () else

        let stripe_width = bounds.Height * RECIPROCAL_GRADIENT
        let travel_distance = bounds.Width + stripe_width * 4.5f
        
        let start = bounds.Left - stripe_width * 4.5f + travel_distance * percent

        Draw.untextured_quad
        <| Quad.createv
            (start + stripe_width, bounds.Top)
            (start + stripe_width * 3.0f, bounds.Top)
            (start + stripe_width * 2.0f, bounds.Bottom)
            (start, bounds.Bottom)
        <| color.AsQuad
        
        Draw.untextured_quad
        <| Quad.createv
            (start + 3.5f * stripe_width, bounds.Top)
            (start + 4.5f * stripe_width, bounds.Top)
            (start + 3.5f * stripe_width, bounds.Bottom)
            (start + 2.5f * stripe_width, bounds.Bottom)
        <| color.AsQuad

    let draw_stencilled (percent: float32) (bounds: Rect) (color: Color) =
        if percent <= 0.0f || percent >= 1.0f then () else

        Stencil.start_stencilling false
        Draw.rect bounds Color.Transparent

        Stencil.start_drawing ()

        draw percent bounds color

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

    let SCALE = 150.0f

    let draw (inbound: bool) (amount: float32) (bounds: Rect) =
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

module StripeWipe =
        
    let RECIPROCAL_GRADIENT = 0.35f

    let draw_left_to_right (left: float32) (right: float32) (bounds: Rect) (color: Color) =
        let stripe_width = bounds.Height * RECIPROCAL_GRADIENT
        let travel_distance = bounds.Width + stripe_width * 2.0f
        
        let left = bounds.Left - stripe_width + travel_distance * left
        let right = bounds.Left - stripe_width + travel_distance * right

        Draw.untextured_quad
            (Quad.createv
                (left + stripe_width, bounds.Top)
                (right, bounds.Top)
                (right - stripe_width, bounds.Bottom)
                (left, bounds.Bottom)
            )
            color.AsQuad

module LoadingAnimation =

    let private draw_border_piece (bounds: Rect) (a: float32) (length: float32) (color: Color) =
        let perimeter = (bounds.Width + bounds.Height) * 2.0f
        let a = a % 1.0f
        let b = a + length

        let corner_1 = bounds.Width / perimeter
        let corner_2 = (bounds.Width + bounds.Height) / perimeter
        let corner_3 = corner_1 + corner_2

        if b > 1.0f || a < corner_1 then
            Draw.rect
                (Rect.Create(
                    (if b > 1.0f then
                            bounds.Left
                        else
                            bounds.Left + a * perimeter),
                    bounds.Top,
                    bounds.Left + (b % 1.0f) * perimeter |> min bounds.Right,
                    bounds.Top + Style.PADDING
                ))
                color

        if b > corner_1 && a < corner_2 then
            Draw.rect
                (Rect.Create(
                    bounds.Right - Style.PADDING,
                    bounds.Top + (a - corner_1) * perimeter |> max bounds.Top,
                    bounds.Right,
                    bounds.Top + (b - corner_1) * perimeter |> min bounds.Bottom
                ))
                color

        if b > corner_2 && a < corner_3 then
            Draw.rect
                (Rect.Create(
                    bounds.Right - (a - corner_2) * perimeter |> min bounds.Right,
                    bounds.Bottom - Style.PADDING,
                    bounds.Right - (b - corner_2) * perimeter |> max bounds.Left,
                    bounds.Bottom
                ))
                color

        if b > corner_3 && a < 1.0f then
            Draw.rect
                (Rect.Create(
                    bounds.Left,
                    bounds.Bottom - (a - corner_3) * perimeter |> min bounds.Bottom,
                    bounds.Left + Style.PADDING,
                    bounds.Bottom - (b - corner_3) * perimeter |> max bounds.Top
                ))
                color

    let draw_border (bounds: Rect) (a: float32) (color: Color) =
        draw_border_piece bounds a 0.1f color
        draw_border_piece bounds (a + 0.333f) 0.1f color
        draw_border_piece bounds (a + 0.666f) 0.1f color
    
    let draw_border_more (bounds: Rect) (a: float32) (color: Color) =
        draw_border_piece bounds a 0.04f color
        draw_border_piece bounds (a + 0.1f) 0.04f color
        draw_border_piece bounds (a + 0.2f) 0.04f color
        draw_border_piece bounds (a + 0.3f) 0.04f color
        draw_border_piece bounds (a + 0.4f) 0.04f color
        draw_border_piece bounds (a + 0.5f) 0.04f color
        draw_border_piece bounds (a + 0.6f) 0.04f color
        draw_border_piece bounds (a + 0.7f) 0.04f color
        draw_border_piece bounds (a + 0.8f) 0.04f color
        draw_border_piece bounds (a + 0.9f) 0.04f color