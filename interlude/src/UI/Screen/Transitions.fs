namespace Interlude.UI

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.UI
open OpenTK.Mathematics

module Transitions =

    let private DURATION = 500.0

    let private in_timer = Animation.Delay DURATION
    let private out_timer = Animation.Delay DURATION

    type Flags =
        | Default = 0
        | UnderLogo = 1

    let mutable flags = Flags.Default

    // drawing

    let private beam (direction_deg: float32) (x, y) (r1: float32) (r2: float32) (col: Color) (time: float32) =
        // angle is measured clockwise from vertical
        let cos = MathF.Cos(direction_deg / 180.0f * MathF.PI)
        let sin = MathF.Sin(direction_deg / 180.0f * MathF.PI)

        let o = Vector2(x, y)
        let d = Vector2(sin, -cos)
        let perp = Vector2(cos * 2.5f, sin * 2.5f)
        let startpoint = r1 + (r2 - r1) * (time * time)
        let endpoint = r1 + (r2 - r1) * (time * (2.0f - time))

        Draw.quad
            (Quad.create
                (o + d * startpoint + perp)
                (o + d * endpoint + perp)
                (o + d * endpoint - perp)
                (o + d * startpoint - perp))
            (Quad.color col)
            Sprite.DefaultQuad

    let private glint (x, y) (r1: float32) (r2: float32) (col: Color) (time: float32) =
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

        Draw.quad q (Quad.color c) Sprite.DefaultQuad
        Draw.quad q2 (Quad.color c) Sprite.DefaultQuad
        Draw.quad (Quad.rotate_about o 180.0 q) (Quad.color c) Sprite.DefaultQuad
        Draw.quad (Quad.rotate_about o 180.0 q2) (Quad.color c) Sprite.DefaultQuad

    let private wedge (centre: Vector2) (r1: float32) (r2: float32) (a1: float) (a2: float) (col: Color) =
        let segments = int ((a2 - a1) / 0.10)
        let segsize = (a2 - a1) / float segments

        for i = 1 to segments do
            let a2 = a1 + float i * segsize
            let a1 = a1 + float (i - 1) * segsize
            let ang1 = Vector2(Math.Sin a1 |> float32, -Math.Cos a1 |> float32)
            let ang2 = Vector2(Math.Sin a2 |> float32, -Math.Cos a2 |> float32)

            Draw.quad
                (Quad.create (centre + ang1 * r2) (centre + ang2 * r2) (centre + ang2 * r1) (centre + ang1 * r1))
                (Quad.color col)
                Sprite.DefaultQuad

    let private cwedge =
        wedge <| new Vector2(Viewport.vwidth * 0.5f, Viewport.vheight * 0.5f)

    let private bubble (x, y) (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = float32 (Math.Pow(pos, 0.5)) * (r2 - r1) + r1
        let tail = float32 (Math.Pow(pos, 2.0)) * (r2 - r1) + r1
        wedge (new Vector2(x, y)) tail head 0.0 (Math.PI * 2.0) col

    let private wedgeAnim (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = Math.Clamp((amount - lo) / (hi - lo), 0.0f, 1.0f) |> float
        let head = Math.Pow(pos, 0.5) * Math.PI * 2.0
        let tail = Math.Pow(pos, 2.0) * Math.PI * 2.0
        cwedge r1 r2 tail head col

    let private wedgeAnim2 (r1: float32) (r2: float32) (col: Color) (lo: float32) (hi: float32) (amount: float32) =
        let pos = (amount - lo) / (hi - lo)
        let head = float (Math.Clamp(pos * 2.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        let tail = float (Math.Clamp(pos * 2.0f - 1.0f, 0.0f, 1.0f)) * 2.0 * Math.PI
        cwedge r1 r2 tail head col

    let private fancyTransition inbound amount bounds =
        let amount = if inbound then amount else 2.0f - amount
        let a = int (255.0f * (1.0f - Math.Abs(amount - 1.0f)))
        wedgeAnim2 0.0f 1111.0f (Color.FromArgb(a, 0, 160, 255)) 0.0f 1.8f amount
        wedgeAnim2 0.0f 1111.0f (Color.FromArgb(a, 0, 200, 255)) 0.1f 1.9f amount
        wedgeAnim2 0.0f 1111.0f (Color.FromArgb(a, 0, 240, 255)) 0.2f 2.0f amount
        wedgeAnim 300.0f 500.0f Color.White 0.5f 1.5f amount
        bubble (400.0f, 200.0f) 100.0f 150.0f Color.White 1.2f 1.5f amount
        bubble (300.0f, 250.0f) 60.0f 90.0f Color.White 1.3f 1.6f amount
        bubble (1600.0f, 600.0f) 80.0f 120.0f Color.White 1.0f 1.3f amount
        bubble (1400.0f, 700.0f) 50.0f 75.0f Color.White 1.4f 1.7f amount

    let private diamondWipe inbound amount bounds =
        let s = 150.0f

        let size x =
            let f =
                Math.Clamp(
                    ((4.0f * s + Viewport.vwidth) * (if inbound then amount else 1.0f - amount) - x)
                    / (4.0f * s),
                    0.0f,
                    1.0f
                )

            if inbound then f * s * 0.5f else (1.0f - f) * s * 0.5f

        let diamond x y =
            let r = size x

            Draw.quad
                (Quad.create
                 <| new Vector2(x - r, y)
                 <| new Vector2(x, y - r)
                 <| new Vector2(x + r, y)
                 <| new Vector2(x, y + r))
                (Quad.color Color.Transparent)
                Sprite.DefaultQuad

        Stencil.start_stencilling false

        for x in 0 .. (Viewport.vwidth / s |> float |> Math.Ceiling |> int) do
            for y in 0 .. (Viewport.vheight / s |> float |> Math.Ceiling |> int) do
                diamond (s * float32 x) (s * float32 y)
                diamond (0.5f * s + s * float32 x) (0.5f * s + s * float32 y)

        Stencil.start_drawing ()
        Background.draw (bounds, Palette.color (255.0f * amount |> int, 1.0f, 0.0f), 1.0f)
        Stencil.finish ()

    let private stripes inbound amount bounds =
        let s = 100.0f

        let number_of_stripes =
            MathF.Ceiling((Viewport.vwidth + Viewport.vheight) / s) |> int

        let stripe_size (stripe_no: int) =
            let lo = float32 stripe_no / float32 number_of_stripes
            let region = float32 (number_of_stripes - stripe_no) / float32 number_of_stripes

            if inbound then
                Math.Clamp((amount - lo) / region, 0.0f, 1.0f)
            else
                1.0f - Math.Clamp(((1.0f - amount) - lo) / region, 0.0f, 1.0f)

        let stripe n =
            let h = Viewport.vheight
            let f = stripe_size n
            let x = -h + float32 n * s

            let v = new Vector2(h, -h) * f

            if n % 2 = 0 then
                Draw.quad
                    (Quad.create
                     <| new Vector2(x, h) + v
                     <| new Vector2(x + s, h) + v
                     <| new Vector2(x + s, h)
                     <| new Vector2(x, h))
                    (Quad.color Color.Transparent)
                    Sprite.DefaultQuad
            else
                Draw.quad
                    (Quad.create
                     <| new Vector2(x + h, 0.0f)
                     <| new Vector2(x + h + s, 0.0f)
                     <| new Vector2(x + h + s, 0.0f) - v
                     <| new Vector2(x + h, 0.0f) - v)
                    (Quad.color Color.Transparent)
                    Sprite.DefaultQuad

        Stencil.start_stencilling false

        for i = 0 to number_of_stripes - 1 do
            stripe i

        Stencil.start_drawing ()
        Background.draw (bounds, Palette.color (255.0f * amount |> int, 1.0f, 0.0f), 1.0f)
        Stencil.finish ()

    let private draw_internal flags inbound amount bounds = diamondWipe inbound amount bounds

    // setup

    let mutable active = false

    let draw (bounds: Rect) =
        if active then
            let inbound = in_timer.Elapsed < DURATION

            let amount =
                Math.Clamp(
                    (if inbound then
                         in_timer.Elapsed / DURATION
                     else
                         1.0 - (out_timer.Elapsed / DURATION)),
                    0.0,
                    1.0
                )
                |> float32

            draw_internal flags inbound amount bounds

    let animate (func: unit -> unit, _flags: Flags) : Animation =
        if active then
            failwith "Should not be called while a transition is already in progress"

        active <- true
        flags <- _flags

        Animation.seq
            [
                in_timer
                Animation.Action(fun () ->
                    func ()
                    out_timer.FrameSkip()
                )
                out_timer
                Animation.Action(fun () ->
                    in_timer.Reset()
                    out_timer.Reset()
                    active <- false
                )
            ]
