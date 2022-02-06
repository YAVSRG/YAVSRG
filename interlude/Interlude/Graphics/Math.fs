namespace Interlude.Graphics

open System
open System.Drawing
open OpenTK.Mathematics

(*
    Storage of rectangles as left, top, right, bottom
    Preferred over left, top, width, height for a handful of reasons
*)

type Rect = (struct(float32 * float32 * float32 * float32))

module Rect = 

    let create l t r b : Rect = struct(l, t, r, b)
    let createWH l t w h : Rect = struct(l, t, l + w, t + h)

    let width (struct (left, _, right, _): Rect) = right - left
    let height (struct (_, top, _, bottom): Rect) = bottom - top

    let centerX (struct (left, _, right, _): Rect) = (right + left) * 0.5f
    let centerY (struct (_, top, _, bottom): Rect) = (bottom + top) * 0.5f
    let center (r: Rect) = (centerX r, centerY r)
    let centerV (r: Rect) = new Vector2(centerX r, centerY r)

    let intersect (struct (l, t, r, b): Rect) (struct (left, top, right, bottom): Rect) : Rect =
        struct (Math.Max(l, left), Math.Max(t, top), Math.Min(r, right), Math.Min(b, bottom))

    let translate (x,y) (struct (left, top, right, bottom): Rect) : Rect =
        struct (left + x, top + y, right + x, bottom + y)

    let expand (x,y) (struct (left, top, right, bottom): Rect) : Rect =
        struct (left - x, top - y, right + x, bottom + y)

    let sliceLeft v (struct (left, top, _, bottom): Rect) : Rect =
        struct (left, top, left + v, bottom)

    let sliceTop v (struct (left, top, right, _): Rect) : Rect =
        struct (left, top, right, top + v)

    let sliceRight v (struct (_, top, right, bottom): Rect) : Rect =
        struct (right - v, top, right, bottom)

    let sliceBottom v (struct (left, _, right, bottom): Rect) : Rect =
        struct (left, bottom - v, right, bottom)

    let trimLeft v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left + v, top, right, bottom)

    let trimTop v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left, top + v, right, bottom)

    let trimRight v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left, top, right - v, bottom)

    let trimBottom v (struct (left, top, right, bottom): Rect) : Rect =
        struct (left, top, right, bottom - v)

    let zero = create 0.f 0.f 0.f 0.f
    let one = create 0.f 0.f 1.f 1.f

(*
    Simple storage of vertices to render as a quad
*)

type Quad = (struct(Vector2 * Vector2 * Vector2 * Vector2))
type QuadColors = (struct(Color * Color * Color * Color))

module Quad =

    let ofRect (struct (l, t, r, b): Rect) : Quad =
        struct (new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b))

    let parallelogram (amount: float32) (struct (l, t, r, b): Rect): Quad =
        let a = (b - t) * 0.5f * amount
        struct (new Vector2(l + a, t), new Vector2(r + a, t), new Vector2(r - a, b), new Vector2(l - a, b))

    let create c1 c2 c3 c4 : Quad = struct (c1, c2, c3, c4)
    let createv (c1x, c1y) (c2x, c2y) (c3x, c3y) (c4x, c4y) : Quad = struct (new Vector2(c1x, c1y), new Vector2(c2x, c2y), new Vector2(c3x, c3y), new Vector2(c4x, c4y))

    let colorOf c: QuadColors = struct (c, c, c, c)

    let flip (struct (c1, c2, c3, c4): Quad) : Quad = struct (c4, c3, c2, c1)

    let map f (struct (c1, c2, c3, c4): Quad) : Quad = struct (f c1, f c2, f c3, f c4)

    /// ang is in degrees, clockwise
    let rotateDeg ang (struct (c1, c2, c3, c4): Quad) : Quad =
        let centre = (c1 + c2 + c3 + c4) * 0.25f
        let mat = Matrix2.CreateRotation(-(float32(ang / 180.0 * Math.PI)))
        struct (c1, c2, c3, c4)
        |> map ((fun c -> c - centre) >> (fun c -> mat * c) >> (fun c -> c + centre))