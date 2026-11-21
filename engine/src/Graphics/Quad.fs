namespace Percyqaz.Flux.Graphics

open System
open System.Drawing
open OpenTK.Mathematics

/// Represents a 'Quad', a quadrilateral ready to render to the screen
/// All Rects are converted to a Quad right before rendering
/// Quads allow arbitrary corner positions so you can draw non-rectangles by creating them directly
///   The clockwise order of the corners (starting with TopLeft) is just the order they get sent to the GPU
///   It doesn't matter which corner comes first, but the names reflect how a Rect is made into a Quad
[<Struct>]
type Quad =
    {
        TopLeft: Vector2
        TopRight: Vector2
        BottomRight: Vector2
        BottomLeft: Vector2
    }

/// Represents the colors each corner of a 'Quad' should be rendered with
/// Sent along with a Quad during render calls
[<Struct>]
type QuadColors =
    {
        TopLeft: Color
        TopRight: Color
        BottomRight: Color
        BottomLeft: Color
    }

module Quad =

    let inline from_vectors
        (top_left: Vector2, top_right: Vector2, bottom_right: Vector2, bottom_left: Vector2)
        : Quad =
        {
            TopLeft = top_left
            TopRight = top_right
            BottomRight = bottom_right
            BottomLeft = bottom_left
        }

    let inline from_points
        (
            top_left: float32 * float32,
            top_right: float32 * float32,
            bottom_right: float32 * float32,
            bottom_left: float32 * float32
        ) : Quad =

        let tl_x, tl_y = top_left
        let tr_x, tr_y = top_right
        let br_x, br_y = bottom_right
        let bl_x, bl_y = bottom_left

        {
            TopLeft = Vector2(tl_x, tl_y)
            TopRight = Vector2(tr_x, tr_y)
            BottomRight = Vector2(br_x, br_y)
            BottomLeft = Vector2(bl_x, bl_y)
        }

    let inline from_rect (r: Rect) : Quad =
        {
            TopLeft = Vector2(r.Left, r.Top)
            TopRight = Vector2(r.Right, r.Top)
            BottomRight = Vector2(r.Right, r.Bottom)
            BottomLeft = Vector2(r.Left, r.Bottom)
        }

    let inline parallelogram (lean_percentage: float32) (r: Rect) : Quad =
        let a = r.Height * 0.5f * lean_percentage

        {
            TopLeft = Vector2(r.Left + a, r.Top)
            TopRight = Vector2(r.Right + a, r.Top)
            BottomRight = Vector2(r.Right - a, r.Bottom)
            BottomLeft = Vector2(r.Left - a, r.Bottom)
        }

    let inline flip_vertical (q: Quad) : Quad =
        {
            TopLeft = q.BottomLeft
            TopRight = q.BottomRight
            BottomRight = q.TopRight
            BottomLeft = q.TopLeft
        }

    let inline map (corner_transform: Vector2 -> Vector2) (q: Quad) : Quad =
        {
            TopLeft = corner_transform q.TopLeft
            TopRight = corner_transform q.TopRight
            BottomRight = corner_transform q.BottomRight
            BottomLeft = corner_transform q.BottomLeft
        }

    let inline translate (x: float32, y: float32) : Quad -> Quad =
        let v = Vector2(x, y)
        map((+) v)

    let inline rotate_about (origin: Vector2) (degrees: float) : Quad -> Quad =
        let mat = Matrix2.CreateRotation(-(float32(degrees / 180.0 * Math.PI)))
        map((fun c -> c - origin) >> (fun c -> mat * c) >> (fun c -> c + origin))

    let inline rotate (degrees: float) (q: Quad) : Quad =
        let center = (q.TopLeft + q.TopRight + q.BottomRight + q.BottomLeft) * 0.25f
        rotate_about center degrees q

    let inline gradient_left_to_right (left: Color) (right: Color) : QuadColors =
        {
            TopLeft = left
            TopRight = right
            BottomRight = right
            BottomLeft = left
        }

    let inline gradient_top_to_bottom (top: Color) (bottom: Color) : QuadColors =
        {
            TopLeft = top
            TopRight = top
            BottomRight = bottom
            BottomLeft = bottom
        }

    let inline from_color (color: Color) : QuadColors =
        {
            TopLeft = color
            TopRight = color
            BottomRight = color
            BottomLeft = color
        }

[<AutoOpen>]
module QuadExtensions =

    type Color with
        member inline this.AsQuad: QuadColors = Quad.from_color this

    type Rect with
        member inline this.AsQuad: Quad = Quad.from_rect this
