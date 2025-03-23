namespace Percyqaz.Flux.Graphics

open System
open System.Drawing
open OpenTK.Mathematics

// I find it this definition of a rect easier to work with than {left, top, width, height}
// (0.0, 0.0) = top left; +X = towards right; +Y = towards bottom
[<Struct>]
type Rect =
    {
        Left: float32
        Top: float32
        Right: float32
        Bottom: float32
    }

    static member FromEdges(left: float32, top: float32, right: float32, bottom: float32) : Rect =
        {
            Left = left
            Top = top
            Right = right
            Bottom = bottom
        }

    static member FromSize(left: float32, top: float32, width: float32, height: float32) : Rect =
        {
            Left = left
            Top = top
            Right = left + width
            Bottom = top + height
        }

    static member Zero : Rect = Rect.FromSize(0.0f, 0.0f, 0.0f, 0.0f)

    member inline this.Width : float32 = this.Right - this.Left
    member inline this.Height : float32 = this.Bottom - this.Top

    member inline this.CenterX : float32 = (this.Left + this.Right) * 0.5f
    member inline this.CenterY : float32 = (this.Top + this.Bottom) * 0.5f
    member inline this.Center : float32 * float32 = (this.CenterX, this.CenterY)

    member inline this.Visible : bool = this.Left < this.Right && this.Top < this.Bottom

    member inline this.Contains(x: float32, y: float32) : bool =
        x > this.Left && x < this.Right && y > this.Top && y < this.Bottom

    /// Returns a Rect containing the overlapping area shared by `this` and `other`
    member inline this.IntersectWith(other: Rect) : Rect =
        {
            Left = max this.Left other.Left
            Top = max this.Top other.Top
            Right = min this.Right other.Right
            Bottom = min this.Bottom other.Bottom
        }

    // -- Various transform utilities --
    // 'Percent' in the operation name = The operation but as a multiplier of the size

    // Translate: Standard translate operation
    // +Y = lower on screen
    member inline this.TranslateX(x: float32) : Rect =
        {
            Left = this.Left + x
            Top = this.Top
            Right = this.Right + x
            Bottom = this.Bottom
        }
    member inline this.TranslateY(y: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top + y
            Right = this.Right
            Bottom = this.Bottom + y
        }
    member inline this.Translate(x: float32, y: float32) : Rect =
        {
            Left = this.Left + x
            Top = this.Top + y
            Right = this.Right + x
            Bottom = this.Bottom + y
        }

    // Expand: Enlarge by the given amount in all directions
    member inline this.ExpandL(amount: float32) : Rect =
        {
            Left = this.Left - amount
            Top = this.Top
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.ExpandT(amount: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top - amount
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.ExpandR(amount: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Right + amount
            Bottom = this.Bottom
        }
    member inline this.ExpandB(amount: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Right
            Bottom = this.Bottom + amount
        }
    member inline this.ExpandX(amount: float32) : Rect =
        {
            Left = this.Left - amount
            Top = this.Top
            Right = this.Right + amount
            Bottom = this.Bottom
        }
    member inline this.ExpandY(amount: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top - amount
            Right = this.Right
            Bottom = this.Bottom + amount
        }
    member inline this.Expand(x: float32, y: float32) : Rect = this.ExpandX(x).ExpandY(y)
    member inline this.Expand(amount: float32) : Rect = this.Expand(amount, amount)

    member inline this.ExpandPercentL(percent: float32) : Rect = this.ExpandL (this.Width * percent)
    member inline this.ExpandPercentT(percent: float32) : Rect = this.ExpandT (this.Height * percent)
    member inline this.ExpandPercentR(percent: float32) : Rect = this.ExpandR (this.Width * percent)
    member inline this.ExpandPercentB(percent: float32) : Rect = this.ExpandB (this.Height * percent)
    member inline this.ExpandPercentX(percent: float32) : Rect = this.ExpandX (this.Width * percent)
    member inline this.ExpandPercentY(percent: float32) : Rect = this.ExpandY (this.Height * percent)
    member inline this.ExpandPercent(percent_x: float32, percent_y: float32) : Rect = this.Expand (this.Width * percent_x, this.Height * percent_y)
    member inline this.ExpandPercent(percent: float32) : Rect = this.Expand (this.Width * percent, this.Height * percent)

    // Shrink: Opposite of expand
    member inline this.ShrinkL(amount: float32) : Rect = this.ExpandL -amount
    member inline this.ShrinkT(amount: float32) : Rect = this.ExpandT -amount
    member inline this.ShrinkR(amount: float32) : Rect = this.ExpandR -amount
    member inline this.ShrinkB(amount: float32) : Rect = this.ExpandB -amount
    member inline this.ShrinkX(amount: float32) : Rect = this.ExpandX -amount
    member inline this.ShrinkY(amount: float32) : Rect = this.ExpandY -amount
    member inline this.Shrink(x: float32, y: float32) : Rect = this.Expand(-x, -y)
    member inline this.Shrink(amount: float32) : Rect = this.Shrink(amount, amount)

    member inline this.ShrinkPercentL(percent: float32) : Rect = this.ShrinkL (this.Width * percent)
    member inline this.ShrinkPercentT(percent: float32) : Rect = this.ShrinkT (this.Height * percent)
    member inline this.ShrinkPercentR(percent: float32) : Rect = this.ShrinkR (this.Width * percent)
    member inline this.ShrinkPercentB(percent: float32) : Rect = this.ShrinkB (this.Height * percent)
    member inline this.ShrinkPercentX(percent: float32) : Rect = this.ShrinkX (this.Width * percent)
    member inline this.ShrinkPercentY(percent: float32) : Rect = this.ShrinkY (this.Height * percent)
    member inline this.ShrinkPercent(percent_x: float32, percent_y: float32) : Rect = this.Shrink (this.Width * percent_x, this.Height * percent_y)
    member inline this.ShrinkPercent(percent: float32) : Rect = this.Shrink (this.Width * percent, this.Height * percent)

    // Slice: Gets a strip of a certain thickness, along an edge of the box
    // e.g. SliceL(50.0f) takes a 50px vertical strip from the left side
    // e.g. SliceL(100.0f, 50.0f) takes a 50px vertical strip from the left side but inset 100px from the edge
    member inline this.SliceL(thickness: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Left + thickness
            Bottom = this.Bottom
        }
    member inline this.SliceL(start: float32, thickness: float32) : Rect =
        {
            Left = this.Left + start
            Top = this.Top
            Right = this.Left + start + thickness
            Bottom = this.Bottom
        }
    member inline this.SliceT(thickness: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Right
            Bottom = this.Top + thickness
        }
    member inline this.SliceT(start: float32, thickness: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top + start
            Right = this.Right
            Bottom = this.Top + start + thickness
        }
    member inline this.SliceR(thickness: float32) : Rect =
        {
            Left = this.Right - thickness
            Top = this.Top
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.SliceR(start: float32, thickness: float32) : Rect =
        {
            Left = this.Right - start - thickness
            Top = this.Top
            Right = this.Right - start
            Bottom = this.Bottom
        }
    member inline this.SliceB(thickness: float32) : Rect =
        {
            Left = this.Left
            Top = this.Bottom - thickness
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.SliceB(start: float32, thickness: float32) : Rect =
        {
            Left = this.Left
            Top = this.Bottom - start - thickness
            Right = this.Right
            Bottom = this.Bottom - start
        }

    /// Gets a `thickness` width strip taken from the horizontal center of the box
    member inline this.SliceX(thickness: float32) : Rect =
        let center_x = this.CenterX
        {
            Left = center_x - 0.5f * thickness
            Top = this.Top
            Right = center_x + 0.5f * thickness
            Bottom = this.Bottom
        }
    /// Gets a `thickness` height strip taken from the vertical center of the box
    member inline this.SliceY(thickness: float32) : Rect =
        let center_y = this.CenterY
        {
            Left = this.Left
            Top = center_y - 0.5f * thickness
            Right = this.Right
            Bottom = center_y + 0.5f * thickness
        }

    // e.g. SlicePercentL(0.5f) returns the left half of a given box
    member inline this.SlicePercentL(percent: float32) : Rect = this.SliceL (this.Width * percent)
    member inline this.SlicePercentL(start: float32, thickness: float32) : Rect = this.ShrinkPercentL(start).SliceL(this.Width * thickness)
    member inline this.SlicePercentT(percent: float32) : Rect = this.SliceT (this.Height * percent)
    member inline this.SlicePercentT(start: float32, thickness: float32) : Rect = this.ShrinkPercentT(start).SliceT(this.Height * thickness)
    member inline this.SlicePercentR(percent: float32) : Rect = this.SliceR (this.Width * percent)
    member inline this.StripPercentR(start: float32, thickness: float32) : Rect = this.ShrinkPercentR(start).SliceR(this.Width * thickness)
    member inline this.SlicePercentB(percent: float32) : Rect = this.SliceB (this.Height * percent)
    member inline this.SlicePercentB(start: float32, thickness: float32) : Rect = this.ShrinkPercentB(start).SliceB(this.Height * thickness)

    member inline this.SlicePercentX(percent: float32) : Rect = this.SliceX (this.Width * percent)
    member inline this.SlicePercentY(percent: float32) : Rect = this.SliceY (this.Height * percent)

    // Border: Gets a strip of a certain thickness, along the edge of the box, but on the outside
    member inline this.BorderL(thickness: float32) : Rect =
        {
            Left = this.Left - thickness
            Top = this.Top
            Right = this.Left
            Bottom = this.Bottom
        }
    member inline this.BorderT(thickness: float32) : Rect =
        {
            Left = this.Left
            Top = this.Top - thickness
            Right = this.Right
            Bottom = this.Top
        }
    member inline this.BorderR(thickness: float32) : Rect =
        {
            Left = this.Right
            Top = this.Top
            Right = this.Right + thickness
            Bottom = this.Bottom
        }
    member inline this.BorderB(thickness: float32) : Rect =
        {
            Left = this.Left
            Top = this.Bottom
            Right = this.Right
            Bottom = this.Bottom + thickness
        }
    member inline this.BorderCornersT(thickness: float32) : Rect =
        {
            Left = this.Left - thickness
            Top = this.Top - thickness
            Right = this.Right + thickness
            Bottom = this.Top
        }
    member inline this.BorderCornersB(thickness: float32) : Rect =
        {
            Left = this.Left - thickness
            Top = this.Bottom
            Right = this.Right + thickness
            Bottom = this.Bottom + thickness
        }

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

    let inline from_vectors (top_left: Vector2, top_right: Vector2, bottom_right: Vector2, bottom_left: Vector2) : Quad =
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
            TopLeft = new Vector2(r.Left + a, r.Top)
            TopRight = new Vector2(r.Right + a, r.Top)
            BottomRight =  new Vector2(r.Right - a, r.Bottom)
            BottomLeft = new Vector2(r.Left - a, r.Bottom)
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
        let v = new Vector2(x, y)
        map ((+) v)

    let inline rotate_about (origin: Vector2) (degrees: float) : Quad -> Quad =
        let mat = Matrix2.CreateRotation(-(float32 (degrees / 180.0 * Math.PI)))
        map ((fun c -> c - origin) >> (fun c -> mat * c) >> (fun c -> c + origin))

    let inline rotate (degrees: float) (q: Quad) : Quad =
        let center = (q.TopLeft + q.TopRight + q.BottomRight + q.BottomLeft) * 0.25f
        rotate_about center degrees q

    let inline gradient_left_to_right (left: Color) (right: Color) : QuadColors = { TopLeft = left; TopRight = right; BottomRight = right; BottomLeft = left }
    let inline gradient_top_to_bottom (top: Color) (bottom: Color) : QuadColors = { TopLeft = top; TopRight = top; BottomRight = bottom; BottomLeft = bottom }
    let inline from_color (color: Color) : QuadColors = { TopLeft = color; TopRight = color; BottomRight = color; BottomLeft = color }

[<AutoOpen>]
module AsQuadExtensions =

    type Color with
        member inline this.AsQuad : QuadColors = Quad.from_color this

    type Rect with
        member inline this.AsQuad : Quad = Quad.from_rect this