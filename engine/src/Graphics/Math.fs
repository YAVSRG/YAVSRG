namespace Percyqaz.Flux.Graphics

open System
open System.Drawing
open OpenTK.Mathematics

(*
    Storage of rectangles as left, top, right, bottom
    Preferred over left, top, width, height for a handful of reasons
*)

[<Struct>]
type Rect =
    {
        Left: float32
        Top: float32
        Right: float32
        Bottom: float32
    }

    static member Create(l, t, r, b) =
        {
            Left = l
            Top = t
            Right = r
            Bottom = b
        }

    static member Box(l, t, w, h) =
        {
            Left = l
            Top = t
            Right = l + w
            Bottom = t + h
        }

    member inline this.Width = this.Right - this.Left
    member inline this.Height = this.Bottom - this.Top

    member inline this.CenterX = (this.Left + this.Right) * 0.5f
    member inline this.CenterY = (this.Top + this.Bottom) * 0.5f
    member inline this.Center = (this.CenterX, this.CenterY)

    member inline this.Visible = this.Left < this.Right && this.Top < this.Bottom

    member inline this.Contains(x, y) =
        x > this.Left && x < this.Right && y > this.Top && y < this.Bottom

    member inline this.Intersect(other: Rect) =
        {
            Left = max this.Left other.Left
            Top = max this.Top other.Top
            Right = min this.Right other.Right
            Bottom = min this.Bottom other.Bottom
        }

    // -- Various transform utilities --

    // Translate
    member inline this.TranslateX x =
        {
            Left = this.Left + x
            Top = this.Top
            Right = this.Right + x
            Bottom = this.Bottom
        }
    member inline this.TranslateY y =
        {
            Left = this.Left
            Top = this.Top + y
            Right = this.Right
            Bottom = this.Bottom + y
        }
    member inline this.Translate (x, y) =
        {
            Left = this.Left + x
            Top = this.Top + y
            Right = this.Right + x
            Bottom = this.Bottom + y
        }

    // Expand: Enlarge by the given amount in all directions
    member inline this.ExpandL amount =
        {
            Left = this.Left - amount
            Top = this.Top
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.ExpandT amount =
        {
            Left = this.Left
            Top = this.Top - amount
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.ExpandR amount =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Right + amount
            Bottom = this.Bottom
        }
    member inline this.ExpandB amount =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Right
            Bottom = this.Bottom + amount
        }
    member inline this.ExpandX amount =
        {
            Left = this.Left - amount
            Top = this.Top
            Right = this.Right + amount
            Bottom = this.Bottom
        }
    member inline this.ExpandY amount =
        {
            Left = this.Left
            Top = this.Top - amount
            Right = this.Right
            Bottom = this.Bottom + amount
        }
    member inline this.Expand(x, y) =
        {
            Left = this.Left - x
            Top = this.Top - y
            Right = this.Right + x
            Bottom = this.Bottom + y
        }
    member inline this.Expand amount = this.Expand(amount, amount)

    member inline this.ExpandPercentL percent = this.ExpandL (this.Width * percent)
    member inline this.ExpandPercentT percent = this.ExpandT (this.Height * percent)
    member inline this.ExpandPercentR percent = this.ExpandR (this.Width * percent)
    member inline this.ExpandPercentB percent = this.ExpandB (this.Height * percent)
    member inline this.ExpandPercentX percent = this.ExpandX (this.Width * percent)
    member inline this.ExpandPercentY percent = this.ExpandY (this.Height * percent)
    member inline this.ExpandPercent (percent_x, percent_y) = this.Expand (this.Width * percent_x, this.Height * percent_y)
    member inline this.ExpandPercent percent = this.Expand (this.Width * percent, this.Height * percent)

    // Shrink: Opposite of expand
    member inline this.ShrinkL amount = this.ExpandL -amount
    member inline this.ShrinkT amount = this.ExpandT -amount
    member inline this.ShrinkR amount = this.ExpandR -amount
    member inline this.ShrinkB amount = this.ExpandB -amount
    member inline this.ShrinkX x = this.ExpandX -x
    member inline this.ShrinkY x = this.ExpandY -x
    member inline this.Shrink(x, y) = this.Expand(-x, -y)
    member inline this.Shrink amount = this.Shrink(amount, amount)

    member inline this.ShrinkPercentL percent = this.ShrinkL (this.Width * percent)
    member inline this.ShrinkPercentT percent = this.ShrinkT (this.Height * percent)
    member inline this.ShrinkPercentR percent = this.ShrinkR (this.Width * percent)
    member inline this.ShrinkPercentB percent = this.ShrinkB (this.Height * percent)
    member inline this.ShrinkPercentX percent = this.ShrinkX (this.Width * percent)
    member inline this.ShrinkPercentY percent = this.ShrinkY (this.Height * percent)
    member inline this.ShrinkPercent (percent_x, percent_y) = this.Shrink (this.Width * percent_x, this.Height * percent_y)
    member inline this.ShrinkPercent percent = this.Shrink (this.Width * percent, this.Height * percent)

    // Slice: Gets a strip of a certain thickness, inside an edge of the box
    member inline this.SliceL thickness =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Left + thickness
            Bottom = this.Bottom
        }
    member inline this.SliceL (start, thickness) =
        {
            Left = this.Left + start
            Top = this.Top
            Right = this.Left + start + thickness
            Bottom = this.Bottom
        }
    member inline this.SliceT thickness =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Right
            Bottom = this.Top + thickness
        }
    member inline this.SliceT (start, thickness) =
        {
            Left = this.Left
            Top = this.Top + start
            Right = this.Right
            Bottom = this.Top + start + thickness
        }
    member inline this.SliceR thickness =
        {
            Left = this.Right - thickness
            Top = this.Top
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.SliceR (start, thickness) =
        {
            Left = this.Right - start - thickness
            Top = this.Top
            Right = this.Right - start
            Bottom = this.Bottom
        }
    member inline this.SliceB thickness =
        {
            Left = this.Left
            Top = this.Bottom - thickness
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.SliceB (start, thickness) =
        {
            Left = this.Left
            Top = this.Bottom - start - thickness
            Right = this.Right
            Bottom = this.Bottom - start
        }
    member inline this.SliceX thickness =
        let center_x = this.CenterX
        {
            Left = center_x - 0.5f * thickness
            Top = this.Top
            Right = center_x + 0.5f * thickness
            Bottom = this.Bottom
        }
    member inline this.SliceY amount =
        let center_y = this.CenterY
        {
            Left = this.Left
            Top = center_y - 0.5f * amount
            Right = this.Right
            Bottom = center_y + 0.5f * amount
        }

    member inline this.SlicePercentL percent = this.SliceL (this.Width * percent)
    member inline this.SlicePercentL (start, thickness) = this.ShrinkPercentL(start).SliceL(this.Width * thickness)
    member inline this.SlicePercentT percent = this.SliceT (this.Height * percent)
    member inline this.SlicePercentT (start, thickness) = this.ShrinkPercentT(start).SliceT(this.Height * thickness)
    member inline this.SlicePercentR percent = this.SliceR (this.Width * percent)
    member inline this.StripPercentR (start, thickness) = this.ShrinkPercentR(start).SliceR(this.Width * thickness)
    member inline this.SlicePercentB percent = this.SliceB (this.Height * percent)
    member inline this.SlicePercentB (start, thickness) = this.ShrinkPercentB(start).SliceB(this.Height * thickness)
    member inline this.SlicePercentX percent = this.SliceX (this.Width * percent)
    member inline this.SlicePercentY percent = this.SliceY (this.Height * percent)

    // Border: Gets a strip of a certain thickness, outside an edge of the box
    member inline this.BorderL amount =
        {
            Left = this.Left - amount
            Top = this.Top
            Right = this.Left
            Bottom = this.Bottom
        }
    member inline this.BorderT amount =
        {
            Left = this.Left
            Top = this.Top - amount
            Right = this.Right
            Bottom = this.Top
        }
    member inline this.BorderR amount =
        {
            Left = this.Right
            Top = this.Top
            Right = this.Right + amount
            Bottom = this.Bottom
        }
    member inline this.BorderB amount =
        {
            Left = this.Left
            Top = this.Bottom
            Right = this.Right
            Bottom = this.Bottom + amount
        }
    member inline this.BorderCornersT amount =
        {
            Left = this.Left - amount
            Top = this.Top - amount
            Right = this.Right + amount
            Bottom = this.Top
        }
    member inline this.BorderCornersB amount =
        {
            Left = this.Left - amount
            Top = this.Bottom
            Right = this.Right + amount
            Bottom = this.Bottom + amount
        }

module Rect =

    let ZERO = Rect.Box(0.0f, 0.0f, 0.0f, 0.0f)
    let ONE = Rect.Box(0.0f, 0.0f, 1.0f, 1.0f)

(*
    Simple storage of vertices to render as a quad
*)

[<Struct>]
type Quad = { TopLeft: Vector2; TopRight: Vector2; BottomRight: Vector2; BottomLeft: Vector2 }
[<Struct>]
type QuadColors = { TopLeft: Color; TopRight: Color; BottomRight: Color; BottomLeft: Color }

module Quad =

    let inline parallelogram (amount: float32) (r: Rect) : Quad =
        let a = r.Height * 0.5f * amount

        {
            TopLeft = new Vector2(r.Left + a, r.Top)
            TopRight = new Vector2(r.Right + a, r.Top)
            BottomRight =  new Vector2(r.Right - a, r.Bottom)
            BottomLeft = new Vector2(r.Left - a, r.Bottom)
        }

    let inline create a b c d : Quad = { TopLeft = a; TopRight = b; BottomRight = c; BottomLeft = d }

    let inline createv (ax, ay) (bx, by) (cx, cy) (dx, dy) : Quad =
        create (new Vector2(ax, ay)) (new Vector2(bx, by)) (new Vector2(cx, cy)) (new Vector2(dx, dy))

    let inline gradient_left_to_right (left: Color) (right: Color) : QuadColors = { TopLeft = left; TopRight = right; BottomRight = right; BottomLeft = left }
    let inline gradient_top_to_bottom (top: Color) (bottom: Color) : QuadColors = { TopLeft = top; TopRight = top; BottomRight = bottom; BottomLeft = bottom }

    let inline flip_vertical (q: Quad) : Quad = { TopLeft = q.BottomLeft; TopRight = q.BottomRight; BottomRight = q.TopRight; BottomLeft = q.TopLeft }

    let inline map f (q: Quad) : Quad = { TopLeft = f q.TopLeft; TopRight = f q.TopRight; BottomRight = f q.BottomRight; BottomLeft = f q.BottomLeft }

    let inline translate (x, y) : Quad -> Quad =
        let v = new Vector2(x, y)
        map ((+) v)

    let inline rotate_about origin degrees : Quad -> Quad =
        let mat = Matrix2.CreateRotation(-(float32 (degrees / 180.0 * Math.PI)))
        map ((fun c -> c - origin) >> (fun c -> mat * c) >> (fun c -> c + origin))

    let inline rotate degrees (q: Quad) : Quad =
        let center = (q.TopLeft + q.TopRight + q.BottomRight + q.BottomLeft) * 0.25f
        rotate_about center degrees q

[<AutoOpen>]
module AsQuadExtensions =

    type Color with
        member inline this.AsQuad : QuadColors = { TopLeft = this; TopRight = this; BottomRight = this; BottomLeft = this }

    type Rect with
        member inline this.AsQuad : Quad =
            {
                TopLeft = new Vector2(this.Left, this.Top)
                TopRight = new Vector2(this.Right, this.Top)
                BottomRight = new Vector2(this.Right, this.Bottom)
                BottomLeft = new Vector2(this.Left, this.Bottom)
            }