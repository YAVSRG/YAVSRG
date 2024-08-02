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

    member inline this.AsQuad =
        struct (new Vector2(this.Left, this.Top),
                new Vector2(this.Right, this.Top),
                new Vector2(this.Right, this.Bottom),
                new Vector2(this.Left, this.Bottom))

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

    member inline this.Translate(x, y) =
        {
            Left = this.Left + x
            Top = this.Top + y
            Right = this.Right + x
            Bottom = this.Bottom + y
        }
    member inline this.TranslateX amount =
        {
            Left = this.Left + amount
            Top = this.Top
            Right = this.Right + amount
            Bottom = this.Bottom
        }
    member inline this.TranslateY amount =
        {
            Left = this.Left
            Top = this.Top + amount
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

    member inline this.ExpandPercentL percent = this.ExpandL (this.Width * percent)
    member inline this.ExpandPercentT percent = this.ExpandT (this.Height * percent)
    member inline this.ExpandPercentR percent = this.ExpandR (this.Width * percent)
    member inline this.ExpandPercentB percent = this.ExpandB (this.Height * percent)
    member inline this.ExpandPercentX percent = this.ExpandX (this.Width * percent)
    member inline this.ExpandPercentY percent = this.ExpandY (this.Height * percent)
    member inline this.ExpandPercent percent = this.Expand (this.Width * percent, this.Height * percent)

    member inline this.Shrink(x, y) = this.Expand(-x, -y)
    member inline this.Shrink amount = this.Shrink(amount, amount)
    member inline this.ShrinkL amount = this.ExpandL -amount
    member inline this.ShrinkT amount = this.ExpandT -amount
    member inline this.ShrinkR amount = this.ExpandR -amount
    member inline this.ShrinkB amount = this.ExpandB -amount
    member inline this.ShrinkX x = this.ExpandX -x
    member inline this.ShrinkY x = this.ExpandY -x
    member inline this.ShrinkPercentL percent = this.ShrinkL (this.Width * percent)
    member inline this.ShrinkPercentT percent = this.ShrinkT (this.Height * percent)
    member inline this.ShrinkPercentR percent = this.ShrinkR (this.Width * percent)
    member inline this.ShrinkPercentB percent = this.ShrinkB (this.Height * percent)
    member inline this.ShrinkPercentX percent = this.ShrinkX (this.Width * percent)
    member inline this.ShrinkPercentY percent = this.ShrinkY (this.Height * percent)
    member inline this.ShrinkPercent percent = this.Shrink (this.Width * percent, this.Height * percent)

    member inline this.SliceL amount =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Left + amount
            Bottom = this.Bottom
        }
    member inline this.SliceT amount =
        {
            Left = this.Left
            Top = this.Top
            Right = this.Right
            Bottom = this.Top + amount
        }
    member inline this.SliceR amount =
        {
            Left = this.Right - amount
            Top = this.Top
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.SliceB amount =
        {
            Left = this.Left
            Top = this.Bottom - amount
            Right = this.Right
            Bottom = this.Bottom
        }
    member inline this.SliceX amount =
        let center_x = this.CenterX
        {
            Left = center_x - 0.5f * amount
            Top = this.Top
            Right = center_x + 0.5f * amount
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
    member inline this.SlicePercentT percent = this.SliceT (this.Height * percent)
    member inline this.SlicePercentR percent = this.SliceR (this.Width * percent)
    member inline this.SlicePercentB percent = this.SliceB (this.Height * percent)
    member inline this.SlicePercentX percent = this.SliceX (this.Width * percent)
    member inline this.SlicePercentY percent = this.SliceY (this.Height * percent)

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

type Quad = (struct (Vector2 * Vector2 * Vector2 * Vector2))
type QuadColors = (struct (Color * Color * Color * Color))

[<AutoOpen>]
module QuadColorExtensions =

    type Color with
        member inline this.AsQuad = struct (this, this, this, this)

module Quad =

    let parallelogram (amount: float32) (r: Rect) : Quad =
        let a = r.Height * 0.5f * amount

        struct (new Vector2(r.Left + a, r.Top),
                new Vector2(r.Right + a, r.Top),
                new Vector2(r.Right - a, r.Bottom),
                new Vector2(r.Left - a, r.Bottom))

    let create c1 c2 c3 c4 : Quad = struct (c1, c2, c3, c4)

    let inline createv (c1x, c1y) (c2x, c2y) (c3x, c3y) (c4x, c4y) : Quad =
        struct (new Vector2(c1x, c1y), new Vector2(c2x, c2y), new Vector2(c3x, c3y), new Vector2(c4x, c4y))

    let inline gradient_left_to_right (left: Color) (right: Color) = struct(left, right, right, left)
    let inline gradient_top_to_bottom (top: Color) (bottom: Color) = struct(top, top, bottom, bottom)

    let flip_vertical (struct (c1, c2, c3, c4): Quad) : Quad = struct (c4, c3, c2, c1)

    let map f (struct (c1, c2, c3, c4): Quad) : Quad = struct (f c1, f c2, f c3, f c4)

    let translate (x, y) (struct (c1, c2, c3, c4): Quad) : Quad =
        let v = new Vector2(x, y)
        struct (c1 + v, c2 + v, c3 + v, c4 + v)

    let rotate_about origin degrees (struct (c1, c2, c3, c4): Quad) : Quad =
        let mat = Matrix2.CreateRotation(-(float32 (degrees / 180.0 * Math.PI)))

        struct (c1, c2, c3, c4)
        |> map ((fun c -> c - origin) >> (fun c -> mat * c) >> (fun c -> c + origin))

    let rotate degrees (struct (c1, c2, c3, c4): Quad) : Quad =
        let center = (c1 + c2 + c3 + c4) * 0.25f
        rotate_about center degrees struct (c1, c2, c3, c4)
