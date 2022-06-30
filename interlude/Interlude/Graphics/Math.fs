namespace Interlude.Graphics

open System
open System.Drawing
open OpenTK.Mathematics

(*
    Storage of rectangles as left, top, right, bottom
    Preferred over left, top, width, height for a handful of reasons
*)

type [<Struct>] Rect = 
    { Left: float32; Top: float32; Right: float32; Bottom: float32 }
    static member Create(l, t, r, b) = { Left = l; Top = t; Right = r; Bottom = b }
    static member Box(l, t, w, h) = { Left = l; Top = t; Right = l + w; Bottom = t + h }

    member inline this.Width = this.Right - this.Left
    member inline this.Height = this.Bottom - this.Top

    member inline this.CenterX = (this.Left + this.Right) * 0.5f
    member inline this.CenterY = (this.Top + this.Bottom) * 0.5f
    member inline this.Center = (this.CenterX, this.CenterY)

    member inline this.Contains (x, y) = x > this.Left && x < this.Right && y > this.Top && y < this.Bottom

    member inline this.Intersect (other: Rect) =
        { Left = max this.Left other.Left; Top = max this.Top other.Top; Right = min this.Right other.Right; Bottom = min this.Bottom other.Bottom }

    member inline this.Translate (x, y) =
        { Left = this.Left + x; Top = this.Top + y; Right = this.Right + x; Bottom = this.Bottom + y }

    member inline this.Expand (x, y) =
        { Left = this.Left - x; Top = this.Top - y; Right = this.Right + x; Bottom = this.Bottom + y }
    member inline this.Expand amount = this.Expand (amount, amount)

    member inline this.Shrink (x, y) = this.Expand (-x, -y)
    member inline this.Shrink amount = this.Shrink (amount, amount)

    member inline this.SliceLeft amount =
        { Left = this.Left; Top = this.Top; Right = this.Left + amount; Bottom = this.Bottom }
    member inline this.SliceTop amount = 
        { Left = this.Left; Top = this.Top; Right = this.Right; Bottom = this.Top + amount }
    member inline this.SliceRight amount =
        { Left = this.Right - amount; Top = this.Top; Right = this.Right; Bottom = this.Bottom }
    member inline this.SliceBottom amount =
        { Left = this.Left; Top = this.Bottom - amount; Right = this.Right; Bottom = this.Bottom }
    
    member inline this.TrimLeft amount =
        { Left = this.Left + amount; Top = this.Top; Right = this.Right; Bottom = this.Bottom }
    member inline this.TrimTop amount =
        { Left = this.Left; Top = this.Top + amount; Right = this.Right; Bottom = this.Bottom }
    member inline this.TrimRight amount =
        { Left = this.Left; Top = this.Top; Right = this.Right - amount; Bottom = this.Bottom }
    member inline this.TrimBottom amount =
        { Left = this.Left; Top = this.Top; Right = this.Right; Bottom = this.Bottom - amount }

module Rect = 

    let ZERO = Rect.Box(0.0f, 0.0f, 0.0f, 0.0f)
    let ONE = Rect.Box(0.0f, 0.0f, 1.0f, 1.0f)

(*
    Simple storage of vertices to render as a quad
*)

type Quad = (struct(Vector2 * Vector2 * Vector2 * Vector2))
type QuadColors = (struct(Color * Color * Color * Color))

module Quad =

    let ofRect (r: Rect) : Quad =
        struct (new Vector2(r.Left, r.Top), new Vector2(r.Right, r.Top), new Vector2(r.Right, r.Bottom), new Vector2(r.Left, r.Bottom))

    let parallelogram (amount: float32) (r: Rect): Quad =
        let a = r.Height * 0.5f * amount
        struct (new Vector2(r.Left + a, r.Top), new Vector2(r.Right + a, r.Top), new Vector2(r.Right - a, r.Bottom), new Vector2(r.Left - a, r.Bottom))

    let create c1 c2 c3 c4 : Quad = struct (c1, c2, c3, c4)
    let createv (c1x, c1y) (c2x, c2y) (c3x, c3y) (c4x, c4y) : Quad = struct (new Vector2(c1x, c1y), new Vector2(c2x, c2y), new Vector2(c3x, c3y), new Vector2(c4x, c4y))

    let colorOf c: QuadColors = struct (c, c, c, c)

    let flip (struct (c1, c2, c3, c4): Quad) : Quad = struct (c4, c3, c2, c1)

    let map f (struct (c1, c2, c3, c4): Quad) : Quad = struct (f c1, f c2, f c3, f c4)

    let rotateDeg degrees (struct (c1, c2, c3, c4): Quad) : Quad =
        let centre = (c1 + c2 + c3 + c4) * 0.25f
        let mat = Matrix2.CreateRotation(-(float32(degrees / 180.0 * Math.PI)))
        struct (c1, c2, c3, c4)
        |> map ((fun c -> c - centre) >> (fun c -> mat * c) >> (fun c -> c + centre))