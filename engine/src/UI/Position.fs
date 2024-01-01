namespace Percyqaz.Flux.UI

type Position =
    {
        Left: float32 * float32
        Top: float32 * float32
        Right: float32 * float32
        Bottom: float32 * float32
    }

[<AutoOpen>]
module PositionOperators =
    let (%+) (percentage: float32) (offset: float32) = (offset, percentage)
    let (%-) (percentage: float32) (offset: float32) = (-offset, percentage)
    let (^+) (x, percentage) offset = (x + offset, percentage)
    let (^-) (x, percentage) offset = (x - offset, percentage)

module Position =
    let min = 0.0f %+ 0.0f
    let max = 1.0f %+ 0.0f

    let calculate (pos: Position) (bounds: Percyqaz.Flux.Graphics.Rect) =
        let inline c (offset, percent) min max : float32 = min + percent * (max - min) + offset

        {
            Left = c pos.Left bounds.Left bounds.Right
            Top = c pos.Top bounds.Top bounds.Bottom
            Right = c pos.Right bounds.Left bounds.Right
            Bottom = c pos.Bottom bounds.Top bounds.Bottom
        }
        : Percyqaz.Flux.Graphics.Rect

type Position with
    static member Default =
        {
            Left = Position.min
            Top = Position.min
            Right = Position.max
            Bottom = Position.max
        }

    member this.Margin(x, y) =
        {
            Left = this.Left ^+ x
            Top = this.Top ^+ y
            Right = this.Right ^- x
            Bottom = this.Bottom ^- y
        }

    member this.Margin amount = this.Margin(amount, amount)
    static member Margin(x, y) = Position.Default.Margin(x, y)
    static member Margin amount = Position.Margin(amount, amount)

    member this.Translate(x, y) =
        {
            Left = this.Left ^+ x
            Top = this.Top ^+ y
            Right = this.Right ^+ x
            Bottom = this.Bottom ^+ y
        }

    member this.SliceLeft amount =
        { this with
            Right = this.Left ^+ amount
        }

    static member SliceLeft amount = Position.Default.SliceLeft amount

    member this.SliceTop amount =
        { this with
            Bottom = this.Top ^+ amount
        }

    static member SliceTop amount = Position.Default.SliceTop amount

    member this.SliceRight amount =
        { this with
            Left = this.Right ^- amount
        }

    static member SliceRight amount = Position.Default.SliceRight amount

    member this.SliceBottom amount =
        { this with
            Top = this.Bottom ^- amount
        }

    static member SliceBottom amount = Position.Default.SliceBottom amount

    member this.TrimLeft amount =
        { this with Left = this.Left ^+ amount }

    static member TrimLeft amount = Position.Default.TrimLeft amount

    member this.TrimTop amount = { this with Top = this.Top ^+ amount }
    static member TrimTop amount = Position.Default.TrimTop amount

    member this.TrimRight amount =
        { this with
            Right = this.Right ^- amount
        }

    static member TrimRight amount = Position.Default.TrimRight amount

    member this.TrimBottom amount =
        { this with
            Bottom = this.Bottom ^- amount
        }

    static member TrimBottom amount = Position.Default.TrimBottom amount

    static member Row(y, height) =
        {
            Left = Position.min
            Top = 0.0f %+ y
            Right = Position.max
            Bottom = 0.0f %+ (y + height)
        }

    static member Column(x, width) =
        {
            Left = 0.0f %+ x
            Top = Position.min
            Right = 0.0f %+ (x + width)
            Bottom = Position.max
        }

    static member Grid(l, t, r, b) =
        {
            Left = l %+ 0.0f
            Top = t %+ 0.0f
            Right = r %+ 0.0f
            Bottom = b %+ 0.0f
        }

    static member Box(anchorx, anchory, x, y, width, height) =
        {
            Left = anchorx %+ x
            Top = anchory %+ y
            Right = anchorx %+ (x + width)
            Bottom = anchory %+ (y + height)
        }

    static member Box(anchorx, anchory, width, height) =
        {
            Left = anchorx %+ 0.0f
            Top = anchory %+ 0.0f
            Right = anchorx %+ width
            Bottom = anchory %+ height
        }
