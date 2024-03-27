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

    member this.SliceTop amount =
        { this with
            Bottom = this.Top ^+ amount
        }

    member this.SliceRight amount =
        { this with
            Left = this.Right ^- amount
        }

    member this.SliceBottom amount =
        { this with
            Top = this.Bottom ^- amount
        }

    static member SliceLeft amount = Position.Default.SliceLeft amount
    static member SliceTop amount = Position.Default.SliceTop amount
    static member SliceRight amount = Position.Default.SliceRight amount
    static member SliceBottom amount = Position.Default.SliceBottom amount

    member this.TrimLeft amount =
        { this with Left = this.Left ^+ amount }

    member this.TrimTop amount = { this with Top = this.Top ^+ amount }

    member this.TrimRight amount =
        { this with
            Right = this.Right ^- amount
        }

    member this.TrimBottom amount =
        { this with
            Bottom = this.Bottom ^- amount
        }

    static member TrimLeft amount = Position.Default.TrimLeft amount
    static member TrimTop amount = Position.Default.TrimTop amount
    static member TrimRight amount = Position.Default.TrimRight amount
    static member TrimBottom amount = Position.Default.TrimBottom amount

    member this.BorderLeft amount =
        { this with
            Left = this.Left ^- amount
            Right = this.Left
        }

    member this.BorderTop amount =
        { this with
            Top = this.Top ^- amount
            Bottom = this.Top
        }

    member this.BorderRight amount =
        { this with
            Left = this.Right
            Right = this.Right ^+ amount
        }

    member this.BorderBottom amount =
        { this with
            Top = this.Bottom
            Bottom = this.Bottom ^+ amount
        }

    static member BorderLeft amount = Position.Default.BorderLeft amount
    static member BorderTop amount = Position.Default.BorderTop amount
    static member BorderRight amount = Position.Default.BorderRight amount
    static member BorderBottom amount = Position.Default.BorderBottom amount

    member this.BorderTopCorners amount =
        { this with
            Left = this.Left ^- amount
            Top = this.Top ^- amount
            Right = this.Right ^+ amount
            Bottom = this.Top
        }

    member this.BorderBottomCorners amount =
        { this with
            Left = this.Left ^- amount
            Top = this.Bottom
            Right = this.Right ^+ amount
            Bottom = this.Bottom ^+ amount
        }

    static member BorderTopCorners amount =
        Position.Default.BorderTopCorners amount

    static member BorderBottomCorners amount =
        Position.Default.BorderBottomCorners amount

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
