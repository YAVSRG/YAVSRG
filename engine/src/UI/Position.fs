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
    let MIN = 0.0f %+ 0.0f
    let MAX = 1.0f %+ 0.0f

    let calculate (pos: Position) (bounds: Percyqaz.Flux.Graphics.Rect) =
        let inline c (offset, percent) min max : float32 = min + percent * (max - min) + offset

        {
            Left = c pos.Left bounds.Left bounds.Right
            Top = c pos.Top bounds.Top bounds.Bottom
            Right = c pos.Right bounds.Left bounds.Right
            Bottom = c pos.Bottom bounds.Top bounds.Bottom
        }
        : Percyqaz.Flux.Graphics.Rect

    let DEFAULT =
        {
            Left = MIN
            Top = MIN
            Right = MAX
            Bottom = MAX
        }

type Position with

    member inline this.Translate(x, y) =
        {
            Left = this.Left ^+ x
            Top = this.Top ^+ y
            Right = this.Right ^+ x
            Bottom = this.Bottom ^+ y
        }
    member inline this.TranslateX amount =
        { this with
            Left = this.Left ^+ amount
            Right = this.Right ^+ amount
        }
    member inline this.TranslateY amount =
        { this with
            Top = this.Top ^+ amount
            Bottom = this.Bottom ^+ amount
        }

    member inline this.Shrink(x, y) =
        {
            Left = this.Left ^+ x
            Top = this.Top ^+ y
            Right = this.Right ^- x
            Bottom = this.Bottom ^- y
        }
    member inline this.Shrink amount = this.Shrink(amount, amount)
    member inline this.ShrinkL amount = { this with Left = this.Left ^+ amount }
    member inline this.ShrinkT amount = { this with Top = this.Top ^+ amount }
    member inline this.ShrinkR amount = { this with Right = this.Right ^- amount }
    member inline this.ShrinkB amount = { this with Bottom = this.Bottom ^- amount }
    member inline this.ShrinkX amount = { this with Left = this.Left ^+ amount; Right = this.Right ^- amount }
    member inline this.ShrinkY amount = { this with Top = this.Top ^+ amount; Bottom = this.Bottom ^- amount }

    static member inline Shrink (x, y) = Position.DEFAULT.Shrink(x, y)
    static member inline Shrink amount = Position.DEFAULT.Shrink(amount, amount)
    static member inline ShrinkL amount = Position.DEFAULT.ShrinkL amount
    static member inline ShrinkT amount = Position.DEFAULT.ShrinkT amount
    static member inline ShrinkR amount = Position.DEFAULT.ShrinkR amount
    static member inline ShrinkB amount = Position.DEFAULT.ShrinkB amount
    static member inline ShrinkX amount = Position.DEFAULT.ShrinkX amount
    static member inline ShrinkY amount = Position.DEFAULT.ShrinkY amount

    member inline this.Expand (x, y) = this.Shrink (-x, -y)
    member inline this.Expand amount = this.Shrink -amount
    member inline this.ExpandL amount = this.ShrinkL -amount
    member inline this.ExpandT amount = this.ShrinkT -amount
    member inline this.ExpandR amount = this.ShrinkR -amount
    member inline this.ExpandB amount = this.ShrinkB -amount
    member inline this.ExpandX amount = this.ShrinkX -amount
    member inline this.ExpandY amount = this.ShrinkY -amount

    static member inline Expand (x, y) = Position.DEFAULT.Expand(x, y)
    static member inline Expand amount = Position.DEFAULT.Expand(amount, amount)
    static member inline ExpandL amount = Position.DEFAULT.ExpandL amount
    static member inline ExpandT amount = Position.DEFAULT.ExpandT amount
    static member inline ExpandR amount = Position.DEFAULT.ExpandR amount
    static member inline ExpandB amount = Position.DEFAULT.ExpandB amount
    static member inline ExpandX amount = Position.DEFAULT.ExpandX amount
    static member inline ExpandY amount = Position.DEFAULT.ExpandY amount

    member inline this.SliceL amount = { this with Right = this.Left ^+ amount }
    member inline this.SliceT amount = { this with Bottom = this.Top ^+ amount }
    member inline this.SliceR amount = { this with Left = this.Right ^- amount }
    member inline this.SliceB amount = { this with Top = this.Bottom ^- amount }
    member inline this.SliceX width =
        let (lefto, lefta) = this.Left
        let (righto, righta) = this.Right
        let center = (0.5f * (lefto + righto), 0.5f * (lefta + righta))
        { this with
            Left = center ^- (width * 0.5f)
            Right = center ^+ (width * 0.5f)
        }
    member inline this.SliceY height =
        let (topo, topa) = this.Top
        let (bottomo, bottoma) = this.Bottom
        let center = (0.5f * (topo + bottomo), 0.5f * (topa + bottoma))
        { this with
            Top = center ^- (height * 0.5f)
            Bottom = center ^+ (height * 0.5f)
        }

    static member inline SliceL amount = Position.DEFAULT.SliceL amount
    static member inline SliceT amount = Position.DEFAULT.SliceT amount
    static member inline SliceR amount = Position.DEFAULT.SliceR amount
    static member inline SliceB amount = Position.DEFAULT.SliceB amount
    static member inline SliceX width = Position.DEFAULT.SliceX width
    static member inline SliceY height = Position.DEFAULT.SliceY height

    // todo: rename these to be consistent with Rect
    static member inline SliceLPercent percent = { Position.DEFAULT with Right = percent %+ 0.0f }
    static member inline SliceTPercent percent = { Position.DEFAULT with Bottom = percent %+ 0.0f }
    static member inline SliceRPercent percent = { Position.DEFAULT with Left = (1.0f - percent) %+ 0.0f }
    static member inline SliceBPercent percent = { Position.DEFAULT with Top = (1.0f - percent) %+ 0.0f }
    static member inline SliceXPercent percent = { Position.DEFAULT with Left = ((1.0f - percent) * 0.5f) %+ 0.0f; Right = ((1.0f + percent) * 0.5f) %+ 0.0f }
    static member inline SliceYPercent percent = { Position.DEFAULT with Top = ((1.0f - percent) * 0.5f) %+ 0.0f; Bottom = ((1.0f + percent) * 0.5f) %+ 0.0f }

    member inline this.BorderL amount =
        { this with
            Left = this.Left ^- amount
            Right = this.Left
        }
    member inline this.BorderT amount =
        { this with
            Top = this.Top ^- amount
            Bottom = this.Top
        }
    member inline this.BorderR amount =
        { this with
            Left = this.Right
            Right = this.Right ^+ amount
        }
    member inline this.BorderB amount =
        { this with
            Top = this.Bottom
            Bottom = this.Bottom ^+ amount
        }
    member inline this.BorderCornersT amount =
        { this with
            Left = this.Left ^- amount
            Top = this.Top ^- amount
            Right = this.Right ^+ amount
            Bottom = this.Top
        }
    member inline this.BorderCornersB amount =
        { this with
            Left = this.Left ^- amount
            Top = this.Bottom
            Right = this.Right ^+ amount
            Bottom = this.Bottom ^+ amount
        }

    static member inline BorderL amount = Position.DEFAULT.BorderL amount
    static member inline BorderT amount = Position.DEFAULT.BorderT amount
    static member inline BorderR amount = Position.DEFAULT.BorderR amount
    static member inline BorderB amount = Position.DEFAULT.BorderB amount
    static member inline BorderCornersT amount = Position.DEFAULT.BorderCornersT amount
    static member inline BorderCornersB amount = Position.DEFAULT.BorderCornersB amount

    static member Row(y, height) =
        {
            Left = Position.MIN
            Top = 0.0f %+ y
            Right = Position.MAX
            Bottom = 0.0f %+ (y + height)
        }

    static member Column(x, width) =
        {
            Left = 0.0f %+ x
            Top = Position.MIN
            Right = 0.0f %+ (x + width)
            Bottom = Position.MAX
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