namespace Percyqaz.Flux.UI

module Alignment =

    [<Literal>]
    let LEFT = 0.0f

    [<Literal>]
    let CENTER = 0.5f

    [<Literal>]
    let RIGHT = 1.0f

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

    let lerp (amount: float32) (x, percentage) (x2, percentage2) =
        x + (x2 - x) * amount,
        percentage + (percentage2 - percentage) * amount

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

// -- Various transform utilities --
type Position with

    // Translate: Standard translate operation
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
    member inline this.Translate(x, y) =
        {
            Left = this.Left ^+ x
            Top = this.Top ^+ y
            Right = this.Right ^+ x
            Bottom = this.Bottom ^+ y
        }

    // Shrink: Opposite of expand
    member inline this.ShrinkL amount = { this with Left = this.Left ^+ amount }
    member inline this.ShrinkT amount = { this with Top = this.Top ^+ amount }
    member inline this.ShrinkR amount = { this with Right = this.Right ^- amount }
    member inline this.ShrinkB amount = { this with Bottom = this.Bottom ^- amount }
    member inline this.ShrinkX amount = { this with Left = this.Left ^+ amount; Right = this.Right ^- amount }
    member inline this.ShrinkY amount = { this with Top = this.Top ^+ amount; Bottom = this.Bottom ^- amount }
    member inline this.Shrink (x, y) = this.ShrinkX(x).ShrinkY(y)
    member inline this.Shrink amount = this.Shrink(amount, amount)

    member inline this.ShrinkPercentL percent = { this with Left = Position.lerp percent this.Left this.Right }
    member inline this.ShrinkPercentT percent = { this with Top = Position.lerp percent this.Top this.Bottom }
    member inline this.ShrinkPercentR percent = { this with Right = Position.lerp percent this.Right this.Left }
    member inline this.ShrinkPercentB percent = { this with Bottom = Position.lerp percent this.Bottom this.Top }
    member inline this.ShrinkPercentX percent =
        { this with
            Left = Position.lerp percent this.Left this.Right
            Right = Position.lerp percent this.Right this.Left
        }
    member inline this.ShrinkPercentY percent =
        { this with
            Top = Position.lerp percent this.Top this.Bottom
            Bottom = Position.lerp percent this.Bottom this.Top
        }
    member inline this.ShrinkPercent (percent_x, percent_y) = this.ShrinkPercentX(percent_x).ShrinkPercentY(percent_y)
    member inline this.ShrinkPercent percent = this.ShrinkPercentX(percent).ShrinkPercentY(percent)

    // Expand: Enlarge by the given amount in all directions
    member inline this.ExpandL amount = this.ShrinkL -amount
    member inline this.ExpandT amount = this.ShrinkT -amount
    member inline this.ExpandR amount = this.ShrinkR -amount
    member inline this.ExpandB amount = this.ShrinkB -amount
    member inline this.ExpandX amount = this.ShrinkX -amount
    member inline this.ExpandY amount = this.ShrinkY -amount
    member inline this.Expand (x, y) = this.Shrink (-x, -y)
    member inline this.Expand amount = this.Shrink -amount

    member inline this.ExpandPercentL percent = this.ShrinkPercentL -percent
    member inline this.ExpandPercentT percent = this.ShrinkPercentT -percent
    member inline this.ExpandPercentR percent = this.ShrinkPercentR -percent
    member inline this.ExpandPercentB percent = this.ShrinkPercentB -percent
    member inline this.ExpandPercentX percent = this.ShrinkPercentX -percent
    member inline this.ExpandPercentY percent = this.ShrinkPercentY -percent
    member inline this.ExpandPercent (percent_x, percent_y) = this.ShrinkPercent(percent_x, percent_y)
    member inline this.ExpandPercent percent = this.ShrinkPercent -percent

    // Slice: Gets a strip of a certain thickness, inside an edge of the bounding box
    member inline this.SliceL thickness = { this with Right = this.Left ^+ thickness }
    member inline this.SliceL (start, thickness) = { this with Left = this.Left ^+ start; Right = this.Left ^+ start + thickness }
    member inline this.SliceT thickness = { this with Bottom = this.Top ^+ thickness }
    member inline this.SliceT (start, thickness) = { this with Top = this.Top ^+ start; Bottom = this.Top ^+ start + thickness }
    member inline this.SliceR thickness = { this with Left = this.Right ^- thickness }
    member inline this.SliceR (start, thickness) = { this with Right = this.Right ^- start; Left = this.Right ^- (start + thickness) }
    member inline this.SliceB thickness = { this with Top = this.Bottom ^- thickness }
    member inline this.SliceB (start, thickness) = { this with Bottom = this.Bottom ^- start; Top = this.Bottom ^- (start + thickness) }
    member inline this.SliceX thickness =
        let center = Position.lerp 0.5f this.Left this.Right
        { this with
            Left = center ^- (thickness * 0.5f)
            Right = center ^+ (thickness * 0.5f)
        }
    member inline this.SliceY thickness =
        let center = Position.lerp 0.5f this.Top this.Bottom
        { this with
            Top = center ^- (thickness * 0.5f)
            Bottom = center ^+ (thickness * 0.5f)
        }

    member inline this.SlicePercentL percent = { this with Right = Position.lerp percent this.Left this.Right }
    member inline this.SlicePercentL (start: float32, thickness: float32) = this.ShrinkPercentL(start).SlicePercentL(thickness / (1.0f - start))
    member inline this.SlicePercentT percent = { this with Bottom = Position.lerp percent this.Top this.Bottom }
    member inline this.SlicePercentT (start: float32, thickness: float32) = this.ShrinkPercentT(start).SlicePercentT(thickness / (1.0f - start))
    member inline this.SlicePercentR percent = { this with Left = Position.lerp percent this.Right this.Left }
    member inline this.SlicePercentR (start: float32, thickness: float32) = this.ShrinkPercentR(start).SlicePercentR(thickness / (1.0f - start))
    member inline this.SlicePercentB percent = { this with Top = Position.lerp percent this.Bottom this.Top }
    member inline this.SlicePercentB (start: float32, thickness: float32) = this.ShrinkPercentB(start).SlicePercentB(thickness / (1.0f - start))
    member inline this.SlicePercentX percent =
        { this with
            Left = Position.lerp (0.5f - percent * 0.5f) this.Left this.Right
            Right = Position.lerp (0.5f + percent * 0.5f) this.Left this.Right
        }
    member inline this.SlicePercentY percent =
        { this with
            Top = Position.lerp (0.5f - percent * 0.5f) this.Top this.Bottom
            Bottom = Position.lerp (0.5f + percent * 0.5f) this.Top this.Bottom
        }

    // Border: Gets a strip of a certain thickness, outside an edge of the bounding box
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

    // Methods to cut an area into even slices, with spacing between the slices
    member inline this.GridX (position: int, columns: int, spacing: float32) : Position =
        let flerp = Percyqaz.Common.Combinators.lerp
        let l = float32 (position - 1) / float32 columns
        let r = float32 position / float32 columns
        { this with
            Left = Position.lerp l this.Left this.Right ^+ (flerp l 0.0f spacing)
            Right = Position.lerp r this.Left this.Right ^- (flerp r spacing 0.0f)
        }

    member inline this.GridX(position: int, columns: int) : Position = this.GridX(position, columns, 0.0f)

    member inline this.GridY (position: int, rows: int, spacing: float32) : Position =
        let flerp = Percyqaz.Common.Combinators.lerp
        let t = float32 (position - 1) / float32 rows
        let b = float32 position / float32 rows
        { this with
            Top = Position.lerp t this.Top this.Bottom ^+ (flerp t 0.0f spacing)
            Bottom = Position.lerp b this.Top this.Bottom ^- (flerp b spacing 0.0f)
        }

    member inline this.GridY(position: int, rows: int) : Position = this.GridY(position, rows, 0.0f)

    // Static equivalents of all methods, to write as Position.___ instead of Position.DEFAULT.___
    static member inline ExpandL amount = Position.DEFAULT.ExpandL amount
    static member inline ExpandT amount = Position.DEFAULT.ExpandT amount
    static member inline ExpandR amount = Position.DEFAULT.ExpandR amount
    static member inline ExpandB amount = Position.DEFAULT.ExpandB amount
    static member inline ExpandX amount = Position.DEFAULT.ExpandX amount
    static member inline ExpandY amount = Position.DEFAULT.ExpandY amount
    static member inline Expand (x, y) = Position.DEFAULT.Expand(x, y)
    static member inline Expand amount = Position.DEFAULT.Expand(amount, amount)

    static member inline ExpandPercentL percent = Position.DEFAULT.ExpandPercentL percent
    static member inline ExpandPercentT percent = Position.DEFAULT.ExpandPercentT percent
    static member inline ExpandPercentR percent = Position.DEFAULT.ExpandPercentR percent
    static member inline ExpandPercentB percent = Position.DEFAULT.ExpandPercentB percent
    static member inline ExpandPercentX percent = Position.DEFAULT.ExpandPercentX percent
    static member inline ExpandPercentY percent = Position.DEFAULT.ExpandPercentY percent

    static member inline ShrinkL amount = Position.DEFAULT.ShrinkL amount
    static member inline ShrinkT amount = Position.DEFAULT.ShrinkT amount
    static member inline ShrinkR amount = Position.DEFAULT.ShrinkR amount
    static member inline ShrinkB amount = Position.DEFAULT.ShrinkB amount
    static member inline ShrinkX amount = Position.DEFAULT.ShrinkX amount
    static member inline ShrinkY amount = Position.DEFAULT.ShrinkY amount
    static member inline Shrink (x, y) = Position.DEFAULT.Shrink(x, y)
    static member inline Shrink amount = Position.DEFAULT.Shrink(amount, amount)

    static member inline ShrinkPercentL percent = Position.DEFAULT.ShrinkPercentL percent
    static member inline ShrinkPercentT percent = Position.DEFAULT.ShrinkPercentT percent
    static member inline ShrinkPercentR percent = Position.DEFAULT.ShrinkPercentR percent
    static member inline ShrinkPercentB percent = Position.DEFAULT.ShrinkPercentB percent
    static member inline ShrinkPercentX percent = Position.DEFAULT.ShrinkPercentX percent
    static member inline ShrinkPercentY percent = Position.DEFAULT.ShrinkPercentY percent

    static member inline SliceL thickness = Position.DEFAULT.SliceL thickness
    static member inline SliceL (start, thickness) = Position.DEFAULT.SliceL (start, thickness)
    static member inline SliceT thickness = Position.DEFAULT.SliceT thickness
    static member inline SliceT (start, thickness) = Position.DEFAULT.SliceT (start, thickness)
    static member inline SliceR thickness = Position.DEFAULT.SliceR thickness
    static member inline SliceR (start, thickness) = Position.DEFAULT.SliceR (start, thickness)
    static member inline SliceB thickness = Position.DEFAULT.SliceB thickness
    static member inline SliceB (start, thickness) = Position.DEFAULT.SliceB (start, thickness)
    static member inline SliceX thickness = Position.DEFAULT.SliceX thickness
    static member inline SliceY thickness = Position.DEFAULT.SliceY thickness

    static member inline SlicePercentL percent = { Position.DEFAULT with Right = percent %+ 0.0f }
    static member inline SlicePercentL (start, thickness) = { Position.DEFAULT with Left = start %+ 0.0f; Right = (start + thickness) %+ 0.0f }
    static member inline SlicePercentT percent = { Position.DEFAULT with Bottom = percent %+ 0.0f }
    static member inline SlicePercentT (start, thickness) = { Position.DEFAULT with Top = start %+ 0.0f; Bottom = (start + thickness) %+ 0.0f }
    static member inline SlicePercentR percent = { Position.DEFAULT with Left = (1.0f - percent) %+ 0.0f }
    static member inline SlicePercentR (start, thickness) = { Position.DEFAULT with Left = (1.0f - start - thickness) %+ 0.0f; Right = (1.0f - start) %+ 0.0f }
    static member inline SlicePercentB percent = { Position.DEFAULT with Top = (1.0f - percent) %+ 0.0f }
    static member inline SlicePercentB (start, thickness) = { Position.DEFAULT with Top = (1.0f - start - thickness) %+ 0.0f; Bottom = (1.0f - start) %+ 0.0f }
    static member inline SlicePercentX percent = { Position.DEFAULT with Left = ((1.0f - percent) * 0.5f) %+ 0.0f; Right = ((1.0f + percent) * 0.5f) %+ 0.0f }
    static member inline SlicePercentY percent = { Position.DEFAULT with Top = ((1.0f - percent) * 0.5f) %+ 0.0f; Bottom = ((1.0f + percent) * 0.5f) %+ 0.0f }

    static member inline BorderL thickness = Position.DEFAULT.BorderL thickness
    static member inline BorderT thickness = Position.DEFAULT.BorderT thickness
    static member inline BorderR thickness = Position.DEFAULT.BorderR thickness
    static member inline BorderB thickness = Position.DEFAULT.BorderB thickness
    static member inline BorderCornersT thickness = Position.DEFAULT.BorderCornersT thickness
    static member inline BorderCornersB thickness = Position.DEFAULT.BorderCornersB thickness

    static member inline GridX(position: int, columns: int, spacing: float32) : Position = Position.DEFAULT.GridX(position, columns, spacing)
    static member inline GridX(position: int, columns: int) : Position = Position.DEFAULT.GridX(position, columns)
    static member inline GridY(position: int, rows: int, spacing: float32) : Position = Position.DEFAULT.GridY(position, rows, spacing)
    static member inline GridY(position: int, rows: int) : Position = Position.DEFAULT.GridY(position, rows)

    static member Box(anchorx, anchory, x, y, width, height) =
        {
            Left = anchorx %+ x
            Top = anchory %+ y
            Right = anchorx %+ (x + width)
            Bottom = anchory %+ (y + height)
        }