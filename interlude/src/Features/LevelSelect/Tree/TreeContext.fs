namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Data.Library
open Interlude.UI
open Interlude.Features.Gameplay

[<AutoOpen>]
module TreeConstants =

    let [<Literal>] DRAG_THRESHOLD = 40.0f
    let [<Literal>] DRAG_LEFTCLICK_SCALE = 1.75f

    let MULTI_SELECT_KEY = Bind.mk Keys.LeftShift

    let [<Literal>] CHART_HEIGHT = 90.0f
    let [<Literal>] GROUP_HEIGHT = 55.0f

[<Struct>]
[<RequireQualifiedAccess>]
type private ScrollTo =
    | Nothing
    | Chart
    | Group of string * LibraryGroupContext

type private TreeContext =
    {
        mutable SelectedGroup: string * LibraryGroupContext
        mutable ExpandedGroup: string * LibraryGroupContext
        mutable SelectedChart: string

        // todo: should this be part of Tree object and not context?
        ScrollPosition: Animation.Fade

        // todo: make these methods instead of this hook into update
        mutable ScrollTo: ScrollTo
        mutable CurrentlyDragScrolling: bool
        mutable DragScrollDistance: float32
        mutable DragScrollPosition: float32
        mutable ClickDebounce: float
        // todo: this should be local to Tree, not here
        mutable ScrollToChartOnce: bool

        // todo: needs better name
        mutable CacheFlag: int

        mutable MultiSelection: MultiSelection option
    }

    member this.Scroll(amount: float32) : unit =
        let remaining = this.ScrollPosition.Target - this.ScrollPosition.Value

        // todo: should it add target instead of value (with a clamp for exceedingly long auto scrolls)
        this.ScrollPosition.Target <- this.ScrollPosition.Value + amount

    member this.SelectChart(chart_meta: ChartMeta, library_ctx: LibraryContext, group_name: string, group_ctx: LibraryGroupContext) : unit =
        if not (Transitions.in_progress()) then
            SelectedChart.change (chart_meta, library_ctx, true)
            Selection.clear ()
            this.SelectedChart <- chart_meta.Hash
            this.ExpandedGroup <- group_name, group_ctx
            this.SelectedGroup <- group_name, group_ctx
            this.ScrollTo <- ScrollTo.Chart

    member this.AddToMultiSelect(items: (ChartMeta * LibraryContext) seq) : unit =
        match this.MultiSelection with
        | Some s -> s.Select items
        | None -> this.MultiSelection <- Some (MultiSelection.Create items)

    member this.AddToMultiSelect(chart_meta: ChartMeta, library_ctx: LibraryContext) : unit = this.AddToMultiSelect([chart_meta, library_ctx])

    member this.RemoveFromMultiSelect(items: (ChartMeta * LibraryContext) seq) : unit =
        match this.MultiSelection with
        | None -> ()
        | Some s ->
            s.Deselect items
            if s.IsEmpty then this.MultiSelection <- None

    member this.RemoveFromMultiSelect(chart_meta: ChartMeta, library_ctx: LibraryContext) : unit = this.RemoveFromMultiSelect([chart_meta, library_ctx])

    static member Create : TreeContext =
        {
            SelectedGroup = "", LibraryGroupContext.None
            ExpandedGroup = "", LibraryGroupContext.None
            SelectedChart = ""

            ScrollPosition = Animation.Fade 300.0f

            ScrollTo = ScrollTo.Nothing
            CurrentlyDragScrolling = false
            DragScrollDistance = 0.0f
            DragScrollPosition = 0.0f
            ClickDebounce = 0.0
            ScrollToChartOnce = false

            CacheFlag = 0

            MultiSelection = None
        }

[<AbstractClass>]
type private TreeItem(ctx: TreeContext) =
    abstract member Bounds: float32 -> Rect
    abstract member Selected: bool
    abstract member Spacing: float32

    member this.CheckBounds(top: float32, origin: float32, originB: float32, if_visible: Rect -> unit) =
        let bounds = this.Bounds(top + this.Spacing * 0.5f)

        if bounds.Bottom > origin && top < originB then
            if_visible bounds

        top + bounds.Height + this.Spacing

    member this.LeftClick(origin: float32) : bool =
        ctx.ClickDebounce <= 0.0
        && Mouse.released Mouse.LEFT
        && ctx.DragScrollDistance <= DRAG_THRESHOLD
        && Mouse.y () > origin

    member this.RightClick(origin: float32) : bool =
        ctx.ClickDebounce <= 0.0
        && Mouse.released Mouse.RIGHT
        && ctx.DragScrollDistance <= DRAG_THRESHOLD
        && Mouse.y () > origin