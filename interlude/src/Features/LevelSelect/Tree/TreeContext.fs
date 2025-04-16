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
    let [<Literal>] CHART_SPACING = Style.PADDING
    let [<Literal>] GROUP_HEIGHT = 55.0f
    let [<Literal>] GROUP_SPACING = Style.PADDING * 4.0f

    let [<Literal>] TREE_LEFT_SPLIT = 0.4f

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
        mutable MultiSelection: MultiSelection option

        ScrollPosition: Animation.Fade

        // todo: make these methods instead of this hook into update
        mutable ScrollTo: ScrollTo
        mutable ClickDebounce: float

        mutable DragScrollDistance: float32
        // todo: these 3 should be local to Tree?
        mutable CurrentlyDragScrolling: bool
        mutable DragScrollPosition: float32
        mutable ScrollToChartOnce: bool

        // todo: needs better name
        mutable CacheFlag: int

    }

    member this.IsSelected(chart_meta: ChartMeta, library_ctx: LibraryContext) : bool =
        this.SelectedChart = chart_meta.Hash &&
        SelectedChart.LIBRARY_CTX.Matches library_ctx

    member this.IsMultiSelected(chart_meta: ChartMeta, library_ctx: LibraryContext) : bool =
        match this.MultiSelection with
        | Some s -> s.Contains(chart_meta, library_ctx)
        | None -> false

    member this.IsGroupSelected(name: string, group_ctx: LibraryGroupContext) : bool =
        this.SelectedGroup = (name, group_ctx)

    member this.IsGroupExpanded(name: string, group_ctx: LibraryGroupContext) : bool =
        this.ExpandedGroup = (name, group_ctx)

    member this.Scroll(amount: float32) : unit =
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

    member this.ToggleMultiSelect(chart_meta: ChartMeta, library_ctx: LibraryContext) : unit =
        match this.MultiSelection with
        | Some s when s.Contains(chart_meta, library_ctx) -> this.RemoveFromMultiSelect(chart_meta, library_ctx)
        | _ -> this.AddToMultiSelect(chart_meta, library_ctx)

    member this.ToggleMultiSelect(group_name: string, group_ctx: LibraryGroupContext, charts_as_seq: (ChartMeta * LibraryContext) seq) : unit =
        match this.MultiSelection with
        | Some s when s.GroupAmountSelected(group_name, group_ctx, charts_as_seq) = AmountSelected.All ->
            this.RemoveFromMultiSelect charts_as_seq
        | _ -> this.AddToMultiSelect charts_as_seq

    static member Create : TreeContext =
        {
            SelectedGroup = "", LibraryGroupContext.None
            ExpandedGroup = "", LibraryGroupContext.None
            SelectedChart = ""
            MultiSelection = None

            ScrollPosition = Animation.Fade 300.0f

            ScrollTo = ScrollTo.Nothing
            CurrentlyDragScrolling = false
            DragScrollDistance = 0.0f
            DragScrollPosition = 0.0f
            ClickDebounce = 0.0
            ScrollToChartOnce = false

            CacheFlag = 0
        }

[<AbstractClass>]
type private TreeItem(ctx: TreeContext) =
    abstract member Bounds: float32 -> Rect
    abstract member Spacing: float32

    /// Runs the passed function if this item is visible
    /// Returns the new "this_top" that the next element should use
    member this.IfVisible(this_top: float32, tree_top: float32, tree_bottom: float32, if_visible: Rect -> unit) : float32 =
        let bounds = this.Bounds(this_top + this.Spacing * 0.5f)

        if bounds.Bottom > tree_top && this_top < tree_bottom then
            if_visible bounds

        this_top + bounds.Height + this.Spacing

    member this.LeftClicked(tree_top: float32) : bool =
        ctx.ClickDebounce <= 0.0
        && Mouse.released Mouse.LEFT
        && ctx.DragScrollDistance <= DRAG_THRESHOLD
        && Mouse.y () > tree_top

    member this.RightClicked(tree_top: float32) : bool =
        ctx.ClickDebounce <= 0.0
        && Mouse.released Mouse.RIGHT
        && ctx.DragScrollDistance <= DRAG_THRESHOLD
        && Mouse.y () > tree_top