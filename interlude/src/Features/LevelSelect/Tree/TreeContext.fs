namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Data.Library
open Interlude.UI
open Interlude.Features.Gameplay

[<Struct>]
[<RequireQualifiedAccess>]
type private ScrollTo =
    | Nothing
    | Chart
    | Group of string * LibraryGroupContext

[<AutoOpen>]
module TreeConstants =

    let [<Literal>] DRAG_THRESHOLD = 40.0f
    let [<Literal>] DRAG_LEFTCLICK_SCALE = 1.75f

    let MULTI_SELECT_KEY = Bind.mk Keys.LeftShift

    let [<Literal>] CHART_HEIGHT = 90.0f
    let [<Literal>] GROUP_HEIGHT = 55.0f

module private TreeState =

    /// Group's name = this string => Selected chart is in this group
    let mutable selected_group = "", LibraryGroupContext.None
    /// Group's name = this string => That group is expanded in level select
    /// Only one group can be expanded at a time, and it is independent of the "selected" group
    let mutable expanded_group = "", LibraryGroupContext.None
    /// Chart's hash = this string && contexts match => It's the selected chart
    let mutable selected_chart = ""

    let scroll_pos = Animation.Fade 300.0f

    let scroll (amount: float32) : unit =
        scroll_pos.Target <- scroll_pos.Value + amount

    /// Set this value to have it "consumed" in the next frame by a level select item with sufficient knowledge to do so
    let mutable scroll_to = ScrollTo.Nothing

    let mutable currently_drag_scrolling = false
    let mutable drag_scroll_distance = 0.0f
    let mutable drag_scroll_position = 0.0f
    let mutable click_debounce = 0.0
    let mutable scroll_to_chart_once = false

    /// Increment the flag to recalculate cached data on tree items
    /// Tree items use this number + their local copy of it to track if they have refreshed their data yet
    let mutable cache_flag = 0

    let switch_chart (chart_meta: ChartMeta, context: LibraryContext, group_name: string, group_ctx: LibraryGroupContext) : unit =
        if not (Transitions.in_progress()) then
            SelectedChart.change (chart_meta, context, true)
            Selection.clear ()
            selected_chart <- chart_meta.Hash
            expanded_group <- group_name, group_ctx
            selected_group <- group_name, group_ctx
            scroll_to <- ScrollTo.Chart

    let mutable multi_selection: MultiSelection option = None

    let select_multiple (items: (ChartMeta * LibraryContext) seq) : unit =
        match multi_selection with
        | Some s -> s.Select items
        | None -> multi_selection <- Some (MultiSelection.Create items)

    let deselect_multiple (items: (ChartMeta * LibraryContext) seq) : unit =
        match multi_selection with
        | None -> ()
        | Some s ->
            s.Deselect items
            if s.IsEmpty then multi_selection <- None

[<AbstractClass>]
type private TreeItem() =
    abstract member Bounds: float32 -> Rect
    abstract member Selected: bool
    abstract member Spacing: float32

    member this.CheckBounds(top: float32, origin: float32, originB: float32, if_visible: Rect -> unit) =
        let bounds = this.Bounds(top + this.Spacing * 0.5f)

        if bounds.Bottom > origin && top < originB then
            if_visible bounds

        top + bounds.Height + this.Spacing

    member this.LeftClick(origin: float32) =
        TreeState.click_debounce <= 0
        && Mouse.released Mouse.LEFT
        && TreeState.drag_scroll_distance <= DRAG_THRESHOLD
        && Mouse.y () > origin

    member this.RightClick(origin: float32) =
        TreeState.click_debounce <= 0
        && Mouse.released Mouse.RIGHT
        && TreeState.drag_scroll_distance <= DRAG_THRESHOLD
        && Mouse.y () > origin