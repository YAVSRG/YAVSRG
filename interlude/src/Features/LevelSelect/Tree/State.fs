namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay
open Interlude.UI
open Interlude.Features.Gameplay

[<Struct>]
[<RequireQualifiedAccess>]
type private ScrollTo =
    | Nothing
    | Chart
    | Pack of string

module private TreeState =

    /// Group's name = this string => Selected chart is in this group
    let mutable selected_group = ""
    /// Chart's filepath = this string && context_index match => It's the selected chart
    let mutable selected_chart = ""
    /// Group's name = this string => That group is expanded in level select
    /// Only one group can be expanded at a time, and it is independent of the "selected" group
    let mutable expanded_group = ""

    let scroll_pos = Animation.Fade 300.0f

    let scroll (amount: float32) =
        scroll_pos.Target <- scroll_pos.Value + amount

    /// Set this value to have it "consumed" in the next frame by a level select item with sufficient knowledge to do so
    let mutable scroll_to = ScrollTo.Nothing

    let mutable currently_drag_scrolling = false
    let mutable drag_scroll_distance = 0.0f
    let mutable drag_scroll_position = 0.0f
    let mutable click_cooldown = 0.0

    let DRAG_THRESHOLD = 40.0f
    let DRAG_LEFTCLICK_SCALE = 1.75f

    /// Increment the flag to recalculate cached data on tree items
    /// Tree items use this number + their local copy of it to track if they have refreshed their data yet
    let mutable cache_flag = 0

    // future todo: different color settings?
    let color_func: Bests option -> Color =
        function
        | None -> Colors.grey_2.O2
        | Some b -> Colors.white.O2

    let get_pb (bests: PersonalBests<'T>) (color_func: 'T -> Color) (format: 'T -> string) =
        match PersonalBests.get_best_above_with_rate SelectedChart.rate.Value bests with
        | Some(v, r) -> Some(v, r, color_func v, format v)
        | None ->

        match PersonalBests.get_best_below_with_rate SelectedChart.rate.Value bests with
        | Some(v, r) -> Some(v, r, Colors.white.O2, format v)
        | None -> None

    let CHART_HEIGHT = 90.0f
    let GROUP_HEIGHT = 55.0f

    // todo: react to the event of switching instead of doing stuff here
    let switch_chart (cc, context, group_name) =
        if not Transitions.active then
            SelectedChart.change (cc, context, true)
            Selection.clear ()
            selected_chart <- cc.Key
            expanded_group <- group_name
            selected_group <- group_name
            scroll_to <- ScrollTo.Chart

    [<AbstractClass>]
    type TreeItem() =
        abstract member Bounds: float32 -> Rect
        abstract member Selected: bool
        abstract member Spacing: float32

        member this.CheckBounds(top: float32, origin: float32, originB: float32, if_visible: Rect -> unit) =
            let bounds = this.Bounds(top + this.Spacing * 0.5f)

            if bounds.Bottom > origin && top < originB then
                if_visible bounds

            top + bounds.Height + this.Spacing

        member this.LeftClick(origin) =
            click_cooldown <= 0
            && Mouse.released Mouse.LEFT
            && drag_scroll_distance <= DRAG_THRESHOLD
            && Mouse.y () > origin

        member this.RightClick(origin) =
            click_cooldown <= 0
            && Mouse.released Mouse.RIGHT
            && drag_scroll_distance <= DRAG_THRESHOLD
            && Mouse.y () > origin