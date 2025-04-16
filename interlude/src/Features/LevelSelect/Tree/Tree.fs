namespace Interlude.Features.LevelSelect

open System.Linq
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay

module Tree =

    let private tree_ctx = TreeContext.Create

    let debounce() = tree_ctx.ClickDebounce <- 500.0
    let multi_selection() = tree_ctx.MultiSelection
    let clear_multi_selection() = tree_ctx.MultiSelection <- None

    let mutable private groups: GroupItem list = []
    let mutable private last_item: ChartItem option = None
    let mutable is_empty = false
    let scroll_fade = Animation.Fade 0.0f

    let private find_selected_chart_in_tree () : unit =
        match SelectedChart.CACHE_DATA with
        | None -> ()
        | Some chart_meta ->

        seq {
            for group in groups do
                for chart in group.Items do
                    if chart.Chart.Hash = chart_meta.Hash && chart.Context.Matches SelectedChart.LIBRARY_CTX then
                        yield chart.Context, group
            for group in groups do
                for chart in group.Items do
                    if chart.Chart.Hash = chart_meta.Hash && chart.Context.SoftMatches SelectedChart.LIBRARY_CTX then
                        yield chart.Context, group
        }
        |> Seq.tryHead
        |> function
        | Some (ctx, group) ->
            SelectedChart.LIBRARY_CTX <- ctx
            tree_ctx.SelectedChart <- chart_meta.Hash
            tree_ctx.SelectedGroup <- group.Name, group.Context
            tree_ctx.ExpandedGroup <- tree_ctx.SelectedGroup
            tree_ctx.ScrollTo <- ScrollTo.Chart
        | None ->
            tree_ctx.SelectedChart <- ""
            tree_ctx.SelectedGroup <- "", LibraryGroupContext.None

    let refresh () : unit =
        // fetch groups
        let library_groups =
            let ctx: LibraryViewContext =
                {
                    Rate = SelectedChart.rate.Value
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    UserDatabase = Content.UserData
                    Library = Content.Library
                }

            LibraryView.get_groups
                LevelSelect.filter
                Grouping.modes.[options.ChartGroupMode.Value]
                options.ChartGroupReverse.Value
                Sorting.modes.[options.ChartSortMode.Value]
                options.ChartSortReverse.Value
                options.TreeAlwaysShowCollections.Value
                Content.Table
                ctx
            |> Seq.toArray

        // if exactly 1 result, switch to it
        if library_groups.Length = 1 then
            let group_name, group = library_groups.[0]

            if group.Charts.Length = 1 then
                let chart_meta, context = group.Charts.[0]

                if chart_meta.Hash <> tree_ctx.SelectedChart then
                    tree_ctx.SelectChart(chart_meta, context, group_name, group.Context)
        // build groups ui
        last_item <- None

        groups <-
            library_groups
            |> Seq.map (fun (group_name, group) ->
                group.Charts
                |> Seq.map (fun (chart_meta, context) ->
                    let i = ChartItem(tree_ctx, group_name, group.Context, chart_meta, context)
                    last_item <- Some i
                    i
                )
                |> ResizeArray
                |> fun l -> GroupItem(tree_ctx, group_name, l, group.Context)
            )
            |> List.ofSeq

        find_selected_chart_in_tree()

        is_empty <- List.isEmpty groups
        tree_ctx.CacheFlag <- 0
        tree_ctx.ClickDebounce <- 500.0
        tree_ctx.MultiSelection <- None

    do
        LevelSelect.on_refresh_all.Add refresh
        LevelSelect.on_refresh_details.Add(fun () -> tree_ctx.CacheFlag <- tree_ctx.CacheFlag + 1)
        SelectedChart.on_chart_change_started.Add(fun info -> if info.ChartMeta.Hash <> tree_ctx.SelectedChart then find_selected_chart_in_tree())

    let previous () : unit =
        match last_item with
        | Some l ->
            let mutable searching = true
            let mutable last = l

            for g in groups do
                for c in g.Items do
                    if c.Selected && searching then
                        last.Select()
                        searching <- false
                    else
                        last <- c

            if searching then
                l.Select()
        | None -> ()

    let next () : unit =
        match last_item with
        | Some l ->
            let mutable found = false
            let mutable select_the_next_one = l.Selected

            for g in groups do
                for c in g.Items do
                    if select_the_next_one then
                        c.Select()
                        select_the_next_one <- false
                        found <- true
                    elif c.Selected then
                        select_the_next_one <- true

            if not found then
                groups.First().SelectFirst()
        | None -> ()

    let previous_group () : unit =
        match last_item with
        | Some _ ->
            let mutable looping = true
            let mutable last = groups.Last()

            for g in groups do
                if g.Selected && looping then
                    last.SelectFirst()
                    looping <- false
                else
                    last <- g
        | None -> ()

    let next_group () : unit =
        match last_item with
        | Some _ ->
            let mutable select_the_next_one = groups.Last().Selected

            for g in groups do
                if select_the_next_one then
                    g.SelectFirst()
                    select_the_next_one <- false
                elif g.Selected then
                    select_the_next_one <- true
        | None -> ()

    let top_of_group () : unit =
        for g in groups do
            if g.Selected then
                g.SelectFirst()

    let bottom_of_group () : unit =
        for g in groups do
            if g.Selected then
                g.SelectLast()

    let start_drag_scroll () : unit =
        tree_ctx.CurrentlyDragScrolling <- true
        tree_ctx.DragScrollPosition <- Mouse.y ()
        tree_ctx.DragScrollDistance <- 0.0f
        scroll_fade.Target <- 1.0f

    let finish_drag_scroll () : unit =
        tree_ctx.CurrentlyDragScrolling <- false
        scroll_fade.Target <- 0.0f

    let update_drag_scroll (origin: float32, total_height: float32, tree_height: float32) : unit =
        let d = Mouse.y () - tree_ctx.DragScrollPosition
        tree_ctx.DragScrollPosition <- Mouse.y ()
        tree_ctx.DragScrollDistance <- tree_ctx.DragScrollDistance + abs d

        if Mouse.held Mouse.RIGHT then
            if tree_ctx.DragScrollDistance > DRAG_THRESHOLD then
                tree_ctx.ScrollPosition.Target <- -(Mouse.y () - origin) / total_height * tree_height
        elif Mouse.held Mouse.LEFT then
            if tree_ctx.DragScrollDistance > DRAG_THRESHOLD then
                tree_ctx.ScrollPosition.Target <- tree_ctx.ScrollPosition.Target + d * DRAG_LEFTCLICK_SCALE
        else
            finish_drag_scroll ()

    let update (origin: float32, originB: float32, elapsed_ms: float) : unit =
        tree_ctx.ScrollPosition.Update elapsed_ms
        scroll_fade.Update elapsed_ms

        if Dialog.exists () then
            ()
        elif (%%"context_menu").Pressed() && SelectedChart.CACHE_DATA.IsSome then
            match tree_ctx.MultiSelection with
            | Some s -> s.ShowActions()
            | None ->

            match SelectedChart.CACHE_DATA with
            | Some chart_meta -> ChartContextMenu(chart_meta, SelectedChart.LIBRARY_CTX).Show()
            | _ -> ()
        elif (%%"clear_multi_select").Pressed() then tree_ctx.MultiSelection <- None
        else

            if (%%"up").Pressed() && tree_ctx.ExpandedGroup <> ("", LibraryGroupContext.None) then
                tree_ctx.ScrollTo <- ScrollTo.Group tree_ctx.ExpandedGroup
                tree_ctx.ExpandedGroup <- ("", LibraryGroupContext.None)

            if (%%"down").Pressed() && tree_ctx.ExpandedGroup = ("", LibraryGroupContext.None) && tree_ctx.SelectedGroup <> ("", LibraryGroupContext.None) then
                tree_ctx.ExpandedGroup <- tree_ctx.SelectedGroup
                tree_ctx.ScrollTo <- ScrollTo.Group tree_ctx.ExpandedGroup

            let bottom_edge =
                List.fold (fun t (i: GroupItem) -> i.Update(t, origin, originB, elapsed_ms)) tree_ctx.ScrollPosition.Value groups

            let total_height = originB - origin
            let tree_height = bottom_edge - tree_ctx.ScrollPosition.Value

            let mx, my = Mouse.pos ()

            if tree_ctx.CurrentlyDragScrolling then
                update_drag_scroll (origin, total_height, tree_height)
            elif mx > Render.width() * 0.2f && my < originB && my > origin && (Mouse.left_clicked () || Mouse.right_clicked ()) then
                start_drag_scroll ()
            elif mx < Render.width() * 0.2f then
                if not tree_ctx.ScrollToChartOnce then
                    tree_ctx.ScrollTo <- ScrollTo.Chart
                    tree_ctx.ScrollToChartOnce <- true
            else
                tree_ctx.ScrollToChartOnce <- false

            if tree_ctx.ClickDebounce > 0.0 then
                tree_ctx.ClickDebounce <- tree_ctx.ClickDebounce - elapsed_ms

            let lo = total_height - tree_height - origin
            let hi = 20.0f + origin
            tree_ctx.ScrollPosition.Target <- min hi (max lo (tree_ctx.ScrollPosition.Target + Mouse.scroll () * 100.0f))

            if tree_ctx.ScrollPosition.Value < lo then
                tree_ctx.ScrollPosition.Value <- lo
            elif tree_ctx.ScrollPosition.Value > hi then
                tree_ctx.ScrollPosition.Value <- hi

    let draw (origin: float32, originB: float32) : unit =

        let screen_bounds = Render.bounds()

        Render.stencil_create false
        Render.rect_edges 0.0f origin screen_bounds.Width originB Color.Transparent

        Render.stencil_begin_draw ()

        let bottom_edge =
            List.fold (fun t (i: GroupItem) -> i.Draw(t, origin, originB)) tree_ctx.ScrollPosition.Value groups

        Render.stencil_finish ()

        Render.rect_edges
            (screen_bounds.Right - 10.0f)
            (origin + 5.0f)
            (screen_bounds.Right)
            (originB - 50.0f)
            (Colors.shadow_2.O3a scroll_fade.Alpha)

        let total_height = originB - origin
        let tree_height = bottom_edge - tree_ctx.ScrollPosition.Value
        let lower_bound = total_height - tree_height - origin
        let upper_bound = 20.0f + origin
        let scroll_bar_pos = -(tree_ctx.ScrollPosition.Value - upper_bound) / (upper_bound - lower_bound) * (total_height - 30.0f - 50.0f)

        Render.rect_edges
            (screen_bounds.Right - 10.0f)
            (origin + 5.0f + scroll_bar_pos)
            screen_bounds.Right
            (origin + 30.0f + scroll_bar_pos)
            Colors.white