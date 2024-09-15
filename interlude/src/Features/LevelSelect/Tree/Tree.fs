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

open TreeState

module Tree =

    let mutable private groups: GroupItem list = []
    let mutable private last_item: ChartItem option = None
    let mutable is_empty = false
    let scroll_fade = Animation.Fade 0.0f

    let private find_selected_chart_in_tree () =
        match SelectedChart.CACHE_DATA with
        | None -> ()
        | Some current_cc ->

        seq {
            for group in groups do
                for chart in group.Items do
                    if chart.Chart.Hash = current_cc.Hash && chart.Context.Matches SelectedChart.LIBRARY_CTX then
                        yield chart.Context, group
            for group in groups do
                for chart in group.Items do
                    if chart.Chart.Hash = current_cc.Hash && chart.Context.SoftMatches SelectedChart.LIBRARY_CTX then
                        yield chart.Context, group
        }
        |> Seq.tryHead
        |> function
        | Some (ctx, group) -> 
            SelectedChart.LIBRARY_CTX <- ctx
            selected_chart <- current_cc.Hash
            selected_group <- group.Name, group.Context
            expanded_group <- selected_group
            scroll_to <- ScrollTo.Chart
        | None -> ()


    let refresh () =
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
                let cc, context = group.Charts.[0]

                if cc.Hash <> selected_chart then
                    switch_chart (cc, context, group_name, group.Context)
        // build groups ui
        last_item <- None

        groups <-
            library_groups
            |> Seq.map (fun (group_name, group) ->
                group.Charts
                |> Seq.map (fun (cc, context) ->
                    let i = ChartItem(group_name, group.Context, cc, context)
                    last_item <- Some i
                    i
                )
                |> ResizeArray
                |> fun l -> GroupItem(group_name, l, group.Context)
            )
            |> List.ofSeq

        find_selected_chart_in_tree()

        is_empty <- List.isEmpty groups
        cache_flag <- 0
        click_cooldown <- 500.0

    do
        LevelSelect.on_refresh_all.Add refresh
        LevelSelect.on_refresh_details.Add(fun () -> cache_flag <- cache_flag + 1)
        SelectedChart.on_chart_change_started.Add(fun info -> if info.CacheInfo.Hash <> selected_chart then find_selected_chart_in_tree())

    let previous () =
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

    let next () =
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

    let previous_group () =
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

    let next_group () =
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

    let top_of_group () =
        for g in groups do
            if g.Selected then
                g.SelectFirst()

    let bottom_of_group () =
        for g in groups do
            if g.Selected then
                g.SelectLast()

    let start_drag_scroll () =
        currently_drag_scrolling <- true
        drag_scroll_position <- Mouse.y ()
        drag_scroll_distance <- 0.0f
        scroll_fade.Target <- 1.0f

    let finish_drag_scroll () = 
        currently_drag_scrolling <- false
        scroll_fade.Target <- 0.0f

    let update_drag_scroll (origin, total_height, tree_height) =
        let d = Mouse.y () - drag_scroll_position
        drag_scroll_position <- Mouse.y ()
        drag_scroll_distance <- drag_scroll_distance + abs d

        if Mouse.held Mouse.RIGHT then
            if drag_scroll_distance > DRAG_THRESHOLD then
                scroll_pos.Target <- -(Mouse.y () - origin) / total_height * tree_height
        elif Mouse.held Mouse.LEFT then
            if drag_scroll_distance > DRAG_THRESHOLD then
                scroll_pos.Target <- scroll_pos.Target + d * DRAG_LEFTCLICK_SCALE
        else
            finish_drag_scroll ()

    let update (origin: float32, originB: float32, elapsed_ms: float) =
        scroll_pos.Update elapsed_ms
        scroll_fade.Update elapsed_ms

        if Dialog.exists () then
            ()
        elif (%%"context_menu").Tapped() && SelectedChart.CACHE_DATA.IsSome then
            ChartContextMenu(SelectedChart.CACHE_DATA.Value, SelectedChart.LIBRARY_CTX).Show()
        else

            if (%%"up").Tapped() && expanded_group <> ("", LibraryGroupContext.None) then
                scroll_to <- ScrollTo.Pack expanded_group
                expanded_group <- ("", LibraryGroupContext.None)

            if (%%"down").Tapped() && expanded_group = ("", LibraryGroupContext.None) && selected_group <> ("", LibraryGroupContext.None) then
                expanded_group <- selected_group
                scroll_to <- ScrollTo.Pack expanded_group

            let bottom_edge =
                List.fold (fun t (i: GroupItem) -> i.Update(t, origin, originB, elapsed_ms)) scroll_pos.Value groups

            let total_height = originB - origin
            let tree_height = bottom_edge - scroll_pos.Value

            let mx, my = Mouse.pos ()

            if currently_drag_scrolling then
                update_drag_scroll (origin, total_height, tree_height)
            elif mx > Viewport.vwidth * 0.2f && my < originB && my > origin && (Mouse.left_click () || Mouse.right_click ()) then
                start_drag_scroll ()
            elif mx < Viewport.vwidth * 0.2f then
                if not scroll_to_chart_once then
                    scroll_to <- ScrollTo.Chart
                    scroll_to_chart_once <- true
            else
                scroll_to_chart_once <- false

            if click_cooldown > 0.0 then
                click_cooldown <- click_cooldown - elapsed_ms

            let lo = total_height - tree_height - origin
            let hi = 20.0f + origin
            scroll_pos.Target <- min hi (max lo (scroll_pos.Target + Mouse.scroll () * 100.0f))

            if scroll_pos.Value < lo then
                scroll_pos.Value <- lo
            elif scroll_pos.Value > hi then
                scroll_pos.Value <- hi

    let draw (origin: float32, originB: float32) =

        Stencil.start_stencilling false

        Draw.rect (Rect.Create(0.0f, origin, Viewport.vwidth, originB)) Color.Transparent

        Stencil.start_drawing ()

        let bottom_edge =
            List.fold (fun t (i: GroupItem) -> i.Draw(t, origin, originB)) scroll_pos.Value groups

        Stencil.finish ()

        Draw.rect 
            (Rect.Create(
                Viewport.bounds.Right - 10.0f,
                origin + 5.0f,
                Viewport.bounds.Right,
                originB - 50.0f
            ))
            (Colors.shadow_2.O3a scroll_fade.Alpha)

        let total_height = originB - origin
        let tree_height = bottom_edge - scroll_pos.Value
        let lower_bound = total_height - tree_height - origin
        let upper_bound = 20.0f + origin
        let scroll_bar_pos = -(scroll_pos.Value - upper_bound) / (upper_bound - lower_bound) * (total_height - 30.0f - 50.0f)

        Draw.rect
            (Rect.Create(
                Viewport.bounds.Right - 10.0f,
                origin + 5.0f + scroll_bar_pos,
                Viewport.bounds.Right,
                origin + 30.0f + scroll_bar_pos
            ))
            Colors.white
