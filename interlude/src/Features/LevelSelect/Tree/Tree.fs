namespace Interlude.Features.LevelSelect

open System.Linq
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Sorting
open Prelude.Data.Library.Collections
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay

open TreeState

module Tree =

    let mutable private groups: GroupItem list = []
    let mutable private last_item: ChartItem option = None
    let mutable is_empty = false

    let refresh () =
        // fetch groups
        let library_groups =
            let ctx: LibraryViewContext =
                {
                    Rate = SelectedChart.rate.Value
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    ScoreDatabase = Content.Scores
                    Library = Content.Library
                }

            match options.LibraryMode.Value with
            | LibraryMode.Collections ->
                get_collection_groups LevelSelect.filter sorting_modes.[options.ChartSortMode.Value] ctx
            | LibraryMode.Table ->
                match Content.Table with
                | Some table ->
                    get_table_groups LevelSelect.filter sorting_modes.[options.ChartSortMode.Value] table ctx
                | None -> get_empty_view ()
            | LibraryMode.All ->
                get_groups
                    LevelSelect.filter
                    grouping_modes.[options.ChartGroupMode.Value]
                    sorting_modes.[options.ChartSortMode.Value]
                    ctx
        // if exactly 1 result, switch to it
        if library_groups.Count = 1 then
            let g = library_groups.Keys.First()

            if library_groups.[g].Charts.Count = 1 then
                let cc, context = library_groups.[g].Charts.[0]

                if cc.Key <> selected_chart then
                    switch_chart (cc, context, snd g)
        // build groups ui
        last_item <- None

        groups <-
            library_groups.Keys
            |> Seq.sortBy (fun (index, group_name) -> (index, group_name.ToLower()))
            |> if options.ChartGroupReverse.Value then Seq.rev else id
            |> Seq.map (fun (index, group_name) ->
                library_groups.[(index, group_name)].Charts
                |> Seq.map (fun (cc, context) ->
                    // todo: extract this into a callable function to react to chart changes with
                    match SelectedChart.CACHE_DATA with
                    | None -> ()
                    | Some current_cc ->
                        if current_cc.Key = cc.Key && (context = LibraryContext.None || context = SelectedChart.LIBRARY_CTX) then
                            selected_chart <- current_cc.Key
                            selected_group <- group_name

                    let i = ChartItem(group_name, cc, context)
                    last_item <- Some i
                    i
                )
                |> if options.ChartSortReverse.Value then Seq.rev else id
                |> ResizeArray
                |> fun l -> GroupItem(group_name, l, library_groups.[(index, group_name)].Context)
            )
            |> List.ofSeq

        is_empty <- List.isEmpty groups
        cache_flag <- 0
        expanded_group <- selected_group
        scroll_to <- ScrollTo.Chart
        click_cooldown <- 500.0

    do
        LevelSelect.on_refresh_all.Add refresh
        LevelSelect.on_refresh_details.Add(fun () -> cache_flag <- cache_flag + 1)

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

    let finish_drag_scroll () = currently_drag_scrolling <- false

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
        scroll_pos.Update(elapsed_ms) |> ignore

        if Dialog.exists () then
            ()
        elif (%%"context_menu").Tapped() && SelectedChart.CACHE_DATA.IsSome then
            ChartContextMenu(SelectedChart.CACHE_DATA.Value, SelectedChart.LIBRARY_CTX).Show()
        else

            if (%%"up").Tapped() && expanded_group <> "" then
                scroll_to <- ScrollTo.Pack expanded_group
                expanded_group <- ""

            if (%%"down").Tapped() && expanded_group = "" && selected_group <> "" then
                expanded_group <- selected_group
                scroll_to <- ScrollTo.Pack expanded_group

            let bottom_edge =
                List.fold (fun t (i: GroupItem) -> i.Update(t, origin, originB, elapsed_ms)) scroll_pos.Value groups

            let total_height = originB - origin
            let tree_height = bottom_edge - scroll_pos.Value

            let my = Mouse.y ()

            if currently_drag_scrolling then
                update_drag_scroll (origin, total_height, tree_height)
            elif my < originB && my > origin && (Mouse.left_click () || Mouse.right_click ()) then
                start_drag_scroll ()

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

        let total_height = originB - origin
        let tree_height = bottom_edge - scroll_pos.Value
        let lb = total_height - tree_height - origin
        let ub = 20.0f + origin
        let scroll_bar_pos = -(scroll_pos.Value - ub) / (ub - lb) * (total_height - 40.0f)

        Draw.rect
            (Rect.Create(
                Viewport.vwidth - 10.0f,
                origin + 10.0f + scroll_bar_pos,
                Viewport.vwidth - 5.0f,
                origin + 30.0f + scroll_bar_pos
            ))
            Color.White
