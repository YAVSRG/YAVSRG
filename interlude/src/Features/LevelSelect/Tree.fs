namespace Interlude.Features.LevelSelect

open System
open System.Linq
open OpenTK.Mathematics
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Collections
open Interlude.UI
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay

[<Struct>]
[<RequireQualifiedAccess>]
type ScrollTo =
    | Nothing
    | Chart
    | Pack of string

module Tree =

    /// Group's name = this string => Selected chart is in this group
    let mutable private selected_group = ""
    /// Chart's filepath = this string && context_index match => It's the selected chart
    let mutable private selected_chart = ""
    /// Group's name = this string => That group is expanded in level select
    /// Only one group can be expanded at a time, and it is independent of the "selected" group
    let mutable private expanded_group = ""

    let private scroll_pos = Animation.Fade 300.0f

    let private scroll (amount: float32) =
        scroll_pos.Target <- scroll_pos.Value + amount

    /// Set this value to have it "consumed" in the next frame by a level select item with sufficient knowledge to do so
    let mutable private scroll_to = ScrollTo.Nothing

    let mutable private currently_drag_scrolling = false
    let mutable private drag_scroll_distance = 0.0f
    let mutable private drag_scroll_position = 0.0f
    let mutable private click_cooldown = 0.0

    let private DRAG_THRESHOLD = 40.0f
    let private DRAG_LEFTCLICK_SCALE = 1.75f

    /// Increment the flag to recalculate cached data on tree items
    /// Tree items use this number + their local copy of it to track if they have refreshed their data yet
    let mutable private cache_flag = 0

    // future todo: different color settings?
    let private color_func: Bests option -> Color =
        function
        | None -> Colors.grey_2.O2
        | Some b -> Colors.white.O2

    let private get_pb (bests: PersonalBests<'T>) (color_func: 'T -> Color) (format: 'T -> string) =
        match PersonalBests.get_best_above_with_rate rate.Value bests with
        | Some(v, r) -> Some(v, r, color_func v, format v)
        | None ->

        match PersonalBests.get_best_below_with_rate rate.Value bests with
        | Some(v, r) -> Some(v, r, Colors.white.O2, format v)
        | None -> None

    let switch_chart (cc, context, group_name) =
        if Transitions.active then
            ()
        else
            Chart.change (cc, context)
            Selection.clear ()
            selected_chart <- cc.Key
            expanded_group <- group_name
            selected_group <- group_name
            scroll_to <- ScrollTo.Chart

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

    let CHART_HEIGHT = 90.0f
    let GROUP_HEIGHT = 65.0f

    type private ChartItem(group_name: string, cc: CachedChart, context: LibraryContext) =
        inherit TreeItem()

        let hover = Animation.Fade 0.0f
        let mutable last_cached_flag = -1
        let mutable color = Color.Transparent
        let mutable chart_save_data = None
        let mutable personal_bests: Bests option = None
        let mutable grade = None
        let mutable lamp = None
        let mutable markers = ""

        let update_cached_info () =
            last_cached_flag <- cache_flag

            if chart_save_data.IsNone then
                chart_save_data <- Scores.get cc.Hash

            match chart_save_data with
            | Some d when d.PersonalBests.ContainsKey Rulesets.current_hash ->
                personal_bests <- Some d.PersonalBests.[Rulesets.current_hash]
                grade <- get_pb personal_bests.Value.Grade Rulesets.current.GradeColor Rulesets.current.GradeName
                lamp <- get_pb personal_bests.Value.Lamp Rulesets.current.LampColor Rulesets.current.LampName
            | _ -> ()

            color <- color_func personal_bests

            markers <-
                if options.LibraryMode.Value <> LibraryMode.Collections then
                    match Collections.current with
                    | Some(Folder c) -> if c.Contains cc then c.Icon.Value + " " else ""
                    | Some(Playlist p) -> if p.Contains cc then p.Icon.Value + " " else ""
                    | None -> ""
                else
                    ""
                + match chart_save_data with
                  | Some c when c.Comment <> "" -> Icons.comment
                  | _ -> ""

        override this.Bounds(top) =
            Rect.Create(Viewport.vwidth * 0.4f, top, Viewport.vwidth, top + CHART_HEIGHT)

        override this.Selected =
            selected_chart = cc.Key
            && (context = LibraryContext.None || Chart.LIBRARY_CTX = context)

        override this.Spacing = 5.0f
        member this.Chart = cc

        member this.Select() = switch_chart (cc, context, group_name)

        member private this.OnDraw(bounds: Rect) =
            let {
                    Rect.Left = left
                    Top = top
                    Right = right
                    Bottom = bottom
                } =
                bounds

            // draw base
            let accent = Palette.color (80 + int (hover.Value * 40.0f), 1.0f, 0.4f)

            Draw.rect
                bounds
                (if this.Selected then
                     !*Palette.MAIN_100
                 else
                     Colors.shadow_1.O2)

            let stripe_length = (right - left) * (0.4f + 0.6f * hover.Value)

            Draw.quad
                (Quad.create
                 <| new Vector2(left, top)
                 <| new Vector2(left + stripe_length, top)
                 <| new Vector2(left + stripe_length, bottom - 25.0f)
                 <| new Vector2(left, bottom - 25.0f))
                (struct (accent, Color.Transparent, Color.Transparent, accent))
                Sprite.DEFAULT_QUAD

            let border = bounds.Expand(5.0f, 0.0f)
            let border_color = if this.Selected then !*Palette.LIGHT else color

            if border_color.A > 0uy then
                Draw.rect (border.SliceLeft 5.0f) border_color

            // draw pbs
            let disp (data: 'T * float32 * Color * string) (pos: float32) =
                let _, rate, color, formatted = data
                let rate_label = sprintf "(%.2fx)" rate

                if color.A > 0uy then
                    Draw.rect (Rect.Create(right - pos - 40.0f, top, right - pos + 40.0f, bottom)) accent

                    Text.draw_aligned_b (
                        Style.font,
                        formatted,
                        20.0f,
                        right - pos,
                        top + 8.0f,
                        (color, Color.Black),
                        0.5f
                    )

                    Text.draw_aligned_b (
                        Style.font,
                        rate_label,
                        14.0f,
                        right - pos,
                        top + 35.0f,
                        (color, Color.Black),
                        0.5f
                    )

            if personal_bests.IsSome then
                disp grade.Value 290.0f
                disp lamp.Value 165.0f

            // draw text
            Draw.rect (bounds.SliceBottom 25.0f) Colors.shadow_1.O1
            Text.draw_b (Style.font, cc.Title, 23.0f, left + 5f, top, Colors.text)

            Text.draw_b (
                Style.font,
                sprintf "%s  •  %s" cc.Artist cc.Creator,
                18.0f,
                left + 5f,
                top + 34.0f,
                Colors.text_subheading
            )
            // todo: option between subtitle preference, source preference and just always showing difficulty name
            Text.draw_b (
                Style.font,
                cc.Subtitle |> Option.defaultValue cc.DifficultyName,
                15.0f,
                left + 5f,
                top + 65.0f,
                Colors.text_subheading
            )

            Text.draw_aligned_b (Style.font, markers, 25.0f, right - 65.0f, top + 15.0f, Colors.text, Alignment.CENTER)

            if Comments.fade.Value > 0.01f && chart_save_data.IsSome && chart_save_data.Value.Comment <> "" then
                Draw.rect bounds (Palette.color (Comments.fade.Alpha * 2 / 3, 1.0f, 0.0f))

                Text.fill_b (
                    Style.font,
                    chart_save_data.Value.Comment,
                    bounds.Shrink(30.0f, 15.0f),
                    (Colors.white.O4a Comments.fade.Alpha, Colors.shadow_1.O4a Comments.fade.Alpha),
                    Alignment.CENTER
                )

        member this.Draw(top, origin, originB) =
            this.CheckBounds(top, origin, originB, this.OnDraw)

        member private this.OnUpdate(origin, bounds, elapsed_ms) =

            if last_cached_flag < cache_flag then
                update_cached_info ()

            if Mouse.hover bounds then
                hover.Target <- 1.0f

                if this.LeftClick(origin) then
                    if this.Selected then LevelSelect.play () else this.Select()
                elif this.RightClick(origin) then
                    ChartContextMenu(cc, context).Show()
                elif (%%"delete").Tapped() then
                    ChartContextMenu.ConfirmDelete(cc, false)
            else
                hover.Target <- 0.0f

            hover.Update(elapsed_ms) |> ignore

        member this.Update(top, origin, originB, elapsed_ms) =
            if scroll_to = ScrollTo.Chart && group_name = selected_group && this.Selected then
                scroll (-top + 500.0f)
                scroll_to <- ScrollTo.Nothing

            this.CheckBounds(top, origin, originB, (fun b -> this.OnUpdate(origin, b, elapsed_ms)))

    type private GroupItem(name: string, items: ResizeArray<ChartItem>, context: LibraryGroupContext) =
        inherit TreeItem()

        override this.Bounds(top) =
            Rect.Create(Viewport.vwidth * 0.5f, top, Viewport.vwidth - 15.0f, top + GROUP_HEIGHT)

        override this.Selected = selected_group = name
        override this.Spacing = 20.0f

        member this.Items = items
        member this.Expanded = expanded_group = name

        member this.SelectFirst() = items.First().Select()
        member this.SelectLast() = items.Last().Select()

        member private this.OnDraw(bounds: Rect) =
            Draw.rect (bounds.Translate(10.0f, 10.0f)) (Palette.color (255, 0.2f, 0.0f))
            Background.draw (bounds, (Color.FromArgb(40, 40, 40)), 1.5f)

            Draw.rect
                bounds
                (if this.Selected then
                     Palette.color (120, 1.0f, 0.2f)
                 else
                     Palette.color (100, 0.7f, 0.0f))

            Text.fill_b (Style.font, name, bounds.Shrink 5.0f, Colors.text, 0.5f)

        member this.Draw(top, origin, originB) =
            let b = this.CheckBounds(top, origin, originB, this.OnDraw)

            if this.Expanded then
                let h = CHART_HEIGHT + 5.0f

                let mutable index =
                    if scroll_to <> ScrollTo.Nothing then
                        0
                    else
                        (origin - b) / h |> floor |> int |> max 0

                let mutable p = b + float32 index * h

                while (scroll_to <> ScrollTo.Nothing || p < originB) && index < items.Count do
                    p <- items.[index].Draw(p, origin, originB)
                    index <- index + 1

                let b2 = b + float32 items.Count * h

                if b < origin && b2 > origin then
                    Text.draw_aligned_b (
                        Style.font,
                        name,
                        20.0f,
                        Viewport.vwidth - 20f,
                        origin + 10.0f,
                        Colors.text,
                        1.0f
                    )

                b2
            else
                b

        member private this.OnUpdate(origin, bounds, elapsed_ms) =
            if Mouse.hover bounds then
                if this.LeftClick(origin) then
                    if this.Expanded then
                        expanded_group <- ""
                    else
                        (expanded_group <- name
                         scroll_to <- ScrollTo.Pack name)
                elif this.RightClick(origin) then
                    GroupContextMenu.Show(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), context)
                elif (%%"delete").Tapped() then
                    GroupContextMenu.ConfirmDelete(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), false)

        member this.Update(top, origin, originB, elapsed_ms) =
            match scroll_to with
            | ScrollTo.Pack s when s = name ->
                if this.Expanded then
                    scroll (-top + origin + 185.0f)
                else
                    scroll (-top + origin + 400.0f)

                scroll_to <- ScrollTo.Nothing
            | _ -> ()

            let b =
                this.CheckBounds(top, origin, originB, (fun b -> this.OnUpdate(origin, b, elapsed_ms)))

            if this.Expanded then
                let h = CHART_HEIGHT + 5.0f

                let mutable index =
                    if scroll_to <> ScrollTo.Nothing then
                        0
                    else
                        (origin - b) / h |> floor |> int |> max 0

                let mutable p = b + float32 index * h

                while (scroll_to <> ScrollTo.Nothing || p < originB) && index < items.Count do
                    p <- items.[index].Update(p, origin, originB, elapsed_ms)
                    index <- index + 1

                b + float32 items.Count * (CHART_HEIGHT + 5.0f)
            else
                b


    let mutable filter: Filter = []
    let mutable private groups: GroupItem list = []
    let mutable private last_item: ChartItem option = None
    let mutable is_empty = false


    let refresh () =
        // fetch groups
        let library_groups =
            let ctx: GroupContext =
                {
                    Rate = rate.Value
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                }

            match options.LibraryMode.Value with
            | LibraryMode.Collections -> get_collection_groups sorting_modes.[options.ChartSortMode.Value] filter
            | LibraryMode.Table -> get_table_groups sorting_modes.[options.ChartSortMode.Value] filter
            | LibraryMode.All ->
                get_groups
                    ctx
                    grouping_modes.[options.ChartGroupMode.Value]
                    sorting_modes.[options.ChartSortMode.Value]
                    filter
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
            |> Seq.sortBy(fun (index, group_name) -> (index, group_name.ToLower()))
            |> if options.ChartGroupReverse.Value then Seq.rev else id
            |> Seq.map (fun (index, group_name) ->
                library_groups.[(index, group_name)].Charts
                |> Seq.map (fun (cc, context) ->
                    match Chart.CACHE_DATA with
                    | None -> ()
                    | Some c ->
                        if c.Key = cc.Key && (context = LibraryContext.None || context = Chart.LIBRARY_CTX) then
                            selected_chart <- c.Key
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
                groups.First().Items.First().Select()
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
        else

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

            if (%%"up").Tapped() && expanded_group <> "" then
                scroll_to <- ScrollTo.Pack expanded_group
                expanded_group <- ""

            if (%%"down").Tapped() && expanded_group = "" && selected_group <> "" then
                expanded_group <- selected_group
                scroll_to <- ScrollTo.Pack expanded_group
            elif (%%"context_menu").Tapped() && Chart.CACHE_DATA.IsSome then
                ChartContextMenu(Chart.CACHE_DATA.Value, Chart.LIBRARY_CTX).Show()

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
