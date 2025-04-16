namespace Interlude.Features.LevelSelect

open System.Linq
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.UI
open Interlude.Content

type private GroupItem(tree_ctx: TreeContext, name: string, items: ResizeArray<ChartItem>, group_ctx: LibraryGroupContext) =
    inherit TreeItem(tree_ctx)

    let display_name = if group_ctx = LibraryGroupContext.Likes then  %"library.likes" else name

    let charts_as_seq = items |> Seq.map (fun i -> i.Chart, i.Context)

    let mutable last_cached_flag = -1
    let select_animation = Animation.Fade(0.0f)
    let mutable label = ""
    let special_color =
        match group_ctx with
        | LibraryGroupContext.None -> None
        | LibraryGroupContext.Pack _ -> None
        | LibraryGroupContext.Table _ -> None
        | LibraryGroupContext.Likes -> Some (Colors.pink, Colors.pink_shadow)
        | LibraryGroupContext.Folder _ -> Some (Colors.cyan, Colors.cyan_shadow)
        | LibraryGroupContext.Playlist _ -> Some (Colors.green, Colors.green_shadow)

    let update_cached_info () =
        last_cached_flag <- tree_ctx.CacheFlag

        label <-
            match group_ctx with
            | LibraryGroupContext.Folder id ->
                match Content.Collections.GetFolder id with
                | Some folder -> sprintf "%s %i" folder.Icon.Value items.Count
                | None -> sprintf "%s %i" Icons.FOLDER items.Count
            | LibraryGroupContext.Playlist id ->
                match Content.Collections.GetPlaylist id with
                | Some playlist ->
                    let duration = items |> Seq.map (fun i -> i.PlaylistDuration) |> Seq.sum

                    sprintf
                        "%s %s    %s %i"
                        Icons.CLOCK
                        (format_duration_ms duration)
                        playlist.Icon.Value
                        items.Count
                | None -> sprintf "%s %i" Icons.FOLDER items.Count
            | LibraryGroupContext.Table id -> // todo: calc some cool table/folder stats to go here
                sprintf "%s %i" Icons.FOLDER items.Count
            | LibraryGroupContext.Likes -> sprintf "%s %i" Icons.HEART_ON items.Count
            | LibraryGroupContext.Pack _ -> sprintf "%s %i" Icons.FOLDER items.Count
            | LibraryGroupContext.None -> sprintf "%s %i" Icons.FOLDER_PLUS items.Count

    do update_cached_info ()

    override this.Bounds(this_top: float32) =
        Rect.FromEdges(
            Render.width() * (TREE_LEFT_SPLIT + 0.1f - 0.05f * select_animation.Value),
            this_top,
            Render.width() - 25.0f,
            this_top + GROUP_HEIGHT
        )

    member this.Selected : bool = tree_ctx.IsGroupSelected(name, group_ctx)
    override this.Spacing : float32 = GROUP_SPACING

    member this.Items : ResizeArray<ChartItem> = items
    member this.Name : string = name
    member this.Context : LibraryGroupContext = group_ctx
    member this.Expanded : bool =  tree_ctx.IsGroupExpanded(name, group_ctx)

    member this.SelectFirst() : unit = items.First().Select()
    member this.SelectLast() : unit = items.Last().Select()

    /// Only called if this group can be seen on screen
    member private this.DrawCulled(bounds: Rect) : unit =

        let color, bg_color =
            match tree_ctx.MultiSelection with
            | Some s when s.GroupAmountSelected(name, group_ctx, charts_as_seq) <> AmountSelected.None ->
                Colors.grey_2.O2, Colors.shadow_2.O2
            | _ ->
            match special_color with
            | Some (fg, bg) -> if this.Selected then fg.O3, bg else fg.O2, bg.O2
            | None ->
                if this.Selected then
                    Palette.color (120, 0.7f + select_animation.Value * 0.3f, select_animation.Value * 0.3f),
                    Palette.color (255, 0.2f, 0.0f)
                else
                    Palette.color (100, 0.7f, 0.0f),
                    Palette.color (255, 0.2f, 0.0f)

        Render.rect (bounds.Translate(10.0f, 10.0f)) bg_color
        Background.draw (bounds, (Color.FromArgb(40, 40, 40)), 1.5f)

        Render.rect bounds color

        match tree_ctx.MultiSelection with
        | Some s ->
            let filled_icon, name_color =
                match s.GroupAmountSelected(name, group_ctx, charts_as_seq) with
                | AmountSelected.None -> Icons.SQUARE, Colors.text_subheading
                | AmountSelected.Some -> Icons.PLUS_SQUARE, Colors.text_yellow_2
                | AmountSelected.All -> Icons.CHECK_SQUARE, Colors.text_yellow_2
            Text.fill_b (Style.font, display_name, bounds.Shrink(15.0f, 5.0f).ShrinkR(150.0f), name_color, Alignment.LEFT)
            Text.fill_b (Style.font, filled_icon, bounds.Shrink(15.0f, 5.0f), Colors.text, Alignment.RIGHT)
            Text.fill_b (Style.font, label, bounds.Shrink(65.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)
        | None ->
            Text.fill_b (Style.font, display_name, bounds.Shrink(15.0f, 5.0f).ShrinkR(100.0f), Colors.text, Alignment.LEFT)
            Text.fill_b (Style.font, label, bounds.Shrink(15.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)

    member this.Draw(this_top: float32, tree_top: float32, tree_bottom: float32) : float32 =
        let next_top = this.IfVisible(this_top, tree_top, tree_bottom, this.DrawCulled)

        if this.Expanded then
            let padded_chart_height = CHART_HEIGHT + CHART_SPACING

            let first_visible_chart_index = (tree_top - next_top) / padded_chart_height |> floor |> int |> max 0
            let mutable chart_index = first_visible_chart_index

            let mutable top_edge = next_top + float32 chart_index * padded_chart_height

            while top_edge < tree_bottom && chart_index < items.Count do
                top_edge <- items.[chart_index].Draw(top_edge, tree_top, tree_bottom)
                chart_index <- chart_index + 1

            let expanded_next_top = next_top + float32 items.Count * padded_chart_height

            if next_top < tree_top && expanded_next_top > tree_top then
                Text.draw_aligned_b (
                    Style.font,
                    name,
                    20.0f,
                    Render.width() - 20f,
                    tree_top + 10.0f,
                    Colors.text,
                    Alignment.RIGHT
                )

            expanded_next_top
        else
            next_top

    /// Only called if the group can be seen on screen
    member private this.UpdateCulled(tree_top: float32, bounds: Rect) : unit =
        if Mouse.hover(bounds) then

            if this.LeftClicked(tree_top) then
                if MULTI_SELECT_KEY.Held() then
                    tree_ctx.ToggleMultiSelect(name, group_ctx, charts_as_seq)
                elif this.Expanded then
                    tree_ctx.ExpandedGroup <- "", LibraryGroupContext.None
                else
                    tree_ctx.ExpandedGroup <- name, group_ctx
                    tree_ctx.ScrollTo <- ScrollTo.Group (name, group_ctx)

            elif this.RightClicked(tree_top) then
                match tree_ctx.MultiSelection with
                | Some s when s.GroupAmountSelected(name, group_ctx, charts_as_seq) <> AmountSelected.None -> s.ShowActions()
                | _ -> GroupContextMenu.Show(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), group_ctx)

            elif (%%"delete").Pressed() then
                match group_ctx with
                | LibraryGroupContext.Folder _ // todo: show prompt for deleting folder/playlist?
                | LibraryGroupContext.Playlist _
                | LibraryGroupContext.Likes
                | LibraryGroupContext.Table _ -> ()
                | LibraryGroupContext.Pack _
                | LibraryGroupContext.None -> GroupContextMenu.ConfirmDelete(items |> Seq.map (fun (x: ChartItem) -> x.Chart), group_ctx, false)

    member this.Update(this_top: float32, tree_top: float32, tree_bottom: float32, elapsed_ms: float) : float32 =
        if last_cached_flag < tree_ctx.CacheFlag then
            update_cached_info ()

        select_animation.Target <- if this.Selected then 1.0f else 0.0f
        select_animation.Update elapsed_ms

        match tree_ctx.ScrollTo with
        | ScrollTo.Group (a, b) when (a, b) = (name, group_ctx) ->
            if this.Expanded then
                tree_ctx.Scroll(-this_top + tree_top + 185.0f)
            else
                tree_ctx.Scroll(-this_top + tree_top + 400.0f)

            tree_ctx.ScrollTo <- ScrollTo.Nothing
        | _ -> ()

        let next_top =
            this.IfVisible(this_top, tree_top, tree_bottom, (fun b -> this.UpdateCulled(tree_top, b)))

        if this.Expanded then

            if (%%"group_multi_select").Pressed() then
                tree_ctx.ToggleMultiSelect(name, group_ctx, charts_as_seq)

            let padded_chart_height = CHART_HEIGHT + CHART_SPACING

            if tree_ctx.ScrollTo = ScrollTo.Chart && this.Selected then
                match Seq.tryFindIndex (fun (s: ChartItem) -> s.Selected) items with
                | Some i -> tree_ctx.Scroll (-(next_top + float32 i * padded_chart_height) + 500.0f)
                | None -> ()
                tree_ctx.ScrollTo <- ScrollTo.Nothing

            let first_visible_chart_index = (tree_top - next_top) / padded_chart_height |> floor |> int |> max 0
            let mutable chart_index = first_visible_chart_index
            let mutable top_edge = next_top + float32 chart_index * padded_chart_height

            while top_edge < tree_bottom && chart_index < items.Count do
                top_edge <- items.[chart_index].Update(top_edge, tree_top, tree_bottom, elapsed_ms)
                chart_index <- chart_index + 1

            next_top + float32 items.Count * padded_chart_height
        else
            next_top