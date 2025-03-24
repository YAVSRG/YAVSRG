namespace Interlude.Features.LevelSelect

open System.Linq
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.UI
open Interlude.Content

open TreeState

type private GroupItem(name: string, items: ResizeArray<ChartItem>, context: LibraryGroupContext) =
    inherit TreeItem()

    let display_name = if context = LibraryGroupContext.Likes then  %"library.likes" else name

    let charts_as_seq = items |> Seq.map (fun i -> i.Chart, i.Context)

    let mutable last_cached_flag = -1
    let select_animation = Animation.Fade(0.0f)
    let mutable label = ""
    let special_color =
        match context with
        | LibraryGroupContext.None -> None
        | LibraryGroupContext.Pack _ -> None
        | LibraryGroupContext.Table _ -> None
        | LibraryGroupContext.Likes -> Some (Colors.pink, Colors.pink_shadow)
        | LibraryGroupContext.Folder _ -> Some (Colors.cyan, Colors.cyan_shadow)
        | LibraryGroupContext.Playlist _ -> Some (Colors.green, Colors.green_shadow)

    let update_cached_info () =
        last_cached_flag <- cache_flag

        label <-
            match context with
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

    override this.Bounds(top: float32) =
        Rect.FromEdges(
            Render.width() * (0.5f - 0.05f * select_animation.Value),
            top,
            Render.width() - 25.0f,
            top + GROUP_HEIGHT
        )

    override this.Selected = selected_group = (name, context)
    override this.Spacing = 20.0f

    member this.Items = items
    member this.Name = name
    member this.Context = context
    member this.Expanded = expanded_group = (name, context)

    member this.SelectFirst() = items.First().Select()
    member this.SelectLast() = items.Last().Select()

    member private this.OnDraw(bounds: Rect) =

        let color, bg_color =
            match multi_selection with
            | Some s when s.GroupAmountSelected(name, context, charts_as_seq) <> AmountSelected.None ->
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

        match multi_selection with
        | Some s ->
            let filled_icon, name_color =
                match s.GroupAmountSelected(name, context, charts_as_seq) with
                | AmountSelected.None -> Icons.SQUARE, Colors.text_subheading
                | AmountSelected.Some -> Icons.PLUS_SQUARE, Colors.text_yellow_2
                | AmountSelected.All -> Icons.CHECK_SQUARE, Colors.text_yellow_2
            Text.fill_b (Style.font, display_name, bounds.Shrink(15.0f, 5.0f).ShrinkR(150.0f), name_color, Alignment.LEFT)
            Text.fill_b (Style.font, filled_icon, bounds.Shrink(15.0f, 5.0f), Colors.text, Alignment.RIGHT)
            Text.fill_b (Style.font, label, bounds.Shrink(65.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)
        | None ->
            Text.fill_b (Style.font, display_name, bounds.Shrink(15.0f, 5.0f).ShrinkR(100.0f), Colors.text, Alignment.LEFT)
            Text.fill_b (Style.font, label, bounds.Shrink(15.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)

    member this.Draw(top: float32, origin: float32, originB: float32) : float32 =
        let b = this.CheckBounds(top, origin, originB, this.OnDraw)

        if this.Expanded then
            let h = CHART_HEIGHT + 5.0f

            let mutable index = (origin - b) / h |> floor |> int |> max 0

            let mutable p = b + float32 index * h

            while p < originB && index < items.Count do
                p <- items.[index].Draw(p, origin, originB)
                index <- index + 1

            let b2 = b + float32 items.Count * h

            if b < origin && b2 > origin then
                Text.draw_aligned_b (
                    Style.font,
                    name,
                    20.0f,
                    Render.width() - 20f,
                    origin + 10.0f,
                    Colors.text,
                    Alignment.RIGHT
                )

            b2
        else
            b

    member private this.OnUpdate(origin: float32, bounds: Rect, elapsed_ms: float) =
        if Mouse.hover bounds then
            if this.LeftClick(origin) then
                if MULTI_SELECT_KEY.Held() then
                    match multi_selection with
                    | Some s when s.GroupAmountSelected(name, context, charts_as_seq) = AmountSelected.All ->
                        deselect_multiple charts_as_seq
                    | _ -> select_multiple charts_as_seq
                elif this.Expanded then
                    expanded_group <- "", LibraryGroupContext.None
                else
                    expanded_group <- name, context
                    scroll_to <- ScrollTo.Group (name, context)
            elif this.RightClick(origin) then
                match multi_selection with
                | Some s when s.GroupAmountSelected(name, context, charts_as_seq) <> AmountSelected.None -> s.ShowActions()
                | _ -> GroupContextMenu.Show(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), context)
            elif (%%"delete").Pressed() then
                match context with
                | LibraryGroupContext.Folder _
                | LibraryGroupContext.Playlist _
                | LibraryGroupContext.Likes
                | LibraryGroupContext.Table _ -> ()
                | LibraryGroupContext.Pack _
                | LibraryGroupContext.None -> GroupContextMenu.ConfirmDelete(items |> Seq.map (fun (x: ChartItem) -> x.Chart), context, false)

    member this.Update(top: float32, origin: float32, originB: float32, elapsed_ms: float) : float32 =
        if last_cached_flag < cache_flag then
            update_cached_info ()

        select_animation.Target <- if this.Selected then 1.0f else 0.0f
        select_animation.Update elapsed_ms

        match scroll_to with
        | ScrollTo.Group (a, b) when (a, b) = (name, context) ->
            if this.Expanded then
                scroll (-top + origin + 185.0f)
            else
                scroll (-top + origin + 400.0f)

            scroll_to <- ScrollTo.Nothing
        | _ -> ()

        let b =
            this.CheckBounds(top, origin, originB, (fun b -> this.OnUpdate(origin, b, elapsed_ms)))

        if this.Expanded then

            if (%%"group_multi_select").Pressed() then
                match multi_selection with
                | Some s when s.GroupAmountSelected(name, context, charts_as_seq) = AmountSelected.All ->
                    deselect_multiple charts_as_seq
                | _ -> select_multiple charts_as_seq

            let h = CHART_HEIGHT + 5.0f

            if scroll_to = ScrollTo.Chart && this.Selected then
                match Seq.tryFindIndex (fun (s: ChartItem) -> s.Selected) items with
                | Some i -> scroll (-(b + float32 i * h) + 500.0f)
                | None -> ()
                scroll_to <- ScrollTo.Nothing

            let mutable index = (origin - b) / h |> floor |> int |> max 0
            let mutable p = b + float32 index * h

            while p < originB && index < items.Count do
                p <- items.[index].Update(p, origin, originB, elapsed_ms)
                index <- index + 1

            b + float32 items.Count * h
        else
            b