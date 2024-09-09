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

    let mutable last_cached_flag = -1
    let select_animation = Animation.Fade(0.0f)
    let mutable label = ""

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
            | LibraryGroupContext.None -> sprintf "%s %i" Icons.FOLDER items.Count

    do update_cached_info ()

    override this.Bounds(top) =
        Rect.Create(
            Viewport.vwidth * (0.5f - 0.05f * select_animation.Value),
            top,
            Viewport.vwidth - 25.0f,
            top + GROUP_HEIGHT
        )

    override this.Selected = selected_group = name
    override this.Spacing = 20.0f

    member this.Items = items
    member this.Name = name
    member this.Expanded = expanded_group = name

    member this.SelectFirst() = items.First().Select()
    member this.SelectLast() = items.Last().Select()

    member private this.OnDraw(bounds: Rect) =
        Draw.rect (bounds.Translate(10.0f, 10.0f)) (Palette.color (255, 0.2f, 0.0f))
        Background.draw (bounds, (Color.FromArgb(40, 40, 40)), 1.5f)

        Draw.rect
            bounds
            (if this.Selected then
                    Palette.color (120, 0.7f + select_animation.Value * 0.3f, select_animation.Value * 0.3f)
                else
                    Palette.color (100, 0.7f, 0.0f))

        Text.fill_b (Style.font, name, bounds.Shrink(15.0f, 5.0f).ShrinkR(100.0f), Colors.text, Alignment.LEFT)
        Text.fill_b (Style.font, label, bounds.Shrink(15.0f, 5.0f), Colors.text_subheading, Alignment.RIGHT)

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
                    Alignment.RIGHT
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
                    expanded_group <- name
                    scroll_to <- ScrollTo.Pack name
            elif this.RightClick(origin) then
                GroupContextMenu.Show(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), context)
            elif (%%"delete").Tapped() then
                GroupContextMenu.ConfirmDelete(name, items |> Seq.map (fun (x: ChartItem) -> x.Chart), false)

    member this.Update(top, origin, originB, elapsed_ms) =
        if last_cached_flag < cache_flag then
            update_cached_info ()

        select_animation.Target <- if this.Selected then 1.0f else 0.0f
        select_animation.Update elapsed_ms

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