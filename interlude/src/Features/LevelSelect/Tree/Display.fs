namespace Interlude.Features.LevelSelect

open System.Linq
open OpenTK.Mathematics
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Data
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Interlude.UI
open Interlude.Utils
open Interlude.Content
open Interlude.Features.Gameplay

open TreeState

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
            chart_save_data <- Some(ScoreDatabase.get cc.Hash Content.Scores)

        match chart_save_data with
        | Some d when d.PersonalBests.ContainsKey Rulesets.current_hash ->
            personal_bests <- Some d.PersonalBests.[Rulesets.current_hash]
            grade <- get_pb personal_bests.Value.Grade Rulesets.current.GradeColor Rulesets.current.GradeName
            lamp <- get_pb personal_bests.Value.Lamp Rulesets.current.LampColor Rulesets.current.LampName
        | _ -> ()

        color <- color_func personal_bests

        markers <-
            //if options.LibraryMode.Value <> LibraryMode.Collections then
            //    match Collections.current with
            //    | Some(Folder c) -> if c.Contains cc then c.Icon.Value + " " else ""
            //    | Some(Playlist p) -> if p.Contains cc then p.Icon.Value + " " else ""
            //    | None -> ""
            //else
            ""
            + match chart_save_data with
                | Some c when not (System.String.IsNullOrEmpty c.Comment) -> Icons.MESSAGE_SQUARE
                | _ -> ""

    override this.Bounds(top) =
        Rect.Create(Viewport.vwidth * 0.4f, top, Viewport.vwidth, top + CHART_HEIGHT)

    override this.Selected =
        selected_chart = cc.Key
        && (context = LibraryContext.None || Chart.LIBRARY_CTX = context)

    override this.Spacing = 5.0f
    member this.Chart = cc

    member this.PlaylistDuration =
        match context with
        | LibraryContext.Playlist(_, _, data) -> cc.Length / data.Rate.Value
        | _ -> 0.0f<ms>

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

        Draw.untextured_quad
            (Quad.create
                <| new Vector2(left, top)
                <| new Vector2(left + stripe_length, top)
                <| new Vector2(left + stripe_length, bottom - 25.0f)
                <| new Vector2(left, bottom - 25.0f))
            (struct (accent, Color.Transparent, Color.Transparent, accent))

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
        Text.draw_b (Style.font, cc.Title, 23.0f, left + 7f, top, Colors.text)

        Text.draw_b (
            Style.font,
            sprintf "%s  •  %s" cc.Artist cc.Creator,
            18.0f,
            left + 7f,
            top + 34.0f,
            Colors.text_subheading
        )

        Text.draw_b (
            Style.font,
            cc.Subtitle |> Option.defaultValue cc.DifficultyName,
            15.0f,
            left + 7f,
            top + 65.0f,
            Colors.text_subheading
        )

        Text.draw_aligned_b (Style.font, markers, 25.0f, right - 65.0f, top + 15.0f, Colors.text, Alignment.CENTER)

        if
            Comments.fade.Value > 0.01f
            && chart_save_data.IsSome
            && chart_save_data.Value.Comment <> ""
        then
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
                if this.Selected then
                    LevelSelect.choose_this_chart ()
                else
                    this.Select()
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
            Viewport.vwidth - 15.0f,
            top + GROUP_HEIGHT
        )

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
                    Palette.color (120, 0.7f + select_animation.Value * 0.3f, select_animation.Value * 0.3f)
                else
                    Palette.color (100, 0.7f, 0.0f))

        Text.fill_b (Style.font, name, bounds.Shrink(15.0f, 5.0f).TrimRight(100.0f), Colors.text, Alignment.LEFT)
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