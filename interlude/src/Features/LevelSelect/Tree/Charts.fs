namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Features.Collections

[<Struct>]
type PersonalBestCached =
    {
        Text: string
        Color: Color
        Details: string
    }

type private ChartItem(tree_ctx: TreeContext, group_name: string, group_ctx: LibraryGroupContext, chart_meta: ChartMeta, library_ctx: LibraryContext) =
    inherit TreeItem(tree_ctx)

    let hover = Animation.Fade 0.0f
    let mutable last_cached_flag = -1
    let mutable chart_save_data = None
    let mutable personal_bests: Bests option = None
    let mutable grade_or_accuracy : PersonalBestCached option = None
    let mutable lamp : PersonalBestCached option = None
    let mutable markers = ""

    let get_pb (bests: PersonalBests<'T>) (rate: Rate) =
        match PersonalBests.get_best_above rate bests with
        | Some pb_above_rate -> Some pb_above_rate
        | None -> PersonalBests.get_best_below rate bests

    let details (rate: Rate) (timestamp: int64) =
        if options.TreeShowTimestamps.Value then
            sprintf "%.2fx • %s" rate (timestamp |> Timestamp.since |> format_timespan)
        else
            sprintf "(%.2fx)" rate

    let grade_pb (bests: Bests) (rate: Rate) : PersonalBestCached option =
        match get_pb bests.Grade rate with
        | Some (grade, r, timestamp) when r >= rate ->
            Some {
                Text = Rulesets.current.GradeName grade
                Color = Rulesets.current.GradeColor grade
                Details = details r timestamp
            }
        | Some (grade, r, timestamp) ->
            Some {
                Text = Rulesets.current.GradeName grade
                Color = Colors.white.O2
                Details = details r timestamp
            }
        | None -> None

    let acc_pb (bests: Bests) (rate: Rate) : PersonalBestCached option =
        match grade_pb bests rate with
        | None -> None
        | Some d ->

        match get_pb bests.Accuracy rate with
        | Some (accuracy, r, timestamp) when r >= rate ->
            Some
                { d with
                    Text = Rulesets.current.FormatAccuracy accuracy
                    Details = details r timestamp
                }
        | Some (accuracy, r, timestamp) ->
            Some
                { d with
                    Text = Rulesets.current.FormatAccuracy accuracy
                    Details = details r timestamp
                }
        | None -> None

    let lamp_pb (bests: Bests) (rate: Rate) : PersonalBestCached option =
        match get_pb bests.Lamp rate with
        | Some (lamp, r, timestamp) when r >= rate ->
            Some {
                Text = Rulesets.current.LampName lamp
                Color = Rulesets.current.LampColor lamp
                Details = details r timestamp
            }
        | Some (lamp, r, timestamp) ->
            Some {
                Text = Rulesets.current.LampName lamp
                Color = Colors.white.O2
                Details = details r timestamp
            }
        | None -> None

    let update_cached_info () =
        last_cached_flag <- tree_ctx.CacheFlag

        if chart_save_data.IsNone then
            chart_save_data <- Some(UserDatabase.get_chart_data chart_meta.Hash Content.UserData)

        match chart_save_data with
        | Some d when d.PersonalBests.ContainsKey Rulesets.current_hash ->
            let rate =
                match library_ctx with
                | LibraryContext.Playlist (_, _, d) -> d.Rate.Value
                | _ -> SelectedChart.rate.Value

            personal_bests <- Some d.PersonalBests.[Rulesets.current_hash]
            grade_or_accuracy <- if options.TreeShowGradesOnly.Value then grade_pb personal_bests.Value rate else acc_pb personal_bests.Value rate
            lamp <- lamp_pb personal_bests.Value rate
        | _ ->
            personal_bests <- None
            grade_or_accuracy <- None
            lamp <- None

        markers <-
            match library_ctx with
            | LibraryContext.Likes -> ""
            | _ -> if CollectionActions.is_liked chart_meta then Icons.HEART else ""

    override this.Bounds(top: float32) : Rect =
        Rect.FromEdges(Render.width() * 0.4f + Style.PADDING, top, Render.width(), top + CHART_HEIGHT)
    override this.Spacing = CHART_SPACING

    member this.Selected : bool = tree_ctx.IsSelected(chart_meta, library_ctx)
    member this.Chart = chart_meta
    member this.Context = library_ctx

    member this.PlaylistDuration : GameplayTime =
        match library_ctx with
        | LibraryContext.Playlist(_, _, data) -> chart_meta.Length / data.Rate.Value
        | _ -> 0.0f<ms / rate>

    member this.Select() : unit = tree_ctx.SelectChart(chart_meta, library_ctx, group_name, group_ctx)

    /// Only called if this chart can be seen on screen
    member private this.DrawCulled(bounds: Rect) : unit =

        let is_multi_selected = match tree_ctx.MultiSelection with Some s -> s.Contains(chart_meta, library_ctx) | None -> false

        let accent =
            let alpha = 80 + int (hover.Value * 40.0f)
            if is_multi_selected then Colors.grey_2.O2a alpha else Palette.color (alpha, 1.0f, 0.4f)

        let color, hover_color =
            if is_multi_selected then Colors.grey_2.O2, Colors.white.O2
            elif this.Selected then !*Palette.MAIN_100, !*Palette.LIGHT
            else Colors.shadow_1.O2, Colors.grey_2.O2

        Render.rect bounds color

        let stripe_length = bounds.Width * (0.4f + 0.6f * hover.Value)
        Render.quad_points_c
            (bounds.Left, bounds.Top)
            (bounds.Left + stripe_length, bounds.Top)
            (bounds.Left + stripe_length, bounds.Bottom - 25.0f)
            (bounds.Left, bounds.Bottom - 25.0f)
            (Quad.gradient_left_to_right accent Color.Transparent)
        Render.rect (bounds.BorderL Style.PADDING) hover_color

        // draw pbs
        let disp (data: PersonalBestCached) (pos: float32) =

            if data.Color.A > 0uy then
                Render.rect (bounds.SliceR(pos - 40.0f, 80.0f)) accent

                Text.draw_aligned_b (
                    Style.font,
                    data.Text,
                    20.0f,
                    bounds.Right - pos,
                    bounds.Top + 8.0f,
                    (data.Color, Color.Black),
                    0.5f
                )

                Text.draw_aligned_b (
                    Style.font,
                    data.Details,
                    14.0f,
                    bounds.Right - pos,
                    bounds.Top + 35.0f,
                    (data.Color, Color.Black),
                    0.5f
                )

        if personal_bests.IsSome then
            disp grade_or_accuracy.Value 290.0f
            disp lamp.Value 165.0f

        // draw text
        Render.rect (bounds.SliceB 25.0f) Colors.shadow_1.O1
        Text.draw_b (
            Style.font,
            (
                if options.TreeShowNativeText.Value then
                    chart_meta.TitleNative |> Option.defaultValue chart_meta.Title
                else
                    chart_meta.Title
            ),
            23.0f,
            bounds.Left + 7f,
            bounds.Top,
            if is_multi_selected then Colors.text_yellow_2 else Colors.text
        )

        Text.draw_b (
            Style.font,
            sprintf "%s  •  %s" (if options.TreeShowNativeText.Value then chart_meta.ArtistNative |> Option.defaultValue chart_meta.Artist else chart_meta.Artist) chart_meta.Creator,
            18.0f,
            bounds.Left + 7f,
            bounds.Top + 34.0f,
            Colors.text_subheading
        )

        Text.draw_b (
            Style.font,
            chart_meta.Subtitle |> Option.defaultValue chart_meta.DifficultyName,
            15.0f,
            bounds.Left + 7f,
            bounds.Top + 65.0f,
            Colors.text_subheading
        )

        let icon =
            if tree_ctx.MultiSelection.IsSome then
                if is_multi_selected then Icons.CHECK_SQUARE else Icons.SQUARE
            else markers

        Text.draw_aligned_b (Style.font, icon, 25.0f, bounds.Right - 65.0f, bounds.Top + 15.0f, Colors.text, Alignment.CENTER)

    member this.Draw(this_top: float32, tree_top: float32, tree_bottom: float32) : float32 =
        this.IfVisible(this_top, tree_top, tree_bottom, this.DrawCulled)

    /// Only called if this chart can be seen on screen
    member private this.UpdateCulled(tree_top: float32, bounds: Rect, elapsed_ms: float) : unit =

        if last_cached_flag < tree_ctx.CacheFlag then
            update_cached_info ()

        if this.Selected && (%%"multi_select").Pressed() then
            tree_ctx.ToggleMultiSelect(chart_meta, library_ctx)

        if Mouse.hover bounds then
            hover.Target <- 1.0f

            if this.LeftClicked(tree_top) then
                if MULTI_SELECT_KEY.Held() then
                    tree_ctx.ToggleMultiSelect(chart_meta, library_ctx)
                elif this.Selected then
                    LevelSelect.choose_this_chart ()
                else
                    if not (Transitions.in_progress()) then LevelSelect.History.append_current ()
                    this.Select()

            elif this.RightClicked(tree_top) then
                match tree_ctx.MultiSelection with
                | Some selection when selection.Contains(chart_meta, library_ctx) -> selection.ShowActions()
                | _ -> ChartContextMenu(chart_meta, library_ctx).Show()

            elif (%%"delete").Pressed() then
                match tree_ctx.MultiSelection with
                | Some selection when selection.Contains(chart_meta, library_ctx) -> selection.ConfirmDelete()
                | _ -> ChartDeleteMenu(chart_meta, library_ctx, false).Show()
        else
            hover.Target <- 0.0f

        hover.Update(elapsed_ms) |> ignore

    member this.Update(this_top: float32, tree_top: float32, tree_bottom: float32, elapsed_ms: float) : float32 =
        this.IfVisible(this_top, tree_top, tree_bottom, (fun b -> this.UpdateCulled(tree_top, b, elapsed_ms)))