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

open TreeState

type private ChartItem(group_name: string, group_ctx: LibraryGroupContext, cc: ChartMeta, ctx: LibraryContext) =
    inherit TreeItem()

    let hover = Animation.Fade 0.0f
    let mutable last_cached_flag = -1
    let mutable chart_save_data = None
    let mutable personal_bests: Bests option = None
    let mutable grade = None
    let mutable lamp = None
    let mutable markers = ""

    let get_pb (bests: PersonalBests<'T>) (rate: Rate) (color_func: 'T -> Color) (format: 'T -> string) =
        match PersonalBests.get_best_above rate bests with
        | Some(v, r, _) -> Some(v, r, color_func v, format v)
        | None ->

        match PersonalBests.get_best_below rate bests with
        | Some(v, r, _) -> Some(v, r, Colors.white.O2, format v)
        | None -> None

    let update_cached_info () =
        last_cached_flag <- cache_flag

        if chart_save_data.IsNone then
            chart_save_data <- Some(UserDatabase.get_chart_data cc.Hash Content.UserData)

        match chart_save_data with
        | Some d when d.PersonalBests.ContainsKey Rulesets.current_hash ->
            let rate =
                match ctx with
                | LibraryContext.Playlist (_, _, d) -> d.Rate.Value
                | _ -> SelectedChart.rate.Value

            personal_bests <- Some d.PersonalBests.[Rulesets.current_hash]
            grade <-
                match get_pb personal_bests.Value.Grade rate Rulesets.current.GradeColor Rulesets.current.GradeName with
                | Some (grade, grade_rate, color, text) when not options.TreeShowGradesOnly.Value ->
                    match get_pb personal_bests.Value.Accuracy rate (K Colors.white) Rulesets.current.FormatAccuracy with
                    | Some (accuracy, accuracy_rate, _, text) ->
                        Some (grade, accuracy_rate, color, text)
                    | None -> Some (grade, grade_rate, color, text)
                | otherwise -> otherwise
            lamp <- get_pb personal_bests.Value.Lamp rate Rulesets.current.LampColor Rulesets.current.LampName
        | _ ->
            personal_bests <- None
            grade <- None
            lamp <- None

        markers <-
            match ctx with
            | LibraryContext.Likes -> ""
            | _ -> if CollectionActions.is_liked cc then Icons.HEART else ""

    override this.Bounds(top: float32) : Rect =
        Rect.FromEdges(Render.width() * 0.4f + Style.PADDING, top, Render.width(), top + CHART_HEIGHT)

    override this.Selected : bool = selected_chart = cc.Hash && SelectedChart.LIBRARY_CTX.Matches ctx

    override this.Spacing = 5.0f
    member this.Chart = cc
    member this.Context = ctx

    member this.PlaylistDuration : GameplayTime =
        match ctx with
        | LibraryContext.Playlist(_, _, data) -> cc.Length / data.Rate.Value
        | _ -> 0.0f<ms / rate>

    member this.Select() : unit = switch_chart (cc, ctx, group_name, group_ctx)

    member private this.OnDraw(bounds: Rect) =
        let {
                Rect.Left = left
                Top = top
                Right = right
                Bottom = bottom
            } =
            bounds

        let is_multi_selected = match multi_selection with Some s -> s.IsSelected(cc, ctx) | None -> false

        let accent =
            let alpha = 80 + int (hover.Value * 40.0f)
            if is_multi_selected then Colors.grey_2.O2a alpha else Palette.color (alpha, 1.0f, 0.4f)
        let color, hover_color =
            if is_multi_selected then Colors.grey_2.O2, Colors.white.O2
            elif this.Selected then !*Palette.MAIN_100, !*Palette.LIGHT
            else Colors.shadow_1.O2, Colors.grey_2.O2

        Render.rect bounds color

        let stripe_length = (right - left) * (0.4f + 0.6f * hover.Value)
        Render.quad_points_c
            (left, top)
            (left + stripe_length, top)
            (left + stripe_length, bottom - 25.0f)
            (left, bottom - 25.0f)
            (Quad.gradient_left_to_right accent Color.Transparent)
        Render.rect (bounds.BorderL Style.PADDING) hover_color

        // draw pbs
        let disp (data: 'T * Rate * Color * string) (pos: float32) =
            let _, rate, color, formatted = data
            let rate_label = sprintf "(%.2fx)" rate

            if color.A > 0uy then
                Render.rect (Rect.FromEdges(right - pos - 40.0f, top, right - pos + 40.0f, bottom)) accent

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
        Render.rect (bounds.SliceB 25.0f) Colors.shadow_1.O1
        Text.draw_b (Style.font, (if options.TreeShowNativeText.Value then cc.TitleNative |> Option.defaultValue cc.Title else cc.Title), 23.0f, left + 7f, top, if is_multi_selected then Colors.text_yellow_2 else Colors.text)

        Text.draw_b (
            Style.font,
            sprintf "%s  •  %s" (if options.TreeShowNativeText.Value then cc.ArtistNative |> Option.defaultValue cc.Artist else cc.Artist) cc.Creator,
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

        let icon =
            match multi_selection with
            | Some s -> if s.IsSelected(cc, ctx) then Icons.CHECK_SQUARE else Icons.SQUARE
            | None -> markers

        Text.draw_aligned_b (Style.font, icon, 25.0f, right - 65.0f, top + 15.0f, Colors.text, Alignment.CENTER)

    member this.Draw(top: float32, origin: float32, originB: float32) : float32 =
        this.CheckBounds(top, origin, originB, this.OnDraw)

    member private this.OnUpdate(origin: float32, bounds: Rect, elapsed_ms: float) =

        if last_cached_flag < cache_flag then
            update_cached_info ()

        if this.Selected && (%%"multi_select").Pressed() then
            match multi_selection with
            | Some s when s.IsSelected(cc, ctx) -> deselect_multiple [(cc, ctx)]
            | _ -> select_multiple [(cc, ctx)]

        if Mouse.hover bounds then
            hover.Target <- 1.0f

            if this.LeftClick(origin) then
                if MULTI_SELECT_KEY.Held() then
                    match multi_selection with
                    | Some s when s.IsSelected(cc, ctx) -> deselect_multiple [(cc, ctx)]
                    | _ -> select_multiple [(cc, ctx)]
                elif this.Selected then
                    LevelSelect.choose_this_chart ()
                else
                    if not (Transitions.in_progress()) then LevelSelect.History.append_current ()
                    this.Select()

            elif this.RightClick(origin) then
                match multi_selection with
                | Some s when s.IsSelected(cc, ctx) -> s.ShowActions()
                | _ -> ChartContextMenu(cc, ctx).Show()

            elif (%%"delete").Pressed() then
                match multi_selection with
                | Some s when s.IsSelected(cc, ctx) -> s.ConfirmDelete()
                | _ -> ChartDeleteMenu(cc, ctx, false).Show()
        else
            hover.Target <- 0.0f

        hover.Update(elapsed_ms) |> ignore

    member this.Update(top: float32, origin: float32, originB: float32, elapsed_ms: float) : float32 =
        this.CheckBounds(top, origin, originB, (fun b -> this.OnUpdate(origin, b, elapsed_ms)))