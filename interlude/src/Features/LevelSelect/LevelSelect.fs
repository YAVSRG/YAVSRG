namespace Interlude.Features.LevelSelect

open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections
open Prelude.Data.Charts.Suggestions
open Prelude.Data.Charts.Tables
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Online

type LevelSelectScreen() =
    inherit Screen()

    let search_text = Setting.simple ""

    let info_panel =
        ChartInfo(
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 175.0f
                    Right = 0.4f %- 10.0f
                    Bottom = 1.0f %+ 0.0f
                }
        )

    let refresh () =
        info_panel.Refresh()
        Tree.refresh ()

    let random_chart () =
        if options.AdvancedRecommendations.Value && Chart.CACHE_DATA.IsSome then
            match Suggestion.get_suggestion Chart.CACHE_DATA.Value Tree.filter with
            | Some c ->
                Tree.switch_chart (c, LibraryContext.None, "")
                refresh ()
            | None -> Notifications.action_feedback(Icons.ALERT_CIRCLE, %"notification.suggestion_failed", "")
        else
            match Suggestion.get_random Tree.filter with
            | Some c ->
                Tree.switch_chart (c, LibraryContext.None, "")
                refresh ()
            | None -> ()

    override this.Init(parent: Widget) =
        base.Init parent

        Setting.app (fun s -> if sorting_modes.ContainsKey s then s else "title") options.ChartSortMode
        Setting.app (fun s -> if grouping_modes.ContainsKey s then s else "pack") options.ChartGroupMode

        this
        |+ Text(
            (fun () ->
                match Chart.CACHE_DATA with
                | None -> ""
                | Some c -> c.Title
            ),
            Align = Alignment.CENTER,
            Position =
                {
                    Left = 0.0f %+ 30.0f
                    Top = 0.0f %+ 20.0f
                    Right = 0.4f %- 30.0f
                    Bottom = 0.0f %+ 100.0f
                }
        )

        |+ Text(
            (fun () ->
                match Chart.CACHE_DATA with
                | None -> ""
                | Some c -> c.DifficultyName
            ),
            Align = Alignment.CENTER,
            Position =
                {
                    Left = 0.0f %+ 30.0f
                    Top = 0.0f %+ 90.0f
                    Right = 0.4f %- 30.0f
                    Bottom = 0.0f %+ 140.0f
                }
        )

        |+ SearchBox(
            search_text,
            (fun f ->
                Tree.filter <- f
                refresh ()
            ),
            Position =
                {
                    Left = 1.0f %- 580.0f
                    Top = 0.0f %+ 30.0f
                    Right = 1.0f %- (30.0f + Style.PADDING)
                    Bottom = 0.0f %+ 90.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.search", "search"))

        |+ Conditional(
            (fun () -> Tree.is_empty),
            StaticContainer(NodeType.None)
            |+ Conditional((fun () -> Tree.filter <> []), EmptyState(Icons.SEARCH, %"levelselect.empty.search"))
            |+ Conditional(
                (fun () ->
                    Tree.filter = []
                    && options.LibraryMode.Value = LibraryMode.Table
                    && Table.current().IsNone
                ),
                EmptyState(Icons.SIDEBAR, %"levelselect.empty.no_table")
            )
            |+ Conditional(
                (fun () -> Tree.filter = [] && options.LibraryMode.Value = LibraryMode.Collections),
                EmptyState(Icons.FOLDER, %"levelselect.empty.no_collections")
            )
            |+ Conditional(
                (fun () -> Tree.filter = [] && options.LibraryMode.Value = LibraryMode.All),
                EmptyState(Icons.FOLDER, %"levelselect.empty.no_charts")
            ),
            Position =
                { Position.TrimTop(170.0f) with
                    Left = 0.5f %+ 0.0f
                }
        )

        |+ ActionBar(
            random_chart,
            Position =
                {
                    Left = 1.0f %- 805.0f
                    Top = 0.0f %+ 30.0f
                    Right = 1.0f %- 605.0f
                    Bottom = 0.0f %+ 90.0f
                }
        )

        |+ LibraryModeSettings()

        |* info_panel

        Chart.on_chart_change.Add info_panel.Refresh
        Comments.init this

        LevelSelect.on_refresh_all.Add refresh

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        Comments.update (elapsed_ms, moved)

        if (%%"select").Tapped() then
            LevelSelect.play ()

        elif (%%"next").Tapped() then
            Tree.next ()
        elif (%%"previous").Tapped() then
            Tree.previous ()
        elif (%%"next_group").Tapped() then
            Tree.next_group ()
        elif (%%"previous_group").Tapped() then
            Tree.previous_group ()
        elif (%%"start").Tapped() then
            Tree.top_of_group ()
        elif (%%"end").Tapped() then
            Tree.bottom_of_group ()

        Tree.update (this.Bounds.Top + 170.0f, this.Bounds.Bottom, elapsed_ms)

    override this.Draw() =

        Tree.draw (this.Bounds.Top + 170.0f, this.Bounds.Bottom)

        let w = this.Bounds.Width * 0.4f

        let {
                Rect.Left = left
                Top = top
                Right = right
            } =
            this.Bounds

        Draw.quad
            (Quad.create
             <| Vector2(left, top)
             <| Vector2(left + w + 85.0f, top)
             <| Vector2(left + w, top + 170.0f)
             <| Vector2(left, top + 170.0f))
            (Quad.color !*Palette.DARK_100)
            Sprite.DEFAULT_QUAD

        Draw.rect (this.Bounds.SliceTop(170.0f).SliceLeft(w).Shrink(20.0f)) (Colors.shadow_2.O2)

        Draw.quad
            (Quad.create
             <| Vector2(left + w + 85.0f, top)
             <| Vector2(right, top)
             <| Vector2(right, top + 170.0f)
             <| Vector2(left + w, top + 170.0f))
            (Quad.color (Colors.shadow_2.O2))
            Sprite.DEFAULT_QUAD

        Draw.rect (this.Bounds.SliceTop(175.0f).SliceBottom(5.0f)) (Palette.color (255, 0.8f, 0.0f))

        base.Draw()
        Comments.draw ()

    override this.OnEnter prev =
        Song.on_finish <- SongFinishAction.LoopFromPreview

        if Cache.recache_service.Status <> Async.ServiceStatus.Idle then
            Notifications.system_feedback (
                Icons.ALERT_OCTAGON,
                %"notification.recache_running.title",
                %"notification.recache_running.body"
            )

        refresh ()
        DiscordRPC.in_menus ("Choosing a song")

    override this.OnExit next = Input.remove_listener ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some Screen.Type.Lobby
        else
            Some Screen.Type.MainMenu
