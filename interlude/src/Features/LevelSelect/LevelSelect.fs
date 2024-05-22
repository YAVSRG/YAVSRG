namespace Interlude.Features.LevelSelect

open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Sorting
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.UI
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
        SelectedChart.if_loaded (fun info -> info_panel.OnChartUpdated(info))
        Tree.refresh ()

    override this.Init(parent: Widget) =
        base.Init parent

        Setting.app (fun s -> if sorting_modes.ContainsKey s then s else "title") options.ChartSortMode
        Setting.app (fun s -> if grouping_modes.ContainsKey s then s else "pack") options.ChartGroupMode

        this
        |+ Text(
            (fun () ->
                match SelectedChart.CACHE_DATA with
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
                match SelectedChart.CACHE_DATA with
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
                LevelSelect.filter <- f
                refresh ()
            ),
            Position =
                {
                    Left = 1.0f %- 580.0f
                    Top = 0.0f %+ 30.0f
                    Right = 1.0f %- (20.0f + Style.PADDING)
                    Bottom = 0.0f %+ 90.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.search", "search"))

        |+ Conditional(
            (fun () -> Tree.is_empty),
            Container(NodeType.None)
            |+ Conditional((fun () -> LevelSelect.filter <> []), EmptyState(Icons.SEARCH, %"levelselect.empty.search"))
            |+ Conditional(
                (fun () ->
                    LevelSelect.filter = []
                    && options.LibraryMode.Value = LibraryMode.Table
                    && Content.Table.IsNone
                ),
                EmptyState(Icons.SIDEBAR, %"levelselect.empty.no_table")
            )
            |+ Conditional(
                (fun () -> LevelSelect.filter = [] && options.LibraryMode.Value = LibraryMode.Collections),
                EmptyState(Icons.FOLDER, %"levelselect.empty.no_collections")
            )
            |+ Conditional(
                (fun () -> LevelSelect.filter = [] && options.LibraryMode.Value = LibraryMode.All),
                EmptyState(Icons.FOLDER, %"levelselect.empty.no_charts")
            ),
            Position =
                { Position.TrimTop(170.0f) with
                    Left = 0.5f %+ 0.0f
                }
        )

        |+ ActionBar(
            Position =
                {
                    Left = 1.0f %- 805.0f
                    Top = 0.0f %+ 30.0f
                    Right = 1.0f %- 605.0f
                    Bottom = 0.0f %+ 90.0f
                }
        )

        |+ LibraryViewControls()

        |+ StylishButton(
            LevelSelect.choose_this_chart,
            K (sprintf "%s %s" Icons.PLAY %"levelselect.play"),
            !%Palette.MAIN.O2,
            TiltRight = false,
            Position = Position.SliceBottom(50.0f).SliceRight(300.0f)
        )
            .Tooltip(Tooltip.Info("levelselect.play").Hotkey("select"))
        |+ Conditional(
            (fun () -> match SelectedChart.LIBRARY_CTX with LibraryContext.Playlist _ -> true | _ -> false),
            StylishButton(
                (fun () ->
                    match SelectedChart.LIBRARY_CTX with
                    | LibraryContext.Playlist (_, name, _) ->
                        Suggestions.begin_endless_mode (
                            EndlessModeState.create_from_playlist
                                0
                                (Content.Collections.GetPlaylist(name).Value)
                                Content.Library
                        ) true
                    | _ -> ()
                ),
                K (sprintf "%s %s" Icons.PLAY_CIRCLE %"playlist.play"),
                !%Palette.DARK.O2,
                Position = Position.SliceBottom(50.0f).SliceRight(300.0f).Translate(-325.0f, 0.0f),
                Hotkey = "endless_mode",
                Disabled = (fun () -> Network.lobby.IsSome)
            ).Tooltip(Tooltip.Info("playlist.play").Hotkey("endless_mode"))
        )
        |+ Conditional(
            (fun () -> match SelectedChart.LIBRARY_CTX with LibraryContext.Playlist _ -> false | _ -> true),
            StylishButton(
                (fun () -> SelectedChart.if_loaded(fun info -> EndlessModeMenu(info).Show())),
                K (sprintf "%s %s" Icons.PLAY_CIRCLE %"levelselect.endless_mode"),
                !%Palette.DARK.O2,
                Position = Position.SliceBottom(50.0f).SliceRight(300.0f).Translate(-325.0f, 0.0f),
                Hotkey = "endless_mode",
                Disabled = (fun () -> Network.lobby.IsSome)
            )
                .Tooltip(Tooltip.Info("levelselect.endless_mode").Hotkey("endless_mode"))
        )
        |+ StylishButton(
            (fun () -> SelectedChart.if_loaded(fun info -> ChartContextMenu(info.CacheInfo, info.LibraryContext).Show())),
            K Icons.LIST,
            !%Palette.MAIN.O2,
            Position = Position.SliceBottom(50.0f).SliceRight(60.0f).Translate(-650.0f, 0.0f)
        )
            .Tooltip(Tooltip.Info("levelselect.context_menu").Hotkey("context_menu"))

        |* info_panel

        Comments.init this

        LevelSelect.on_refresh_all.Add refresh

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        Comments.update (elapsed_ms, moved)

        if (%%"select").Tapped() then
            LevelSelect.choose_this_chart ()

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

        Draw.untextured_quad
            (Quad.create
             <| Vector2(left, top)
             <| Vector2(left + w + 85.0f, top)
             <| Vector2(left + w, top + 170.0f)
             <| Vector2(left, top + 170.0f))
            (!*Palette.DARK_100).AsQuad

        Draw.rect (this.Bounds.SliceTop(170.0f).SliceLeft(w).Shrink(20.0f)) (Colors.shadow_2.O2)

        Draw.untextured_quad
            (Quad.create
             <| Vector2(left + w + 85.0f, top)
             <| Vector2(right, top)
             <| Vector2(right, top + 170.0f)
             <| Vector2(left + w, top + 170.0f))
            Colors.shadow_2.O2.AsQuad

        Draw.rect (this.Bounds.SliceTop(175.0f).SliceBottom(5.0f)) (Palette.color (255, 0.8f, 0.0f))

        base.Draw()
        Comments.draw ()

    override this.OnEnter prev =
        Suggestions.exit_endless_mode ()
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
