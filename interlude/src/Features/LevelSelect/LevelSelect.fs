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
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.UI
open Interlude.Features.Online
open Interlude.Features.Play

type LevelSelectScreen() =
    inherit Screen()

    let search_text = Setting.simple ""

    let info_panel =
        ChartInfo(
            Position =
                {
                    Left = 0.0f %+ 0.0f
                    Top = 0.0f %+ 175.0f
                    Right = 0.4f %- 0.0f
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
        |+ CurrentChart(Position = { Position.SliceT(170.0f) with Right = 0.4f %- 0.0f })
        |+ SearchBox(
            search_text,
            (fun f ->
                LevelSelect.filter <- f
                refresh ()
            ),
            Position =
                {
                    Left = 0.4f %+ 290.0f
                    Top = 0.0f %+ 30.0f
                    Right = 1.0f %- (20.0f + Style.PADDING)
                    Bottom = 0.0f %+ 90.0f
                }
        )
            .Help(Help.Info("levelselect.search", "search"))

        |+ (
            Container(NodeType.None, Position = { Position.ShrinkT(170.0f) with Left = 0.5f %+ 0.0f })
            |+ EmptyState(Icons.SEARCH, %"levelselect.empty.search")
                .Conditional(fun () -> LevelSelect.filter <> [])
            |+ EmptyState(Icons.SIDEBAR, %"levelselect.empty.no_table")
                .Conditional(fun () ->
                    LevelSelect.filter = []
                    && options.LibraryMode.Value = LibraryMode.Table
                    && Content.Table.IsNone
                )
            |+ EmptyState(Icons.FOLDER, %"levelselect.empty.no_collections")
                .Conditional(fun () -> LevelSelect.filter = [] && options.LibraryMode.Value = LibraryMode.Collections)
            |+ EmptyState(Icons.FOLDER, %"levelselect.empty.no_charts")
                .Conditional(fun () -> LevelSelect.filter = [] && options.LibraryMode.Value = LibraryMode.All)
        )
            .Conditional(fun () -> Tree.is_empty)

        |+ LibraryViewControls()

        |+ StylishButton(
            LevelSelect.choose_this_chart,
            K (sprintf "%s %s" Icons.PLAY %"levelselect.play"),
            !%Palette.MAIN.O2,
            TiltRight = false,
            Position = Position.SliceB(50.0f).SliceR(250.0f)
        )
            .Help(Help.Info("levelselect.play").Hotkey("select"))
        |+ StylishButton(
            (fun () ->
                SelectedChart.when_loaded
                <| fun info ->
                    Screen.change_new
                        (fun () -> PracticeScreen.practice_screen (info, 0.0f<ms>))
                        Screen.Type.Practice
                        Transitions.Default
                    |> ignore
            ),
            K Icons.TARGET,
            !%Palette.DARK.O2,
            Hotkey = "practice_mode",
            Position = Position.SliceB(50.0f).SliceR(60.0f).Translate(-275.0f, 0.0f)
        )
            .Help(Help.Info("levelselect.practice_mode").Hotkey("practice_mode"))
        |+ StylishButton(
            LevelSelect.random_chart,
            K Icons.REFRESH_CCW,
            !%Palette.MAIN.O2,
            Position = Position.SliceB(50.0f).SliceR(60.0f).Translate(-360.0f, 0.0f)
        )
            .Help(Help.Info("levelselect.random_chart").Hotkey("random_chart"))
        |+ StylishButton(
            (fun () -> SelectedChart.if_loaded(fun info -> ChartContextMenu(info.CacheInfo, info.LibraryContext).Show())),
            K Icons.LIST,
            !%Palette.DARK.O2,
            Position = Position.SliceB(50.0f).SliceR(60.0f).Translate(-445.0f, 0.0f)
        )
            .Help(Help.Info("levelselect.context_menu").Hotkey("context_menu"))

        |* info_panel

        LevelSelect.on_refresh_all.Add refresh

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

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

        Draw.untextured_quad
            (Quad.create
             <| Vector2(left + w + 85.0f, top)
             <| Vector2(right, top)
             <| Vector2(right, top + 170.0f)
             <| Vector2(left + w, top + 170.0f))
            Colors.shadow_2.O2.AsQuad

        Draw.rect (this.Bounds.SliceT(170.0f).BorderB(5.0f)) (Palette.color (255, 0.8f, 0.0f))

        base.Draw()

    override this.OnEnter prev =
        LevelSelect.exit_gameplay()
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
