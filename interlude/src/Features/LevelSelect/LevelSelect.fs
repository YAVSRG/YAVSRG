namespace Interlude.Features.LevelSelect

open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.Options
open Interlude.Features.Gameplay
open Interlude.UI
open Interlude.Features.Online
open Interlude.Features.Play

type LevelSelectScreen() =
    inherit Screen()

    let TOP_BAR_HEIGHT = 150.0f
    let INFO_SCREEN_SPLIT = 0.4f

    let search_text = Setting.simple ""

    let info_panel =
        InfoPanel(Position = Position.ShrinkT(TOP_BAR_HEIGHT + 5.0f).SlicePercentL(INFO_SCREEN_SPLIT))

    let refresh () =
        SelectedChart.if_loaded (fun info -> info_panel.OnChartUpdated(info))
        Tree.refresh ()

    override this.Init(parent: Widget) =
        base.Init parent

        Setting.app (fun s -> if Sorting.modes.ContainsKey s then s else "title") options.ChartSortMode
        Setting.app (fun s -> if Grouping.modes.ContainsKey s then s else "pack") options.ChartGroupMode

        this
        |+ CurrentChart(Position = Position.SliceT(TOP_BAR_HEIGHT).SlicePercentL(INFO_SCREEN_SPLIT))
        |+ SearchBox(
            search_text,
            (fun f ->
                LevelSelect.filter <- f
                refresh ()
            ),
            Position =
                Position
                    .SliceT(TOP_BAR_HEIGHT)
                    .ShrinkB(50.0f)
                    .SliceY(60.0f)
                    .ShrinkPercentL(0.4f)
                    .ShrinkL(290.0f)
                    .ShrinkR(25.0f)
        )
            .Help(Help.Info("levelselect.search", "search"))

        |+ (
            Container(NodeType.None, Position = Position.ShrinkT(TOP_BAR_HEIGHT).ShrinkPercentL(INFO_SCREEN_SPLIT))
            |+ EmptyState(Icons.SEARCH, %"levelselect.empty.search")
                .Conditional(fun () -> search_text.Value <> "")
            |+ EmptyState(Icons.SIDEBAR, %"levelselect.empty.no_table")
                .Conditional(fun () ->
                    search_text.Value = ""
                    && options.ChartGroupMode.Value = "level"
                    && Content.Table.IsNone
                )
            |+ EmptyState(Icons.FOLDER, %"levelselect.empty.no_collections")
                .Conditional(fun () -> search_text.Value = "" && options.ChartGroupMode.Value = "collection")
            |+ EmptyState(Icons.FOLDER, %"levelselect.empty.no_charts")
                .Conditional(fun () -> search_text.Value = "" && options.ChartGroupMode.Value <> "collection" && options.ChartGroupMode.Value <> "level")
        )
            .Conditional(fun () -> Tree.is_empty)

        |+ StylishButton(
            LevelSelect.choose_this_chart,
            K (sprintf "%s %s" Icons.PLAY %"levelselect.play"),
            !%Palette.MAIN.O2,
            TiltRight = false,
            Position = Position.SliceB(50.0f).SliceR(250.0f)
        )
            .Help(Help.Info("levelselect.play").Hotkey("select"))
            .Conditional(fun () -> TreeState.multi_selection.IsNone)
        |+ StylishButton(
            (fun () ->
                SelectedChart.when_loaded true
                <| fun info ->
                    Screen.change_new
                        (fun () -> PracticeScreen.practice_screen (info, 0.0f<ms>))
                        ScreenType.Practice
                        Transitions.Default
                    |> ignore
            ),
            K Icons.TARGET,
            !%Palette.DARK.O2,
            Hotkey = "practice_mode",
            Position = Position.SliceB(50.0f).SliceR(60.0f).TranslateX(-275.0f)
        )
            .Help(Help.Info("levelselect.practice_mode").Hotkey("practice_mode"))
            .Conditional(fun () -> TreeState.multi_selection.IsNone)
        |+ StylishButton(
            (fun () -> LevelSelect.random_chart(); TreeState.click_debounce <- 500.0),
            K Icons.REFRESH_CCW,
            !%Palette.MAIN.O2,
            Position = Position.SliceB(50.0f).SliceR(60.0f).TranslateX(-360.0f)
        )
            .Help(Help.Info("levelselect.random_chart").Hotkey("random_chart"))
            .Conditional(fun () -> TreeState.multi_selection.IsNone)
        |+ StylishButton(
            (fun () -> SelectedChart.if_loaded(fun info -> ChartContextMenu(info.ChartMeta, info.LibraryContext).Show())),
            K Icons.LIST,
            !%Palette.DARK.O2,
            Position = Position.SliceB(50.0f).SliceR(60.0f).TranslateX(-445.0f)
        )
            .Help(Help.Info("levelselect.context_menu").Hotkey("context_menu"))
            .Conditional(fun () -> TreeState.multi_selection.IsNone)

        |+ StylishButton(
            (fun () -> TreeState.multi_selection <- None; TreeState.click_debounce <- 500.0),
            K (sprintf "%s %s" Icons.X %"levelselect.clear_multi_selection"),
            !%Palette.DARK.O2,
            TiltRight = false,
            Position = Position.SliceB(50.0f).SliceR(300.0f)
        )
            .Conditional(fun () -> TreeState.multi_selection.IsSome)
        |+ StylishButton(
            (fun () -> match TreeState.multi_selection with Some s -> s.ShowActions() | None -> ()),
            K (sprintf "%s %s" Icons.LIST %"bulk_actions"),
            !%Palette.MAIN.O2,
            Position = Position.SliceB(50.0f).SliceR(300.0f).TranslateX(-325.0f)
        )
            .Conditional(fun () -> TreeState.multi_selection.IsSome)

        |+ LibraryViewControls(Position = Position.SliceT(TOP_BAR_HEIGHT).SliceB(50.0f).ShrinkPercentL(INFO_SCREEN_SPLIT))
        |* info_panel

        LevelSelect.on_refresh_all.Add refresh
        Rulesets.on_changed.Add (fun _ ->
            match options.ChartGroupMode.Value with
            | "grade"
            | "lamp" -> LevelSelect.refresh_all()
            | _ -> LevelSelect.refresh_details()
        )

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

        Tree.update (this.Bounds.Top + TOP_BAR_HEIGHT, this.Bounds.Bottom, elapsed_ms)

    override this.Draw() =

        Tree.draw (this.Bounds.Top + TOP_BAR_HEIGHT, this.Bounds.Bottom)

        let w = this.Bounds.Width * INFO_SCREEN_SPLIT

        let {
                Rect.Left = left
                Top = top
                Right = right
            } =
            this.Bounds

        Render.quad
        <| Quad.from_points(
            (left, top),
            (left + w + TOP_BAR_HEIGHT * 0.5f, top),
            (left + w, top + TOP_BAR_HEIGHT),
            (left, top + TOP_BAR_HEIGHT)
        )
        <| Quad.gradient_top_to_bottom (!*Palette.MAIN_100) (!*Palette.DARK_100)

        Render.quad
        <| Quad.from_points(
            (left + w + TOP_BAR_HEIGHT * 0.5f, top),
            (right, top),
            (right, top + TOP_BAR_HEIGHT),
            (left + w, top + TOP_BAR_HEIGHT)
        )
        <| Colors.shadow_2.O2.AsQuad

        Render.rect (this.Bounds.SliceT(TOP_BAR_HEIGHT).BorderB(5.0f)) (!*Palette.MAIN)

        base.Draw()

    override this.OnEnter(_: ScreenType) =
        LevelSelect.exit_gameplay()
        Song.on_finish <- SongFinishAction.LoopFromPreview

        refresh ()
        DiscordRPC.in_menus ("Choosing a song")

    override this.OnExit(_: ScreenType) = Input.remove_listener ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some ScreenType.Lobby
        else
            Some ScreenType.MainMenu