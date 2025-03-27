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
    let PLAY_BUTTON_WIDTH = 250.0f
    let BULK_ACTION_BUTTON_WIDTH = 300.0f
    let OTHER_BUTTONS_WIDTH = 60.0f

    let search_text = Setting.simple ""

    override this.Init(parent: Widget) =
        base.Init parent

        LevelSelect.on_refresh_all.Add Tree.refresh
        Rulesets.on_changed.Add (fun _ ->
            match options.ChartGroupMode.Value with
            | "grade"
            | "lamp" -> LevelSelect.refresh_all()
            | _ -> LevelSelect.refresh_details()
        )

        if not (Sorting.modes.ContainsKey options.ChartSortMode.Value) then
            options.ChartSortMode.Value <- "title"
        if not (Grouping.modes.ContainsKey options.ChartGroupMode.Value) then
            options.ChartGroupMode.Value <- "pack"

        this
            .With(
                CurrentChart()
                    .Position(Position.SliceT(TOP_BAR_HEIGHT).SlicePercentL(INFO_SCREEN_SPLIT)),

                SearchBox(search_text, fun f ->
                    LevelSelect.filter <- f
                    Tree.refresh ()
                )
                    .Position(
                        Position
                            .SliceT(TOP_BAR_HEIGHT)
                            .ShrinkB(AngledButton.HEIGHT)
                            .SliceY(SearchBox.HEIGHT)
                            .ShrinkPercentL(0.4f)
                            .ShrinkL(200.0f)
                            .ShrinkR((TOP_BAR_HEIGHT - AngledButton.HEIGHT - SearchBox.HEIGHT - Style.PADDING) * 0.5f)
                    )
                    .Help(Help.Info("levelselect.search", "search")),

                InfoPanel()
                    .Position(Position.ShrinkT(TOP_BAR_HEIGHT + 5.0f).SlicePercentL(INFO_SCREEN_SPLIT)),

                // Empty states explaining why there are no charts to show
                Container(NodeType.None)
                    .Position(Position.ShrinkT(TOP_BAR_HEIGHT).ShrinkPercentL(INFO_SCREEN_SPLIT))
                    .WithConditional(
                        (fun () -> search_text.Value <> ""),
                        EmptyState(Icons.SEARCH, %"levelselect.empty.search")
                    )
                    .WithConditional(
                        (fun () ->
                            search_text.Value = ""
                            && options.ChartGroupMode.Value = "level"
                            && Content.Table.IsNone
                        ),
                        EmptyState(Icons.SIDEBAR, %"levelselect.empty.no_table")
                    )
                    .WithConditional(
                        (fun () ->
                            search_text.Value = ""
                            && options.ChartGroupMode.Value = "collection"
                        ),
                        EmptyState(Icons.FOLDER, %"levelselect.empty.no_collections")
                    )
                    .WithConditional(
                        (fun () ->
                            search_text.Value = ""
                            && options.ChartGroupMode.Value <> "collection"
                            && options.ChartGroupMode.Value <> "level"
                        ),
                        EmptyState(Icons.FOLDER, %"levelselect.empty.no_charts")
                    )
                    .Conditional(fun () -> Tree.is_empty)
            )
            // Normal chart actions (no bulk select)
            .WithConditional(
                (fun () -> TreeState.multi_selection.IsNone),

                AngledButton(
                    sprintf "%s %s" Icons.PLAY %"levelselect.play",
                    LevelSelect.choose_this_chart,
                    Palette.MAIN.O2
                )
                    .LeanRight(false)
                    .Position(Position.SliceB(AngledButton.HEIGHT).SliceR(PLAY_BUTTON_WIDTH))
                    .Help(Help.Info("levelselect.play", "select")),

                AngledButton(
                    Icons.TARGET,
                    (fun () ->
                        SelectedChart.when_loaded true
                        <| fun info ->
                            Screen.change_new
                                (fun () -> PracticeScreen.practice_screen (info, 0.0f<ms>))
                                ScreenType.Practice
                                Transitions.Default
                            |> ignore
                    ),
                    Palette.DARK.O2
                )
                    .Hotkey("practice_mode")
                    .Position(
                        Position
                            .SliceB(AngledButton.HEIGHT)
                            .SliceR(OTHER_BUTTONS_WIDTH)
                            .TranslateX(-PLAY_BUTTON_WIDTH - AngledButton.LEAN_AMOUNT)
                    )
                    .Help(Help.Info("levelselect.practice_mode", "practice_mode")),

                AngledButton(
                    Icons.REFRESH_CCW,
                    (fun () -> LevelSelect.random_chart(); TreeState.click_debounce <- 500.0),
                    Palette.MAIN.O2
                )
                    .Position(
                        Position
                            .SliceB(AngledButton.HEIGHT)
                            .SliceR(OTHER_BUTTONS_WIDTH)
                            .TranslateX(-PLAY_BUTTON_WIDTH - OTHER_BUTTONS_WIDTH - AngledButton.LEAN_AMOUNT * 2.0f)
                    )
                    .Help(Help.Info("levelselect.random_chart", "random_chart")),

                AngledButton(
                    Icons.LIST,
                    (fun () -> SelectedChart.if_loaded(fun info -> ChartContextMenu(info.ChartMeta, info.LibraryContext).Show())),
                    Palette.DARK.O2
                )
                    .Position(
                        Position
                            .SliceB(AngledButton.HEIGHT)
                            .SliceR(OTHER_BUTTONS_WIDTH)
                            .TranslateX(-PLAY_BUTTON_WIDTH - OTHER_BUTTONS_WIDTH * 2.0f - AngledButton.LEAN_AMOUNT * 3.0f)
                    )
                    .Help(Help.Info("levelselect.context_menu").Hotkey("context_menu"))
            )
            // Bulk select actions
            .WithConditional(
                (fun () -> TreeState.multi_selection.IsSome),

                AngledButton(
                    sprintf "%s %s" Icons.X %"levelselect.clear_multi_selection",
                    (fun () -> TreeState.multi_selection <- None; TreeState.click_debounce <- 500.0),
                    Palette.DARK.O2
                )
                    .LeanRight(false)
                    .Position(Position.SliceB(AngledButton.HEIGHT).SliceR(BULK_ACTION_BUTTON_WIDTH)),

                AngledButton(
                    sprintf "%s %s" Icons.LIST %"bulk_actions",
                    (fun () -> match TreeState.multi_selection with Some s -> s.ShowActions() | None -> ()),
                    Palette.MAIN.O2
                )
                    .Position(Position.SliceB(AngledButton.HEIGHT).SliceR(BULK_ACTION_BUTTON_WIDTH).TranslateX(-BULK_ACTION_BUTTON_WIDTH - AngledButton.LEAN_AMOUNT))
            )
            .Add(
                // Goes last so that its dropdowns draw over action buttons
                LibraryViewControls()
                    .Position(Position.SliceT(TOP_BAR_HEIGHT).SliceB(50.0f).ShrinkPercentL(INFO_SCREEN_SPLIT))
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if (%%"select").Pressed() then
            LevelSelect.choose_this_chart ()

        elif (%%"next").Pressed() then
            Tree.next ()
        elif (%%"previous").Pressed() then
            Tree.previous ()
        elif (%%"next_group").Pressed() then
            Tree.next_group ()
        elif (%%"previous_group").Pressed() then
            Tree.previous_group ()
        elif (%%"start").Pressed() then
            Tree.top_of_group ()
        elif (%%"end").Pressed() then
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

        Render.quad_points_c
            (left, top)
            (left + w + TOP_BAR_HEIGHT * 0.5f, top)
            (left + w, top + TOP_BAR_HEIGHT)
            (left, top + TOP_BAR_HEIGHT)
            (Quad.gradient_top_to_bottom (!*Palette.MAIN_100) (!*Palette.DARK_100))

        Render.quad_points
            (left + w + TOP_BAR_HEIGHT * 0.5f, top)
            (right, top)
            (right, top + TOP_BAR_HEIGHT)
            (left + w, top + TOP_BAR_HEIGHT)
            Colors.shadow_2.O2

        Render.rect (this.Bounds.SliceT(TOP_BAR_HEIGHT).BorderB(5.0f)) (!*Palette.MAIN)

        base.Draw()

    override this.OnEnter(_: ScreenType) =
        LevelSelect.exit_gameplay()
        Song.on_finish <- SongFinishAction.LoopFromPreview

        Tree.refresh ()
        DiscordRPC.in_menus ("Choosing a song")

    override this.OnExit(_: ScreenType) = Input.remove_listener ()

    override this.OnBack() =
        if Network.lobby.IsSome then
            Some ScreenType.Lobby
        else
            Some ScreenType.MainMenu