namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Import
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Mounts
open Interlude.Features.LevelSelect

type LibraryPage() =
    inherit Page()

    let import_info =
        Container(NodeType.None, Position = pretty_pos(PAGE_BOTTOM - 4, 4, PageWidth.Custom 300.0f))
        |+ Text(
            (fun () ->
                if ImportsInProgress.import_in_progress () then
                    %"imports.in_progress"
                else
                    %"imports.not_in_progress"
            ),
            Color =
                (fun () ->
                    if ImportsInProgress.import_in_progress () then
                        Colors.text_green
                    else
                        Colors.text_subheading
                ),
            Position = Position.SliceT(40.0f).Shrink(20.0f, 0.0f)
        )
        |+ LoadingIndicator.Strip(
            ImportsInProgress.import_in_progress,
            Position = Position.SliceT(40.0f, Style.PADDING).Shrink(150.0f, 0.0f)
        )
        |+ Text(
            sprintf "%i charts installed" Content.Library.Charts.Entries.Count,
            Color = K Colors.text_subheading,
            Position = Position.SliceT(65.0f, 30.0f).Shrink(20.0f, 0.0f)
        )

    let main_options =
        NavigationContainer.Column(WrapNavigation = false, Position = { Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Right = 0.5f %- 10.0f })
        |+ PageButton(
            %"library.collections",
            (fun () -> ManageCollectionsPage().Show()),
            Icon = Icons.FOLDER
        )
            .Help(Help.Info("library.collections"))
            .Pos(0, 2, PageWidth.Full)
        |+ PageButton(
            %"library.tables",
            (fun () -> SelectTablePage(LevelSelect.refresh_all).Show()),
            Icon = Icons.SIDEBAR
        )
            .Help(Help.Info("library.tables"))
            .Pos(2, 2, PageWidth.Full)
        |+ PageButton
            .Once(
                %"library.recache_patterns",
                fun () ->
                    ChartDatabase.recalculate_data.Request(
                        Content.Charts,
                        fun () ->
                            Notifications.system_feedback (
                                Icons.ALERT_OCTAGON,
                                %"notification.pattern_cache_complete.title",
                                ""
                            )
                    )

                    Notifications.system_feedback (
                        Icons.ALERT_OCTAGON,
                        %"notification.pattern_cache_started.title",
                        %"notification.pattern_cache_started.body"
                    )
            )
            .Help(Help.Info("library.recache_patterns"))
            .Pos(5, 2, PageWidth.Full)
        |+ PageButton
            .Once(
                %"library.recalculate_personal_bests",
                PersonalBests.recalculate
            )
            .Help(Help.Info("library.recalculate_personal_bests"))
            .Pos(7, 2, PageWidth.Full)
        |+ PageButton(
                %"levelselect.options",
                (fun () -> LevelSelectOptionsPage().Show())
            )
            .Pos(10, 2, PageWidth.Full)
        |+ import_info

    let mount_options =
        NavigationContainer.Column(WrapNavigation = false, Position = { Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Left = 0.5f %+ 10.0f })
        |+ MountControl(MountedGameType.Osu, options.OsuMount, Position = Position.SliceT(100.0f, 150.0f))
        |+ MountControl(MountedGameType.Quaver, options.QuaverMount, Position = Position.SliceT(270.0f, 150.0f))
        |+ MountControl(MountedGameType.Etterna, options.EtternaMount, Position = Position.SliceT(440.0f, 150.0f))
        |+ MountControl(MountedGameType.Stepmania, options.StepmaniaMount, Position = Position.SliceT(610.0f, 150.0f))
        |+ Text(%"imports.mount", Align = Alignment.CENTER, Position = Position.SliceT(0.0f, 80.0f))

    override this.Content() =
        NavigationContainer.Row()
        |+ main_options
        |+ mount_options
        :> Widget

    override this.Title = %"library"
    override this.OnClose() = ()