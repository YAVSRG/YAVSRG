namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Flux.UI
open Prelude.Data.Library.Caching
open Prelude
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Collections
open Interlude.Features.Import.Mounts

type LibraryPage() =
    inherit Page()

    let main_options =
        NavigationContainer.Column(WrapNavigation = false, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Right = 0.5f %- 10.0f })
        |+ PageButton(
            %"library.collections",
            fun () -> ManageCollectionsPage().Show()
        )
            .Tooltip(Tooltip.Info("library.collections"))
            .Pos(0, 2, PageWidth.Full)
        |+ PageButton
            .Once(
                %"library.recache_charts",
                fun () ->
                    Cache.recache_service.Request(
                        Content.Cache,
                        fun () ->
                            Notifications.task_feedback (Icons.FOLDER, %"notification.recache_complete", "")
                    )

                    Notifications.action_feedback (Icons.FOLDER, %"notification.recache", "")
            )
            .Tooltip(Tooltip.Info("library.recache_charts"))
            .Pos(3, 2, PageWidth.Full)
        |+ PageButton
            .Once(
                %"library.recache_patterns",
                fun () ->
                    Cache.cache_patterns.Request(
                        (Content.Cache, true),
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
            .Tooltip(Tooltip.Info("library.recache_patterns"))
            .Pos(5, 2, PageWidth.Full)

    let mount_options =
        NavigationContainer.Column(WrapNavigation = false, Position = { Position.Margin(PRETTY_MARGIN_X, PRETTY_MARGIN_Y) with Left = 0.5f %+ 10.0f })
        |+ MountControl(MountedGameType.Osu, options.OsuMount, Position = Position.Row(100.0f, 150.0f))
        |+ MountControl(MountedGameType.Quaver, options.QuaverMount, Position = Position.Row(270.0f, 150.0f))
        |+ MountControl(MountedGameType.Stepmania, options.StepmaniaMount, Position = Position.Row(440.0f, 150.0f))
        |+ MountControl(MountedGameType.Etterna, options.EtternaMount, Position = Position.Row(610.0f, 150.0f))
        |+ Text(%"imports.mount", Align = Alignment.CENTER, Position = Position.Row(0.0f, 80.0f))

    override this.Content() =
        NavigationContainer.Row()
        |+ main_options
        |+ mount_options
        :> Widget

    override this.Title = %"library"
    override this.OnClose() = ()