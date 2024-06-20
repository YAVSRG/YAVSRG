namespace Interlude.Features.OptionsMenu.Library

open Percyqaz.Flux.UI
open Prelude.Data.Library.Caching
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type LibraryPage() =
    inherit Page()

    override this.Content() =
        page_container ()
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
            .Pos(0)
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
            .Pos(2)
        :> Widget

    override this.Title = %"library"
    override this.OnClose() = ()