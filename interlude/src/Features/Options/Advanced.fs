namespace Interlude.Features.OptionsMenu.Advanced

open Percyqaz.Flux.UI
open Prelude.Data.Library.Caching
open Prelude
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.OptionsMenu

type AdvancedPage() =
    inherit Page()

    override this.Content() =
        page_container ()
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
            .Pos(10)
        :> Widget

    override this.Title = %"advanced"
    override this.OnClose() = ()