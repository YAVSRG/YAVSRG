namespace Interlude.Features.OptionsMenu

open Prelude.Data.Library.Caching
open Interlude.Utils
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

module Advanced =

    type AdvancedPage() as this =
        inherit Page()

        do

            this.Content(
                page_container ()
                |+ PageSetting("advanced.enableconsole", Selector<_>.FromBool options.EnableConsole)
                    .Pos(0)
                |+ PageSetting("advanced.confirmexit", Selector<_>.FromBool options.ConfirmExit)
                    .Pos(2)
                    .Tooltip(Tooltip.Info("advanced.confirmexit"))
                |+ PageSetting("advanced.holdtogiveup", Selector<_>.FromBool options.HoldToGiveUp)
                    .Pos(4)
                    .Tooltip(Tooltip.Info("advanced.holdtogiveup"))
                |+ PageSetting("advanced.vanishingnotes", Selector<_>.FromBool options.VanishingNotes)
                    .Pos(6)
                    .Tooltip(Tooltip.Info("advanced.vanishingnotes"))
                |+ PageSetting("advanced.autocalibrateoffset", Selector<_>.FromBool options.AutoCalibrateOffset)
                    .Pos(8)
                    .Tooltip(Tooltip.Info("advanced.autocalibrateoffset"))
                |+ PageButton
                    .Once(
                        "advanced.buildpatterncache",
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
                    .Pos(10)
                    .Tooltip(Tooltip.Info("advanced.buildpatterncache"))
                |+ PageSetting("advanced.advancedrecommendations", Selector<_>.FromBool options.AdvancedRecommendations)
                    .Pos(12)
                    .Tooltip(Tooltip.Info("advanced.advancedrecommendations"))
            )

        override this.Title = %"advanced.name"
        override this.OnClose() = ()
