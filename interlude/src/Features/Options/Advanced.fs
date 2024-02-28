namespace Interlude.Features.OptionsMenu

open Prelude.Data.Charts.Library
open Interlude.Utils
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

module Advanced =

    type AdvancedPage() as this =
        inherit Page()

        do

            this.Content(
                column ()
                |+ PageSetting("advanced.enableconsole", Selector<_>.FromBool options.EnableConsole)
                    .Pos(200.0f)
                |+ PageSetting("advanced.confirmexit", Selector<_>.FromBool options.ConfirmExit)
                    .Pos(270.0f)
                    .Tooltip(Tooltip.Info("advanced.confirmexit"))
                |+ PageSetting("advanced.vanishingnotes", Selector<_>.FromBool options.VanishingNotes)
                    .Pos(340.0f)
                    .Tooltip(Tooltip.Info("advanced.vanishingnotes"))
                |+ PageSetting("advanced.autocalibrateoffset", Selector<_>.FromBool options.AutoCalibrateOffset)
                    .Pos(410.0f)
                    .Tooltip(Tooltip.Info("advanced.autocalibrateoffset"))
                |+ PageButton
                    .Once(
                        "advanced.buildpatterncache",
                        fun () ->
                            cache_patterns.Request(
                                (),
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
                    .Pos(480.0f)
                    .Tooltip(Tooltip.Info("advanced.buildpatterncache"))
                |+ PageSetting("advanced.advancedrecommendations", Selector<_>.FromBool options.AdvancedRecommendations)
                    .Pos(550.0f)
                    .Tooltip(Tooltip.Info("advanced.advancedrecommendations"))
            )

        override this.Title = %"advanced.name"
        override this.OnClose() = ()
