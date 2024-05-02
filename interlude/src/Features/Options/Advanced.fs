namespace Interlude.Features.OptionsMenu.Advanced

open Prelude.Data.Library.Caching
open Prelude
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type AdvancedPage() as this =
    inherit Page()

    do
        page_container ()
        |+ PageSetting("advanced.enableconsole", Checkbox options.EnableConsole)
            .Pos(0)
        |+ PageSetting("advanced.confirmexit", Checkbox options.ConfirmExit)
            .Pos(2)
            .Tooltip(Tooltip.Info("advanced.confirmexit"))
        |+ PageSetting("advanced.holdtogiveup", Checkbox options.HoldToGiveUp)
            .Pos(4)
            .Tooltip(Tooltip.Info("advanced.holdtogiveup"))
        |+ PageSetting("advanced.vanishingnotes", Checkbox options.VanishingNotes)
            .Pos(6)
            .Tooltip(Tooltip.Info("advanced.vanishingnotes"))
        |+ PageSetting("advanced.autocalibrateoffset", Checkbox options.AutoCalibrateOffset)
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
        |+ PageSetting("advanced.advancedrecommendations", Checkbox options.AdvancedRecommendations)
            .Pos(12)
            .Tooltip(Tooltip.Info("advanced.advancedrecommendations"))
        |> this.Content

    override this.Title = %"advanced.name"
    override this.OnClose() = ()
