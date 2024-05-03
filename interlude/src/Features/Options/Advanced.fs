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
            .Tooltip(Tooltip.Info("advanced.confirmexit"))
            .Pos(2)
        |+ PageSetting("advanced.holdtogiveup", Checkbox options.HoldToGiveUp)
            .Tooltip(Tooltip.Info("advanced.holdtogiveup"))
            .Pos(4)
        |+ PageSetting("advanced.vanishingnotes", Checkbox options.VanishingNotes)
            .Tooltip(Tooltip.Info("advanced.vanishingnotes"))
            .Pos(6)
        |+ PageSetting("advanced.autocalibrateoffset", Checkbox options.AutoCalibrateOffset)
            .Tooltip(Tooltip.Info("advanced.autocalibrateoffset"))
            .Pos(8)
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
            .Tooltip(Tooltip.Info("advanced.buildpatterncache"))
            .Pos(10)
        |+ PageSetting("advanced.advancedrecommendations", Checkbox options.AdvancedRecommendations)
            .Tooltip(Tooltip.Info("advanced.advancedrecommendations"))
            .Pos(12)
        |> this.Content

    override this.Title = %"advanced.name"
    override this.OnClose() = ()
