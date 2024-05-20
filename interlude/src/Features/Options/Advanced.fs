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
        :> Widget

    override this.Title = %"advanced"
    override this.OnClose() = ()

 module Advanced =

    let search_settings (tokens: string array) : SearchResult seq =
        seq {
            if token_match tokens [|%"advanced.enableconsole"|] then
                yield PageSetting("advanced.enableconsole", Checkbox options.EnableConsole)
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.confirmexit"|] then
                yield PageSetting("advanced.confirmexit", Checkbox options.ConfirmExit)
                    .Tooltip(Tooltip.Info("advanced.confirmexit"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.holdtogiveup"|] then
                yield PageSetting("advanced.holdtogiveup", Checkbox options.HoldToGiveUp)
                    .Tooltip(Tooltip.Info("advanced.holdtogiveup"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.vanishingnotes"|] then
                yield PageSetting("advanced.vanishingnotes", Checkbox options.VanishingNotes)
                    .Tooltip(Tooltip.Info("advanced.vanishingnotes"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.autocalibrateoffset"|] then
                yield PageSetting("advanced.autocalibrateoffset", Checkbox options.AutoCalibrateOffset)
                    .Tooltip(Tooltip.Info("advanced.autocalibrateoffset"))
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.buildpatterncache"|] then
                yield PageButton.Once("advanced.buildpatterncache",
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
                , 2, 2, PageWidth.Normal
            if token_match tokens [|%"advanced.advancedrecommendations"|] then
                yield PageSetting("advanced.advancedrecommendations", Checkbox options.AdvancedRecommendations)
                    .Tooltip(Tooltip.Info("advanced.advancedrecommendations"))
                , 2, 2, PageWidth.Normal
        }