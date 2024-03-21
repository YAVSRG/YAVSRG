namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Flux.UI
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay

type EditHUDPage() as this =
    inherit Page()

    let grid =
        GridFlowContainer<_>(PRETTYHEIGHT, 2, Spacing = (20.0f, 20.0f), Position = Position.Margin(100.0f, 200.0f))

    let open_hud_editor () =
        if
            Chart.WITH_COLORS.IsSome
            && Screen.change_new
                (fun () -> HUDEditor.edit_hud_screen (Chart.CHART.Value, Chart.WITH_COLORS.Value))
                Screen.Type.Practice
                Transitions.Flags.Default
        then
            Menu.Exit()

    do
        this.Content(
            grid
            |+ PageButton("hud.accuracymeter", (fun () -> AccuracyPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.accuracymeter"))
            |+ PageButton("hud.hitmeter", (fun () -> TimingDisplayPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.hitmeter"))
            |+ PageButton("hud.combo", (fun () -> ComboPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.combo"))
            |+ PageButton("hud.skipbutton", (fun () -> SkipButtonPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.skipbutton"))
            |+ PageButton("hud.progressmeter", (fun () -> ProgressMeterPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.progressmeter"))
            |+ PageButton("hud.pacemaker", (fun () -> PacemakerPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.pacemaker"))
            |+ PageButton("hud.judgementcounts", (fun () -> JudgementCounterPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.judgementcounts"))
            |+ PageButton("hud.judgementmeter", (fun () -> JudgementMeterPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.judgementmeter"))
            |+ PageButton("hud.earlylatemeter", (fun () -> EarlyLateMeterPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.earlylatemeter"))
            |+ PageButton("hud.ratemodmeter", (fun () -> RateModMeterPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.ratemodmeter"))
            |+ PageButton("hud.bpmmeter", (fun () -> BPMMeterPage(ignore).Show()))
                .Tooltip(Tooltip.Info("hud.bpmmeter"))
            |+ PageButton("hud.edit", open_hud_editor).Tooltip(Tooltip.Info("hud.edit"))
        )

    override this.Title = %"hud.name"
    override this.OnClose() = ()
