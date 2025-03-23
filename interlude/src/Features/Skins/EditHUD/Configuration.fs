namespace Interlude.Features.Skins.EditHUD

open Prelude.Skins.HudLayouts

[<AutoOpen>]
module private ConfigurationPageHelper =

    let show_menu (element: HudElement) (on_close: unit -> unit) : unit =
        match element with
        | HudElement.Accuracy -> AccuracyPage(on_close).Show()
        | HudElement.ErrorBar -> ErrorBarPage(on_close).Show()
        | HudElement.Combo -> ComboPage(on_close).Show()
        | HudElement.SkipButton -> SkipButtonPage(on_close).Show()
        | HudElement.Judgement -> JudgementPage(on_close).Show()
        | HudElement.EarlyLate -> EarlyLatePage(on_close).Show()
        | HudElement.ProgressPie -> ProgressPiePage(on_close).Show()
        | HudElement.JudgementCounter -> JudgementCounterPage(on_close).Show()
        | HudElement.RateMods -> RateModsPage(on_close).Show()
        | HudElement.BPM -> BPMPage(on_close).Show()
        | HudElement.InputMeter -> InputMeterPage(on_close).Show()
        | HudElement.Pacemaker -> PacemakerPage(on_close).Show()
        | HudElement.KeysPerSecond -> KeysPerSecondPage(on_close).Show()
        | HudElement.CustomImage -> CustomImagePage(on_close).Show()