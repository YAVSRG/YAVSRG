namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Play.HUD

// todo: move most of this to prelude and/or make them extension methods
module HudElement =

    let private show_pacemaker = Setting.simple true
    let private show_skip_button = Setting.simple true

    let name (element: HudElement) : string =
        match element with
        | HudElement.Accuracy -> %"hud.accuracy"
        | HudElement.ErrorBar -> %"hud.error_bar"
        | HudElement.Combo -> %"hud.combo"
        | HudElement.SkipButton -> %"hud.skip_button"
        | HudElement.Judgement -> %"hud.judgement"
        | HudElement.EarlyLate -> %"hud.early_late"
        | HudElement.ProgressPie -> %"hud.progress_pie"
        | HudElement.JudgementCounter -> %"hud.judgement_counter"
        | HudElement.RateMods -> %"hud.ratemodmeter"
        | HudElement.BPM -> %"hud.bpm"
        | HudElement.Pacemaker -> %"hud.pacemaker"
        | HudElement.InputMeter -> %"hud.input_meter"
        | HudElement.KeysPerSecond -> %"hud.kps_meter"
        | HudElement.CustomImage -> %"hud.custom_image"

    let tooltip (element: HudElement) : string =
        match element with
        | HudElement.Accuracy -> %"hud.accuracy.tooltip"
        | HudElement.ErrorBar -> %"hud.error_bar.tooltip"
        | HudElement.Combo -> %"hud.combo.tooltip"
        | HudElement.SkipButton -> %"hud.skip_button.tooltip"
        | HudElement.Judgement -> %"hud.judgement.tooltip"
        | HudElement.EarlyLate -> %"hud.early_late.tooltip"
        | HudElement.ProgressPie -> %"hud.progress_pie.tooltip"
        | HudElement.JudgementCounter -> %"hud.judgement_counter.tooltip"
        | HudElement.RateMods -> %"hud.ratemodmeter.tooltip"
        | HudElement.BPM -> %"hud.bpm.tooltip"
        | HudElement.Pacemaker -> %"hud.pacemaker.tooltip"
        | HudElement.InputMeter -> %"hud.input_meter.tooltip"
        | HudElement.KeysPerSecond -> %"hud.kps_meter.tooltip"
        | HudElement.CustomImage -> %"hud.custom_image.tooltip"

    let can_configure (element: HudElement) : bool =
        match element with
        | HudElement.BPM -> false
        | HudElement.Pacemaker -> false
        | _ -> true

    let can_toggle (element: HudElement) : bool =
        match element with
        | HudElement.Pacemaker -> false
        | _ -> true

    let constructor (element: HudElement) : HudConfig * PlayState -> Widget =
        let inline cast (f: ^T -> ^U) = fun x -> f x :> Widget

        match element with
        | HudElement.Accuracy -> cast Accuracy
        | HudElement.ErrorBar -> cast ErrorBar
        | HudElement.Combo -> cast Combo
        | HudElement.SkipButton -> cast SkipButton
        | HudElement.Judgement -> cast Judgement
        | HudElement.EarlyLate -> cast EarlyLate
        | HudElement.ProgressPie -> cast ProgressPie
        | HudElement.JudgementCounter -> cast JudgementCounter
        | HudElement.RateMods -> cast RateMods
        | HudElement.BPM -> cast BPM
        | HudElement.Pacemaker -> cast Pacemaker
        | HudElement.InputMeter -> cast InputMeter
        | HudElement.KeysPerSecond -> cast KeysPerSecond
        | HudElement.CustomImage -> cast CustomImage

    let enabled_setting (element: HudElement) : Setting<bool> =
        match element with
        | HudElement.Accuracy ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            AccuracyEnabled = v
                        }
                )
                (fun () -> Content.HUD.AccuracyEnabled)
        | HudElement.ErrorBar ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            TimingDisplayEnabled = v
                        }
                )
                (fun () -> Content.HUD.TimingDisplayEnabled)
        | HudElement.Combo ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            ComboEnabled = v
                        }
                )
                (fun () -> Content.HUD.ComboEnabled)
        | HudElement.SkipButton -> show_skip_button
        | HudElement.Judgement ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            JudgementMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.JudgementMeterEnabled)
        | HudElement.EarlyLate ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            EarlyLateMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.EarlyLateMeterEnabled)
        | HudElement.ProgressPie ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            ProgressMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.ProgressMeterEnabled)
        | HudElement.JudgementCounter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            JudgementCounterEnabled = v
                        }
                )
                (fun () -> Content.HUD.JudgementCounterEnabled)
        | HudElement.RateMods ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            RateModMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.RateModMeterEnabled)
        | HudElement.BPM ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            BPMMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.BPMMeterEnabled)
        | HudElement.Pacemaker -> show_pacemaker
        | HudElement.InputMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            InputMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.InputMeterEnabled)
        | HudElement.KeysPerSecond ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            KeysPerSecondMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.KeysPerSecondMeterEnabled)
        | HudElement.CustomImage ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            CustomImageEnabled = v
                        }
                )
                (fun () -> Content.HUD.CustomImageEnabled)

    let position_setting (e: HudElement) : Setting<HudPosition> =
        match e with
        | HudElement.Accuracy ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            AccuracyPosition = v
                        }
                )
                (fun () -> Content.HUD.AccuracyPosition)
        | HudElement.ErrorBar ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            TimingDisplayPosition = v
                        }
                )
                (fun () -> Content.HUD.TimingDisplayPosition)
        | HudElement.Combo ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            ComboPosition = v
                        }
                )
                (fun () -> Content.HUD.ComboPosition)
        | HudElement.SkipButton ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            SkipButtonPosition = v
                        }
                )
                (fun () -> Content.HUD.SkipButtonPosition)
        | HudElement.Judgement ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            JudgementMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.JudgementMeterPosition)
        | HudElement.EarlyLate ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            EarlyLateMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.EarlyLateMeterPosition)
        | HudElement.ProgressPie ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            ProgressMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.ProgressMeterPosition)
        | HudElement.JudgementCounter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            JudgementCounterPosition = v
                        }
                )
                (fun () -> Content.HUD.JudgementCounterPosition)
        | HudElement.RateMods ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            RateModMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.RateModMeterPosition)
        | HudElement.BPM ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            BPMMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.BPMMeterPosition)
        | HudElement.Pacemaker ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            PacemakerPosition = v
                        }
                )
                (fun () -> Content.HUD.PacemakerPosition)
        | HudElement.InputMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            InputMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.InputMeterPosition)
        | HudElement.KeysPerSecond ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            KeysPerSecondMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.KeysPerSecondMeterPosition)
        | HudElement.CustomImage ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            CustomImagePosition = v
                        }
                )
                (fun () -> Content.HUD.CustomImagePosition)

    let default_position (e: HudElement) : HudPosition =
        let all_defaults = HudConfig.Default

        match e with
        | HudElement.Accuracy -> all_defaults.AccuracyPosition
        | HudElement.ErrorBar ->
            if Content.HUD.TimingDisplayRotation <> ErrorBarRotation.Normal then
                all_defaults.TimingDisplayPosition.Rotate
            else all_defaults.TimingDisplayPosition
        | HudElement.Combo -> all_defaults.ComboPosition
        | HudElement.SkipButton -> all_defaults.SkipButtonPosition
        | HudElement.Judgement -> all_defaults.JudgementMeterPosition
        | HudElement.EarlyLate -> all_defaults.EarlyLateMeterPosition
        | HudElement.ProgressPie -> all_defaults.ProgressMeterPosition
        | HudElement.JudgementCounter -> all_defaults.JudgementCounterPosition
        | HudElement.RateMods -> all_defaults.RateModMeterPosition
        | HudElement.BPM -> all_defaults.BPMMeterPosition
        | HudElement.Pacemaker -> all_defaults.PacemakerPosition
        | HudElement.InputMeter -> all_defaults.InputMeterPosition
        | HudElement.KeysPerSecond -> all_defaults.KeysPerSecondMeterPosition
        | HudElement.CustomImage -> all_defaults.CustomImagePosition