namespace Interlude.Features.Skins

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Play
open Interlude.Features.Play.HUD

module HudElement =

    let name (e: HudElement) : string =
        match e with
        | HudElement.Accuracy -> %"hud.accuracy"
        | HudElement.HitDeviations -> %"hud.hit_deviations"
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

    let tooltip (e: HudElement) : string =
        match e with
        | HudElement.Accuracy -> %"hud.accuracy.tooltip"
        | HudElement.HitDeviations -> %"hud.hit_deviations.tooltip"
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

    let can_configure (e: HudElement) =
        match e with
        | HudElement.SkipButton -> not Content.Noteskin.IsEmbedded
        | HudElement.BPM -> false
        | HudElement.KeysPerSecond -> false
        | HudElement.Pacemaker -> false
        | _ -> true

    let can_toggle (e: HudElement) =
        match e with
        | HudElement.Pacemaker -> false
        | _ -> true

    let constructor (e: HudElement) : HudConfig * PlayState -> Widget =
        let inline cast (f: ^T -> ^U) = fun x -> f x :> Widget

        match e with
        | HudElement.Accuracy -> cast Accuracy
        | HudElement.HitDeviations -> cast HitDeviations
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

    let enabled_setting (e: HudElement) : Setting<bool> =
        match e with
        | HudElement.Accuracy ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            AccuracyEnabled = v
                        }
                )
                (fun () -> Content.HUD.AccuracyEnabled)
        | HudElement.HitDeviations ->
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
        | HudElement.SkipButton ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            SkipButtonEnabled = v
                        }
                )
                (fun () -> Content.HUD.SkipButtonEnabled)
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
        | HudElement.Pacemaker -> // pacemaker cannot be toggled by the user like a setting, it is on depending on gameplay state
            Setting.make (fun _ -> ()) (fun () -> true)
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
        | HudElement.HitDeviations ->
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
        | HudElement.HitDeviations ->
            if Content.HUD.TimingDisplayRotation <> HitDeviationsRotation.Normal then
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