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
        | HudElement.TimingDisplay -> %"hud.timingdisplay"
        | HudElement.Combo -> %"hud.combo"
        | HudElement.SkipButton -> %"hud.skipbutton"
        | HudElement.JudgementMeter -> %"hud.judgementmeter"
        | HudElement.EarlyLateMeter -> %"hud.earlylatemeter"
        | HudElement.ProgressMeter -> %"hud.progressmeter"
        | HudElement.JudgementCounter -> %"hud.judgementcounter"
        | HudElement.RateModMeter -> %"hud.ratemodmeter"
        | HudElement.BPMMeter -> %"hud.bpmmeter"
        | HudElement.Pacemaker -> %"hud.pacemaker"
        | HudElement.InputMeter -> %"hud.inputmeter"
        | HudElement.KeysPerSecondMeter -> %"hud.kps_meter"
        | HudElement.CustomImage -> %"hud.customimage"

    let tooltip (e: HudElement) : string =
        match e with
        | HudElement.Accuracy -> %"hud.accuracy.tooltip"
        | HudElement.TimingDisplay -> %"hud.timingdisplay.tooltip"
        | HudElement.Combo -> %"hud.combo.tooltip"
        | HudElement.SkipButton -> %"hud.skipbutton.tooltip"
        | HudElement.JudgementMeter -> %"hud.judgementmeter.tooltip"
        | HudElement.EarlyLateMeter -> %"hud.earlylatemeter.tooltip"
        | HudElement.ProgressMeter -> %"hud.progressmeter.tooltip"
        | HudElement.JudgementCounter -> %"hud.judgementcounter.tooltip"
        | HudElement.RateModMeter -> %"hud.ratemodmeter.tooltip"
        | HudElement.BPMMeter -> %"hud.bpmmeter.tooltip"
        | HudElement.Pacemaker -> %"hud.pacemaker.tooltip"
        | HudElement.InputMeter -> %"hud.inputmeter.tooltip"
        | HudElement.KeysPerSecondMeter -> %"hud.kps_meter.tooltip"
        | HudElement.CustomImage -> %"hud.customimage.tooltip"

    let can_configure (e: HudElement) =
        match e with
        | HudElement.SkipButton -> not Content.Noteskin.IsEmbedded
        | HudElement.BPMMeter -> false
        | HudElement.KeysPerSecondMeter -> false
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
        | HudElement.TimingDisplay -> cast TimingDisplay
        | HudElement.Combo -> cast Combo
        | HudElement.SkipButton -> cast SkipButton
        | HudElement.JudgementMeter -> cast JudgementMeter
        | HudElement.EarlyLateMeter -> cast EarlyLateMeter
        | HudElement.ProgressMeter -> cast ProgressMeter
        | HudElement.JudgementCounter -> cast JudgementCounter
        | HudElement.RateModMeter -> cast RateModMeter
        | HudElement.BPMMeter -> cast BPMMeter
        | HudElement.Pacemaker -> cast Pacemaker
        | HudElement.InputMeter -> cast InputMeter
        | HudElement.KeysPerSecondMeter -> cast KeysPerSecondMeter
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
        | HudElement.TimingDisplay ->
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
        | HudElement.JudgementMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            JudgementMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.JudgementMeterEnabled)
        | HudElement.EarlyLateMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            EarlyLateMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.EarlyLateMeterEnabled)
        | HudElement.ProgressMeter ->
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
        | HudElement.RateModMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            RateModMeterEnabled = v
                        }
                )
                (fun () -> Content.HUD.RateModMeterEnabled)
        | HudElement.BPMMeter ->
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
        | HudElement.KeysPerSecondMeter ->
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
        | HudElement.TimingDisplay ->
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
        | HudElement.JudgementMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            JudgementMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.JudgementMeterPosition)
        | HudElement.EarlyLateMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            EarlyLateMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.EarlyLateMeterPosition)
        | HudElement.ProgressMeter ->
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
        | HudElement.RateModMeter ->
            Setting.make
                (fun v ->
                    Skins.save_hud_config
                        { Content.HUD with
                            RateModMeterPosition = v
                        }
                )
                (fun () -> Content.HUD.RateModMeterPosition)
        | HudElement.BPMMeter ->
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
        | HudElement.KeysPerSecondMeter ->
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
        | HudElement.TimingDisplay -> all_defaults.TimingDisplayPosition
        | HudElement.Combo -> all_defaults.ComboPosition
        | HudElement.SkipButton -> all_defaults.SkipButtonPosition
        | HudElement.JudgementMeter -> all_defaults.JudgementMeterPosition
        | HudElement.EarlyLateMeter -> all_defaults.EarlyLateMeterPosition
        | HudElement.ProgressMeter -> all_defaults.ProgressMeterPosition
        | HudElement.JudgementCounter -> all_defaults.JudgementCounterPosition
        | HudElement.RateModMeter -> all_defaults.RateModMeterPosition
        | HudElement.BPMMeter -> all_defaults.BPMMeterPosition
        | HudElement.Pacemaker -> all_defaults.PacemakerPosition
        | HudElement.InputMeter -> all_defaults.InputMeterPosition
        | HudElement.KeysPerSecondMeter -> all_defaults.KeysPerSecondMeterPosition
        | HudElement.CustomImage -> all_defaults.CustomImagePosition