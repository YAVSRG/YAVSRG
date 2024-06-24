namespace Interlude.Features.Noteskins.Edit

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins
open Prelude.Skinning.HudLayouts
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

    let can_configure (e: HudElement) =
        match e with
        | HudElement.SkipButton -> not Content.Noteskin.IsEmbedded
        | HudElement.BPMMeter -> false
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

    let enabled_setting (e: HudElement) : Setting<bool> =
        match e with
        | HudElement.Accuracy ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            AccuracyEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.AccuracyEnabled)
        | HudElement.TimingDisplay ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            TimingDisplayEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.TimingDisplayEnabled)
        | HudElement.Combo ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            ComboEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.ComboEnabled)
        | HudElement.SkipButton ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            SkipButtonEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.SkipButtonEnabled)
        | HudElement.JudgementMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            JudgementMeterEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.JudgementMeterEnabled)
        | HudElement.EarlyLateMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            EarlyLateMeterEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.EarlyLateMeterEnabled)
        | HudElement.ProgressMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            ProgressMeterEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.ProgressMeterEnabled)
        | HudElement.JudgementCounter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            JudgementCounterEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.JudgementCounterEnabled)
        | HudElement.RateModMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            RateModMeterEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.RateModMeterEnabled)
        | HudElement.BPMMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            BPMMeterEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.BPMMeterEnabled)
        | HudElement.InputMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            InputMeterEnabled = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.InputMeterEnabled)
        | HudElement.Pacemaker -> // pacemaker cannot be toggled by the user like a setting, it is on depending on gameplay state
            Setting.make (fun _ -> ()) (fun () -> true)

    let position_setting (e: HudElement) : Setting<HudPosition> =
        match e with
        | HudElement.Accuracy ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            AccuracyPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.AccuracyPosition)
        | HudElement.TimingDisplay ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            TimingDisplayPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.TimingDisplayPosition)
        | HudElement.Combo ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            ComboPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.ComboPosition)
        | HudElement.SkipButton ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            SkipButtonPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.SkipButtonPosition)
        | HudElement.JudgementMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            JudgementMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.JudgementMeterPosition)
        | HudElement.EarlyLateMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            EarlyLateMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.EarlyLateMeterPosition)
        | HudElement.ProgressMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            ProgressMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.ProgressMeterPosition)
        | HudElement.JudgementCounter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            JudgementCounterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.JudgementCounterPosition)
        | HudElement.RateModMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            RateModMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.RateModMeterPosition)
        | HudElement.BPMMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            BPMMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.BPMMeterPosition)
        | HudElement.InputMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            InputMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.InputMeterPosition)
        | HudElement.Pacemaker ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            PacemakerPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.PacemakerPosition)

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
        | HudElement.InputMeter -> all_defaults.InputMeterPosition
        | HudElement.Pacemaker -> all_defaults.PacemakerPosition

    let show_menu (e: HudElement) (on_close: unit -> unit) =
        match e with
        | HudElement.Accuracy -> AccuracyPage(on_close).Show()
        | HudElement.TimingDisplay -> TimingDisplayPage(on_close).Show()
        | HudElement.Combo -> ComboPage(on_close).Show()
        | HudElement.SkipButton -> SkipButtonPage(on_close).Show()
        | HudElement.JudgementMeter -> JudgementMeterPage(on_close).Show()
        | HudElement.EarlyLateMeter -> EarlyLateMeterPage(on_close).Show()
        | HudElement.ProgressMeter -> ProgressMeterPage(on_close).Show()
        | HudElement.JudgementCounter -> JudgementCounterPage(on_close).Show()
        | HudElement.RateModMeter -> RateModMeterPage(on_close).Show()
        | HudElement.BPMMeter -> BPMMeterPage(on_close).Show()
        | HudElement.InputMeter -> InputMeterPage(on_close).Show()
        | HudElement.Pacemaker -> PacemakerPage(on_close).Show()