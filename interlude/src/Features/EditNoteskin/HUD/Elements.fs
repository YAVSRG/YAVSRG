namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.Options
open Interlude.Features.Play
open Interlude.Features.Play.HUD

module HUDElement =

    let name (e: HUDElement) : string =
        match e with
        | HUDElement.Accuracy -> %"hud.accuracy"
        | HUDElement.TimingDisplay -> %"hud.timingdisplay"
        | HUDElement.Combo -> %"hud.combo"
        | HUDElement.SkipButton -> %"hud.skipbutton"
        | HUDElement.JudgementMeter -> %"hud.judgementmeter"
        | HUDElement.EarlyLateMeter -> %"hud.earlylatemeter"
        | HUDElement.ProgressMeter -> %"hud.progressmeter"
        | HUDElement.JudgementCounter -> %"hud.judgementcounter"
        | HUDElement.RateModMeter -> %"hud.ratemodmeter"
        | HUDElement.BPMMeter -> %"hud.bpmmeter"
        | HUDElement.Pacemaker -> %"hud.pacemaker"
        | HUDElement.InputMeter -> %"hud.inputmeter"

    let tooltip (e: HUDElement) : string =
        match e with
        | HUDElement.Accuracy -> %"hud.accuracy.tooltip"
        | HUDElement.TimingDisplay -> %"hud.timingdisplay.tooltip"
        | HUDElement.Combo -> %"hud.combo.tooltip"
        | HUDElement.SkipButton -> %"hud.skipbutton.tooltip"
        | HUDElement.JudgementMeter -> %"hud.judgementmeter.tooltip"
        | HUDElement.EarlyLateMeter -> %"hud.earlylatemeter.tooltip"
        | HUDElement.ProgressMeter -> %"hud.progressmeter.tooltip"
        | HUDElement.JudgementCounter -> %"hud.judgementcounter.tooltip"
        | HUDElement.RateModMeter -> %"hud.ratemodmeter.tooltip"
        | HUDElement.BPMMeter -> %"hud.bpmmeter.tooltip"
        | HUDElement.Pacemaker -> %"hud.pacemaker.tooltip"
        | HUDElement.InputMeter -> %"hud.inputmeter.tooltip"

    let can_configure (e: HUDElement) =
        match e with
        | HUDElement.SkipButton -> false
        | HUDElement.BPMMeter -> false
        | HUDElement.Pacemaker -> false
        | _ -> true
    
    let can_toggle (e: HUDElement) =
        match e with
        | HUDElement.Pacemaker -> false
        | _ -> true

    let constructor (e: HUDElement) : HUDUserOptions * HUDNoteskinOptions * PlayState -> Widget =
        let inline cast (f: ^T -> ^U) = fun x -> f x :> Widget

        match e with
        | HUDElement.Accuracy -> cast Accuracy
        | HUDElement.TimingDisplay -> cast TimingDisplay
        | HUDElement.Combo -> cast Combo
        | HUDElement.SkipButton -> cast SkipButton
        | HUDElement.JudgementMeter -> cast JudgementMeter
        | HUDElement.EarlyLateMeter -> cast EarlyLateMeter
        | HUDElement.ProgressMeter -> cast ProgressMeter
        | HUDElement.JudgementCounter -> cast JudgementCounter
        | HUDElement.RateModMeter -> cast RateModMeter
        | HUDElement.BPMMeter -> cast BPMMeter
        | HUDElement.Pacemaker -> cast Pacemaker
        | HUDElement.InputMeter -> cast InputMeter

    let enabled_setting (e: HUDElement) : Setting<bool> =
        match e with
        | HUDElement.Accuracy ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            AccuracyEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.AccuracyEnabled)
        | HUDElement.TimingDisplay ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            TimingDisplayEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.TimingDisplayEnabled)
        | HUDElement.Combo ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            ComboEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.ComboEnabled)
        | HUDElement.SkipButton ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            SkipButtonEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.SkipButtonEnabled)
        | HUDElement.JudgementMeter ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            JudgementMeterEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.JudgementMeterEnabled)
        | HUDElement.EarlyLateMeter ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            EarlyLateMeterEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.EarlyLateMeterEnabled)
        | HUDElement.ProgressMeter ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            ProgressMeterEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.ProgressMeterEnabled)
        | HUDElement.JudgementCounter ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            JudgementCounterEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.JudgementCounterEnabled)
        | HUDElement.RateModMeter ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            RateModMeterEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.RateModMeterEnabled)
        | HUDElement.BPMMeter ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            BPMMeterEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.BPMMeterEnabled)
        | HUDElement.InputMeter ->
            Setting.make
                (fun v ->
                    options.HUD.Set
                        { options.HUD.Value with
                            InputMeterEnabled = v
                        }
                )
                (fun () -> options.HUD.Value.InputMeterEnabled)
        | HUDElement.Pacemaker -> // pacemaker cannot be toggled by the user like a setting, it is on depending on gameplay state
            Setting.make (fun _ -> ()) (fun () -> true)

    let position_setting (e: HUDElement) : Setting<HUDPosition> =
        match e with
        | HUDElement.Accuracy ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            AccuracyPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.AccuracyPosition)
        | HUDElement.TimingDisplay ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            TimingDisplayPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.TimingDisplayPosition)
        | HUDElement.Combo ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            ComboPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.ComboPosition)
        | HUDElement.SkipButton ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            SkipButtonPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.SkipButtonPosition)
        | HUDElement.JudgementMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            JudgementMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.JudgementMeterPosition)
        | HUDElement.EarlyLateMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            EarlyLateMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.EarlyLateMeterPosition)
        | HUDElement.ProgressMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            ProgressMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.ProgressMeterPosition)
        | HUDElement.JudgementCounter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            JudgementCounterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.JudgementCounterPosition)
        | HUDElement.RateModMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            RateModMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.RateModMeterPosition)
        | HUDElement.BPMMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            BPMMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.BPMMeterPosition)
        | HUDElement.InputMeter ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            InputMeterPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.InputMeterPosition)
        | HUDElement.Pacemaker ->
            Setting.make
                (fun v ->
                    Noteskins.save_hud_config
                        { Content.NoteskinConfig.HUD with
                            PacemakerPosition = v
                        }
                )
                (fun () -> Content.NoteskinConfig.HUD.PacemakerPosition)

    let default_position (e: HUDElement) : HUDPosition =
        let all_defaults = HUDNoteskinOptions.Default

        match e with
        | HUDElement.Accuracy -> all_defaults.AccuracyPosition
        | HUDElement.TimingDisplay -> all_defaults.TimingDisplayPosition
        | HUDElement.Combo -> all_defaults.ComboPosition
        | HUDElement.SkipButton -> all_defaults.SkipButtonPosition
        | HUDElement.JudgementMeter -> all_defaults.JudgementMeterPosition
        | HUDElement.EarlyLateMeter -> all_defaults.EarlyLateMeterPosition
        | HUDElement.ProgressMeter -> all_defaults.ProgressMeterPosition
        | HUDElement.JudgementCounter -> all_defaults.JudgementCounterPosition
        | HUDElement.RateModMeter -> all_defaults.RateModMeterPosition
        | HUDElement.BPMMeter -> all_defaults.BPMMeterPosition
        | HUDElement.InputMeter -> all_defaults.InputMeterPosition
        | HUDElement.Pacemaker -> all_defaults.PacemakerPosition

    let show_menu (e: HUDElement) (on_close: unit -> unit) =
        match e with
        | HUDElement.Accuracy -> AccuracyPage(on_close).Show()
        | HUDElement.TimingDisplay -> TimingDisplayPage(on_close).Show()
        | HUDElement.Combo -> ComboPage(on_close).Show()
        | HUDElement.SkipButton -> SkipButtonPage(on_close).Show()
        | HUDElement.JudgementMeter -> JudgementMeterPage(on_close).Show()
        | HUDElement.EarlyLateMeter -> EarlyLateMeterPage(on_close).Show()
        | HUDElement.ProgressMeter -> ProgressMeterPage(on_close).Show()
        | HUDElement.JudgementCounter -> JudgementCounterPage(on_close).Show()
        | HUDElement.RateModMeter -> RateModMeterPage(on_close).Show()
        | HUDElement.BPMMeter -> BPMMeterPage(on_close).Show()
        | HUDElement.InputMeter -> InputMeterPage(on_close).Show()
        | HUDElement.Pacemaker -> PacemakerPage(on_close).Show()