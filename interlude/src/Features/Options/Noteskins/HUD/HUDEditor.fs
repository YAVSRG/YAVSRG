namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Charts.Processing.NoteColors
open Prelude.Gameplay
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Play.HUD

module HUDElement =

    let name (e: HUDElement) : string =
        match e with
        | HUDElement.Accuracy -> %"hud.accuracy.name"
        | HUDElement.TimingDisplay -> %"hud.timingdisplay.name"
        | HUDElement.Combo -> %"hud.combo.name"
        | HUDElement.SkipButton -> %"hud.skipbutton.name"
        | HUDElement.JudgementMeter -> %"hud.judgementmeter.name"
        | HUDElement.EarlyLateMeter -> %"hud.earlylatemeter.name"
        | HUDElement.ProgressMeter -> %"hud.progressmeter.name"
        | HUDElement.JudgementCounter -> %"hud.judgementcounter.name"
        | HUDElement.RateModMeter -> %"hud.ratemodmeter.name"
        | HUDElement.BPMMeter -> %"hud.bpmmeter.name"
        | HUDElement.Pacemaker -> %"hud.pacemaker.name"

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

    let enabled_setting (e: HUDElement) : Setting<bool> =
        match e with
        | HUDElement.Accuracy -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with AccuracyEnabled = v })
                (fun () -> options.HUD.Value.AccuracyEnabled)
        | HUDElement.TimingDisplay -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with TimingDisplayEnabled = v })
                (fun () -> options.HUD.Value.TimingDisplayEnabled)
        | HUDElement.Combo -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with ComboEnabled = v })
                (fun () -> options.HUD.Value.ComboEnabled)
        | HUDElement.SkipButton -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with SkipButtonEnabled = v })
                (fun () -> options.HUD.Value.SkipButtonEnabled)
        | HUDElement.JudgementMeter -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with JudgementMeterEnabled = v })
                (fun () -> options.HUD.Value.JudgementMeterEnabled)
        | HUDElement.EarlyLateMeter -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with EarlyLateMeterEnabled = v })
                (fun () -> options.HUD.Value.EarlyLateMeterEnabled)
        | HUDElement.ProgressMeter -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with ProgressMeterEnabled = v })
                (fun () -> options.HUD.Value.ProgressMeterEnabled)
        | HUDElement.JudgementCounter -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with JudgementCounterEnabled = v })
                (fun () -> options.HUD.Value.JudgementCounterEnabled)
        | HUDElement.RateModMeter -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with RateModMeterEnabled = v })
                (fun () -> options.HUD.Value.RateModMeterEnabled)
        | HUDElement.BPMMeter -> 
            Setting.make
                (fun v -> options.HUD.Set { options.HUD.Value with BPMMeterEnabled = v })
                (fun () -> options.HUD.Value.BPMMeterEnabled)
        | HUDElement.Pacemaker -> // pacemaker cannot be toggled by the user like a setting, it is on depending on gameplay state
            Setting.make
                (fun _ -> ())
                (fun () -> true)

    let position_setting (e: HUDElement) : Setting<HUDPosition> =
        match e with
        | HUDElement.Accuracy -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with AccuracyPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.AccuracyPosition)
        | HUDElement.TimingDisplay -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with TimingDisplayPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.TimingDisplayPosition)
        | HUDElement.Combo -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with ComboPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.ComboPosition)
        | HUDElement.SkipButton -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with SkipButtonPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.SkipButtonPosition)
        | HUDElement.JudgementMeter -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with JudgementMeterPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.JudgementMeterPosition)
        | HUDElement.EarlyLateMeter -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with EarlyLateMeterPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.EarlyLateMeterPosition)
        | HUDElement.ProgressMeter -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with ProgressMeterPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.ProgressMeterPosition)
        | HUDElement.JudgementCounter -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with JudgementCounterPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.JudgementCounterPosition)
        | HUDElement.RateModMeter -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with RateModMeterPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.RateModMeterPosition)
        | HUDElement.BPMMeter -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with BPMMeterPosition = v })
                (fun () -> Content.NoteskinConfig.HUD.BPMMeterPosition)
        | HUDElement.Pacemaker -> 
            Setting.make
                (fun v -> Noteskins.save_hud_config { Content.NoteskinConfig.HUD with PacemakerPosition = v })
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
        | HUDElement.Pacemaker -> all_defaults.PacemakerPosition

type Positioner(elem: HUDElement, ctx: PositionerContext) =
    inherit StaticWidget(NodeType.FocusTrap)

    let mutable dragging_from: (float32 * float32) option = None

    let child = HUDElement.constructor elem (options.HUD.Value, Content.NoteskinConfig.HUD, ctx.State)
    let position = HUDElement.position_setting elem

    override this.Init(parent) =
        base.Init parent
        child.Init this

    override this.Update(elapsed_ms, moved) =

        let mutable moved = moved

        match dragging_from with
        | Some (x, y) ->
            let current = position.Value
            let new_x, new_y = Mouse.pos()
            moved <- true
            this.Position <- 
                {
                    Left = current.Left ^+ (new_x - x)
                    Top = current.Top ^+ (new_y - y)
                    Right = current.Right ^+ (new_x - x)
                    Bottom = current.Bottom ^+ (new_y - y)
                }
            if not (Mouse.held Mouse.LEFT) then
                dragging_from <- None
                position.Value <- 
                    { position.Value with 
                        Left = current.Left ^+ (new_x - x)
                        Top = current.Top ^+ (new_y - y)
                        Right = current.Right ^+ (new_x - x)
                        Bottom = current.Bottom ^+ (new_y - y)
                    }
                this.Focus true
        | None -> 
            if Mouse.hover this.Bounds && Mouse.left_click () then
                dragging_from <- Some (Mouse.pos())
                this.Select true

        base.Update(elapsed_ms, moved)
        child.Update(elapsed_ms, moved)

    override this.OnSelected by_mouse =
        base.OnSelected by_mouse
        ctx.Selected <- Some elem

    override this.Draw() = 
        child.Draw()

and PositionerContext =
    {
        Screen: IPlayScreen
        Playfield: Playfield
        State: PlayState
        mutable Selected: HUDElement option
        mutable Positioners: Map<HUDElement, Positioner>
    }
    member this.Create(e: HUDElement) =
        match this.Positioners.TryFind e with
        | Some existing -> (this.Playfield.Remove existing || this.Screen.Remove existing) |> ignore
        | None -> ()
        Selection.clear()
        let enabled = HUDElement.enabled_setting e
        if enabled.Value then

            let setting = HUDElement.position_setting e

            let p = Positioner(e, this)
            let pos = setting.Value
            p.Position <- { Left = pos.Left; Top = pos.Top; Right = pos.Right; Bottom = pos.Bottom }
            if pos.RelativeToPlayfield then this.Playfield.Add p else this.Screen.Add p
            this.Positioners <- this.Positioners.Add(e, p)
            if this.Selected = Some e then
                p.Focus true

type PositionerInfo(ctx: PositionerContext) =
    inherit FrameContainer(NodeType.None, Fill = K Colors.shadow_2.O3, Border = K Colors.cyan_accent)

    let mutable left = true

    override this.Init(parent) =
        this
        |+ Text((fun () -> match ctx.Selected with Some e -> HUDElement.name e | None -> ""), Position = Position.SliceTop(80.0f).Margin(20.0f, 5.0f))
        |+ Button(
            Icons.REFRESH_CW + " " + "Reset position",
            (fun () -> 
                match ctx.Selected with 
                | Some e -> 
                    HUDElement.position_setting(e).Set(HUDElement.default_position e)
                    sync (fun () -> ctx.Create e)
                | None -> ()
            ),
            Position = Position.Row(85.0f, 60.0f).Margin(20.0f, 0.0f)
        )
        |* Dummy()
        this.Position <- Position.SliceLeft(500.0f).SliceBottom(540.0f).Margin(20.0f)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        if ctx.Selected.IsNone then () else

        let mutable moved = moved

        if left && ctx.Positioners.[ctx.Selected.Value].Bounds.Left - 100.0f < this.Bounds.Right then
            left <- false
            moved <- true
            this.Position <- Position.SliceRight(500.0f).SliceBottom(540.0f).Margin(20.0f)
        elif not left && ctx.Positioners.[ctx.Selected.Value].Bounds.Right + 100.0f > this.Bounds.Left then
            left <- true
            moved <- true
            this.Position <- Position.SliceLeft(500.0f).SliceBottom(540.0f).Margin(20.0f)
        base.Update(elapsed_ms, moved)

module HUDEditor =

    let edit_hud_screen (chart: Chart, with_colors: ColoredChart) =

        let replay_data : IReplayProvider = StoredReplayProvider.AutoPlay(with_colors.Keys, with_colors.Source.Notes)
        
        let FIRST_NOTE = with_colors.FirstNote
        let ruleset = Rulesets.current

        let mutable replay_data = replay_data

        let mutable scoring =
            Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes Gameplay.rate.Value

        let mutable time = -Time.infinity

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider.AutoPlay(with_colors.Keys, with_colors.Source.Notes)
            scoring <- Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes Gameplay.rate.Value
            screen.State.ChangeScoring scoring

        { new IPlayScreen(chart, with_colors, PacemakerInfo.None, scoring) with
            override this.AddWidgets() =

                let ctx =
                    {
                        Screen = this
                        Playfield = this.Playfield
                        State = this.State
                        Selected = None
                        Positioners = Map.empty
                    }

                [
                    HUDElement.Combo
                    HUDElement.SkipButton
                    HUDElement.ProgressMeter
                    HUDElement.Accuracy
                    HUDElement.TimingDisplay
                    HUDElement.JudgementCounter
                    HUDElement.JudgementMeter
                    HUDElement.EarlyLateMeter
                    HUDElement.RateModMeter
                    HUDElement.BPMMeter
                ] |> Seq.iter ctx.Create

                this |* Conditional((fun () -> ctx.Selected.IsSome), PositionerInfo ctx)
                // todo: way to turn on pacemaker/skip button/multiplayer player list

            override this.OnEnter p =
                DiscordRPC.in_menus ("Customising HUD")
                Song.on_finish <- SongFinishAction.LoopFromBeginning
                // todo: don't play leadin
                base.OnEnter p

            override this.Update(elapsed_ms, moved) =
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                if chart_time < time then seek_backwards this
                time <- chart_time

                base.Update(elapsed_ms, moved)
                
                if not replay_data.Finished then
                    scoring.Update chart_time
                else Song.seek 0.0f<ms>
        }
