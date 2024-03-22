namespace Interlude.Features.OptionsMenu.Noteskins

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
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
        | HUDElement.Pacemaker -> PacemakerPage(on_close).Show()

type SubPositioner(drag: (float32 * float32) * (float32 * float32) -> unit, finish_drag: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false

    override this.Update(elapsed_ms, moved) =

        hover <- Mouse.hover this.Bounds

        match dragging_from with
        | Some(x, y) ->
            let new_x, new_y = Mouse.pos ()
            drag ((x, y), (new_x, new_y))

            if not (Mouse.held Mouse.LEFT) then
                dragging_from <- None
                finish_drag ()
                this.Focus true
        | None ->
            if hover && Mouse.left_click () then
                dragging_from <- Some(Mouse.pos ())
                this.Select true

        base.Update(elapsed_ms, moved)

    override this.Draw() =
        if this.Parent.Focused then
            if hover then
                Draw.rect this.Bounds Colors.white.O3
            else
                Draw.rect this.Bounds Colors.white.O1

type Positioner(elem: HUDElement, ctx: PositionerContext) =
    inherit StaticContainer(NodeType.FocusTrap)

    let round (offset: float32, anchor: float32) =
        System.MathF.Round(offset / 5.0f) * 5.0f, anchor

    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false
    let mutable repeat = -1
    let mutable time = 0.0
    let REPEAT_DELAY = 400.0
    let REPEAT_INTERVAL = 40.0

    let child =
        HUDElement.constructor elem (options.HUD.Value, Content.NoteskinConfig.HUD, ctx.State)

    let position = HUDElement.position_setting elem

    let mutable new_unsaved_pos: Position = Position.Default

    let validate_pos (parent_bounds: Rect) (pos: Position) =
        let bounds = Position.calculate pos parent_bounds

        if bounds.Left + 5.0f > bounds.Right || bounds.Top + 5.0f > bounds.Bottom then
            { pos with
                Right = pos.Left ^+ max 5.0f bounds.Width
                Bottom = pos.Top ^+ max 5.0f bounds.Height
            }
        else
            pos

    let save_pos () =
        position.Set
            { position.Value with
                Left = new_unsaved_pos.Left
                Top = new_unsaved_pos.Top
                Right = new_unsaved_pos.Right
                Bottom = new_unsaved_pos.Bottom
            }

    override this.Position
        with set value =

            let value =
                if this.Initialised then
                    let bounds = Position.calculate value this.Parent.Bounds

                    if bounds.Left + 5.0f > bounds.Right || bounds.Top + 5.0f > bounds.Bottom then
                        { value with
                            Right = value.Left ^+ max 5.0f bounds.Width
                            Bottom = value.Top ^+ max 5.0f bounds.Height
                        }
                    else
                        value
                else
                    value

            base.set_Position value
            new_unsaved_pos <- value

    member this.Move(x, y) =
        let current = position.Value

        this.Position <-
            {
                Left = current.Left ^+ x
                Top = current.Top ^+ y
                Right = current.Right ^+ x
                Bottom = current.Bottom ^+ y
            }

        save_pos ()

    override this.Init(parent) =
        this
        |+ SubPositioner(
            (fun ((old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left
                        Top = current.Top
                        Right = current.Right ^+ (new_x - old_x) |> round
                        Bottom = current.Bottom ^+ (new_y - old_y) |> round
                    }
            ),
            save_pos,
            Position = Position.BorderBottomCorners(10.0f).SliceRight(10.0f)
        )
        |+ SubPositioner(
            (fun ((old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left ^+ (new_x - old_x) |> round
                        Top = current.Top
                        Right = current.Right
                        Bottom = current.Bottom ^+ (new_y - old_y) |> round
                    }
            ),
            save_pos,
            Position = Position.BorderBottomCorners(10.0f).SliceLeft(10.0f)
        )
        |+ SubPositioner(
            (fun ((old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left
                        Top = current.Top ^+ (new_y - old_y) |> round
                        Right = current.Right ^+ (new_x - old_x) |> round
                        Bottom = current.Bottom
                    }
            ),
            save_pos,
            Position = Position.BorderTopCorners(10.0f).SliceRight(10.0f)
        )
        |+ SubPositioner(
            (fun ((old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left ^+ (new_x - old_x) |> round
                        Top = current.Top ^+ (new_y - old_y) |> round
                        Right = current.Right
                        Bottom = current.Bottom
                    }
            ),
            save_pos,
            Position = Position.BorderTopCorners(10.0f).SliceLeft(10.0f)
        )

        |+ SubPositioner(
            (fun ((old_x, _), (new_x, _)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left ^+ (new_x - old_x) |> round
                        Top = current.Top
                        Right = current.Right
                        Bottom = current.Bottom
                    }
            ),
            save_pos,
            Position =
                { Position.BorderLeft(10.0f) with
                    Top = 0.5f %- 5.0f
                    Bottom = 0.5f %+ 5.0f
                }
        )
        |+ SubPositioner(
            (fun ((_, old_y), (_, new_y)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left
                        Top = current.Top ^+ (new_y - old_y) |> round
                        Right = current.Right
                        Bottom = current.Bottom
                    }
            ),
            save_pos,
            Position =
                { Position.BorderTop(10.0f) with
                    Left = 0.5f %- 5.0f
                    Right = 0.5f %+ 5.0f
                }
        )
        |+ SubPositioner(
            (fun ((old_x, _), (new_x, _)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left
                        Top = current.Top
                        Right = current.Right ^+ (new_x - old_x) |> round
                        Bottom = current.Bottom
                    }
            ),
            save_pos,
            Position =
                { Position.BorderRight(10.0f) with
                    Top = 0.5f %- 5.0f
                    Bottom = 0.5f %+ 5.0f
                }
        )
        |+ SubPositioner(
            (fun ((_, old_y), (_, new_y)) ->
                let current = position.Value

                this.Position <-
                    {
                        Left = current.Left
                        Top = current.Top
                        Right = current.Right
                        Bottom = current.Bottom ^+ (new_y - old_y) |> round
                    }
            ),
            save_pos,
            Position =
                { Position.BorderBottom(10.0f) with
                    Left = 0.5f %- 5.0f
                    Right = 0.5f %+ 5.0f
                }
        )
        |* child

        base.Init parent

    override this.Update(elapsed_ms, moved) =

        let mutable moved = moved

        if this.Focused then
            let u = (%%"up").Tapped()
            let d = (%%"down").Tapped()
            let l = (%%"left").Tapped()
            let r = (%%"right").Tapped()

            if u || d || l || r then
                repeat <- 0
                time <- 0

                if u then
                    this.Move(0.0f, -5.0f)

                if d then
                    this.Move(0.0f, 5.0f)

                if l then
                    this.Move(-5.0f, 0.0f)

                if r then
                    this.Move(5.0f, 0.0f)

            if repeat >= 0 then
                let u = (%%"up").Pressed()
                let d = (%%"down").Pressed()
                let l = (%%"left").Pressed()
                let r = (%%"right").Pressed()

                time <- time + elapsed_ms

                if (float repeat * REPEAT_INTERVAL + REPEAT_DELAY < time) then
                    repeat <- repeat + 1

                    if u then
                        this.Move(0.0f, -5.0f)

                    if d then
                        this.Move(0.0f, 5.0f)

                    if l then
                        this.Move(-5.0f, 0.0f)

                    if r then
                        this.Move(5.0f, 0.0f)

                if not (u || d || l || r) then
                    repeat <- -1

        hover <- Mouse.hover this.Bounds

        match dragging_from with
        | Some(x, y) ->
            let current = position.Value
            let new_x, new_y = Mouse.pos ()
            moved <- true

            this.Position <-
                {
                    Left = current.Left ^+ (new_x - x) |> round
                    Top = current.Top ^+ (new_y - y) |> round
                    Right = current.Right ^+ (new_x - x) |> round
                    Bottom = current.Bottom ^+ (new_y - y) |> round
                }

            if not (Mouse.held Mouse.LEFT) then
                dragging_from <- None
                save_pos ()
                this.Focus true
        | None ->
            if hover && Mouse.left_click () then
                dragging_from <- Some(Mouse.pos ())
                this.Select true

        base.Update(elapsed_ms, moved)

    override this.OnSelected by_mouse =
        base.OnSelected by_mouse
        ctx.Selected <- Some elem

    override this.Draw() =
        if dragging_from.IsSome then
            let pos = position.Value
            let left_axis = this.Parent.Bounds.Left + this.Parent.Bounds.Width * snd pos.Left
            let right_axis = this.Parent.Bounds.Left + this.Parent.Bounds.Width * snd pos.Right

            Draw.rect
                (Rect.Create(left_axis - 2.5f, this.Parent.Bounds.Top, right_axis + 2.5f, this.Parent.Bounds.Bottom))
                Colors.red_accent.O1

            Draw.rect
                (Rect.Create(right_axis - 2.5f, this.Parent.Bounds.Top, right_axis + 2.5f, this.Parent.Bounds.Bottom))
                Colors.red_accent.O1

            let this_center_x, this_center_y = this.Bounds.Center

            for other_positioner in ctx.Positioners.Values do
                if other_positioner = this then
                    ()
                else

                let other_center_x, other_center_y = other_positioner.Bounds.Center

                if abs (this_center_x - other_center_x) < 5.0f then
                    Draw.rect
                        (Rect.Create(
                            other_center_x - 2.5f,
                            min this.Bounds.Top other_positioner.Bounds.Top,
                            other_center_x + 2.5f,
                            max this.Bounds.Bottom other_positioner.Bounds.Bottom
                        ))
                        Colors.green_accent.O1

                if abs (this_center_y - other_center_y) < 5.0f then
                    Draw.rect
                        (Rect.Create(
                            min this.Bounds.Left other_positioner.Bounds.Left,
                            other_center_y - 2.5f,
                            max this.Bounds.Right other_positioner.Bounds.Right,
                            other_center_y + 2.5f
                        ))
                        Colors.green_accent.O1

        if this.Focused then
            Draw.rect (this.Bounds.BorderTopCorners Style.PADDING) Colors.yellow_accent
            Draw.rect (this.Bounds.BorderBottomCorners Style.PADDING) Colors.yellow_accent
            Draw.rect (this.Bounds.BorderLeft Style.PADDING) Colors.yellow_accent
            Draw.rect (this.Bounds.BorderRight Style.PADDING) Colors.yellow_accent
        elif hover then
            Draw.rect (this.Bounds.BorderTopCorners Style.PADDING) Colors.white.O2
            Draw.rect (this.Bounds.BorderBottomCorners Style.PADDING) Colors.white.O2
            Draw.rect (this.Bounds.BorderLeft Style.PADDING) Colors.white.O2
            Draw.rect (this.Bounds.BorderRight Style.PADDING) Colors.white.O2

        base.Draw()

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

        Selection.clear ()
        let enabled = HUDElement.enabled_setting e

        if enabled.Value then

            let setting = HUDElement.position_setting e

            let p = Positioner(e, this)
            let pos = setting.Value

            p.Position <-
                {
                    Left = pos.Left
                    Top = pos.Top
                    Right = pos.Right
                    Bottom = pos.Bottom
                }

            if pos.RelativeToPlayfield then
                this.Playfield.Add p
            else
                this.Screen.Add p

            this.Positioners <- this.Positioners.Add(e, p)

            if this.Selected = Some e then
                p.Focus true

type PositionerInfo(ctx: PositionerContext) =
    inherit FrameContainer(NodeType.None, Fill = K Colors.shadow_2.O3, Border = K Colors.cyan_accent)

    let mutable left = true

    override this.Init(parent) =
        this
        |+ Text(
            (fun () ->
                match ctx.Selected with
                | Some e -> HUDElement.name e
                | None -> ""
            ),
            Position = Position.SliceTop(80.0f).Margin(20.0f, 5.0f)
        )
        |+ Button(
            Icons.REFRESH_CW + " " + %"hud.generic.reset",
            (fun () ->
                match ctx.Selected with
                | Some e ->
                    HUDElement.position_setting(e).Set(HUDElement.default_position e)
                    sync (fun () -> ctx.Create e)
                | None -> ()
            ),
            Position = Position.Row(85.0f, 60.0f).Margin(20.0f, 0.0f)
        )
        |+ Button(
            Icons.SETTINGS + " " + %"hud.generic.options",
            (fun () ->
                match ctx.Selected with
                | Some e -> HUDElement.show_menu e (fun () -> ctx.Create e)
                | None -> ()
            ),
            Position = Position.Row(145.0f, 60.0f).Margin(20.0f, 0.0f)
        )
        |* Dummy()

        this.Position <- Position.SliceLeft(500.0f).SliceBottom(540.0f).Margin(20.0f)
        base.Init parent

    override this.Update(elapsed_ms, moved) =
        if ctx.Selected.IsNone then
            ()
        else

        let mutable moved = moved

        if
            left
            && ctx.Positioners.[ctx.Selected.Value].Bounds.Left - 100.0f < this.Bounds.Right
        then
            left <- false
            moved <- true
            this.Position <- Position.SliceRight(500.0f).SliceBottom(540.0f).Margin(20.0f)
        elif
            not left
            && ctx.Positioners.[ctx.Selected.Value].Bounds.Right + 100.0f > this.Bounds.Left
        then
            left <- true
            moved <- true
            this.Position <- Position.SliceLeft(500.0f).SliceBottom(540.0f).Margin(20.0f)

        base.Update(elapsed_ms, moved)

module HUDEditor =

    let edit_hud_screen (chart: Chart, with_colors: ColoredChart) =

        let replay_data: IReplayProvider =
            StoredReplayProvider.WavingAutoPlay(with_colors.Keys, with_colors.Source.Notes)

        let FIRST_NOTE = with_colors.FirstNote
        let ruleset = Rulesets.current

        let mutable replay_data = replay_data

        let mutable scoring =
            Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes Gameplay.rate.Value

        let mutable time = -Time.infinity

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider.WavingAutoPlay(with_colors.Keys, with_colors.Source.Notes)
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
                    HUDElement.Pacemaker
                ]
                |> Seq.iter ctx.Create

                this |* Conditional((fun () -> ctx.Selected.IsSome), PositionerInfo ctx)
            // todo: way to turn on multiplayer player list

            override this.OnEnter p =
                DiscordRPC.in_menus ("Customising HUD")
                // todo: don't play leadin
                base.OnEnter p

            override this.Update(elapsed_ms, moved) =
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                if chart_time < time then
                    seek_backwards this

                time <- chart_time

                base.Update(elapsed_ms, moved)

                if not replay_data.Finished then
                    scoring.Update chart_time
                else
                    Song.seek 0.0f<ms>
        }
