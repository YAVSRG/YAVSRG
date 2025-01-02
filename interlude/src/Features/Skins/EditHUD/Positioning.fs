namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Interlude.Options
open Interlude.UI
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Pacemaker
open Interlude.Features.Skins

[<AutoOpen>]
module private ElementMenus =

    let show_menu (e: HudElement) (on_close: unit -> unit) =
        match e with
        | HudElement.Accuracy -> AccuracyPage(on_close).Show()
        | HudElement.HitDeviations -> HitDeviationsPage(on_close).Show()
        | HudElement.Combo -> ComboPage(on_close).Show()
        | HudElement.SkipButton -> SkipButtonPage(on_close).Show()
        | HudElement.Judgement -> JudgementPage(on_close).Show()
        | HudElement.EarlyLate -> EarlyLatePage(on_close).Show()
        | HudElement.ProgressPie -> ProgressPiePage(on_close).Show()
        | HudElement.JudgementCounter -> JudgementCounterPage(on_close).Show()
        | HudElement.RateMods -> RateModPage(on_close).Show()
        | HudElement.BPM -> BPMPage(on_close).Show()
        | HudElement.InputMeter -> InputMeterPage(on_close).Show()
        | HudElement.Pacemaker -> PacemakerPage(on_close).Show()
        | HudElement.KeysPerSecond -> KeysPerSecondPage(on_close).Show()
        | HudElement.CustomImage -> CustomImagePage(on_close).Show()

type SubPositioner(drag: bool * (float32 * float32) * (float32 * float32) -> unit, finish_drag: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false

    let SHIFT = Bind.mk Keys.LeftShift

    override this.Update(elapsed_ms, moved) =

        hover <- this.Parent.Focused && Mouse.hover this.Bounds

        match dragging_from with
        | Some(x, y) ->
            let new_x, new_y = Mouse.pos ()
            drag (Input.held SHIFT, (x, y), (new_x, new_y))

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
                Render.rect this.Bounds Colors.white.O3
            else
                Render.rect this.Bounds Colors.white.O1

type Positioner(elem: HudElement, ctx: PositionerContext) =
    inherit Container(NodeType.FocusTrap)

    let round (offset: float32, anchor: float32) =
        System.MathF.Round(offset / 5.0f) * 5.0f, anchor

    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false

    let SMALL_UP = (%%"up").WithModifiers(false, false, true)
    let SMALL_DOWN = (%%"down").WithModifiers(false, false, true)
    let SMALL_LEFT = (%%"left").WithModifiers(false, false, true)
    let SMALL_RIGHT = (%%"right").WithModifiers(false, false, true)

    let child =
        HudElement.constructor elem (Content.HUD, ctx.State)

    let position = HudElement.position_setting elem

    let mutable new_unsaved_pos: Position = Position.DEFAULT

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
        |+ child
        |+ SubPositioner(
            (fun (preserve_center, (old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left ^- (new_x - old_x) |> round
                            Top = current.Top ^- (new_x - old_x) |> round
                            Right = current.Right ^+ (new_x - old_x) |> round
                            Bottom = current.Bottom ^+ (new_x - old_x) |> round
                        }
                else
                    this.Position <-
                        {
                            Left = current.Left
                            Top = current.Top
                            Right = current.Right ^+ (new_x - old_x) |> round
                            Bottom = current.Bottom ^+ (new_y - old_y) |> round
                        }
            ),
            save_pos,
            Position = Position.BorderCornersB(10.0f).SliceR(10.0f)
        )
        |+ SubPositioner(
            (fun (preserve_center, (old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left ^+ (new_x - old_x) |> round
                            Top = current.Top ^- (new_y - old_y) |> round
                            Right = current.Right ^- (new_x - old_x) |> round
                            Bottom = current.Bottom ^+ (new_y - old_y) |> round
                        }
                else
                    this.Position <-
                        {
                            Left = current.Left ^+ (new_x - old_x) |> round
                            Top = current.Top
                            Right = current.Right
                            Bottom = current.Bottom ^+ (new_y - old_y) |> round
                        }
            ),
            save_pos,
            Position = Position.BorderCornersB(10.0f).SliceL(10.0f)
        )
        |+ SubPositioner(
            (fun (preserve_center, (old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left ^- (new_x - old_x) |> round
                            Top = current.Top ^+ (new_y - old_y) |> round
                            Right = current.Right ^+ (new_x - old_x) |> round
                            Bottom = current.Bottom ^- (new_y - old_y) |> round
                        }
                else
                    this.Position <-
                        {
                            Left = current.Left
                            Top = current.Top ^+ (new_y - old_y) |> round
                            Right = current.Right ^+ (new_x - old_x) |> round
                            Bottom = current.Bottom
                        }
            ),
            save_pos,
            Position = Position.BorderCornersT(10.0f).SliceR(10.0f)
        )
        |+ SubPositioner(
            (fun (preserve_center, (old_x, old_y), (new_x, new_y)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left ^+ (new_x - old_x) |> round
                            Top = current.Top ^+ (new_y - old_y) |> round
                            Right = current.Right ^- (new_x - old_x) |> round
                            Bottom = current.Bottom ^- (new_y - old_y) |> round
                        }
                else
                    this.Position <-
                        {
                            Left = current.Left ^+ (new_x - old_x) |> round
                            Top = current.Top ^+ (new_y - old_y) |> round
                            Right = current.Right
                            Bottom = current.Bottom
                        }
            ),
            save_pos,
            Position = Position.BorderCornersT(10.0f).SliceL(10.0f)
        )

        |+ SubPositioner(
            (fun (preserve_center, (old_x, _), (new_x, _)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left ^+ (new_x - old_x) |> round
                            Top = current.Top
                            Right = current.Right ^- (new_x - old_x) |> round
                            Bottom = current.Bottom
                        }
                else
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
                { Position.BorderL(10.0f) with
                    Top = 0.5f %- 5.0f
                    Bottom = 0.5f %+ 5.0f
                }
        )
        |+ SubPositioner(
            (fun (preserve_center, (_, old_y), (_, new_y)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left
                            Top = current.Top ^+ (new_y - old_y) |> round
                            Right = current.Right
                            Bottom = current.Bottom ^- (new_y - old_y) |> round
                        }
                else
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
                { Position.BorderT(10.0f) with
                    Left = 0.5f %- 5.0f
                    Right = 0.5f %+ 5.0f
                }
        )
        |+ SubPositioner(
            (fun (preserve_center, (old_x, _), (new_x, _)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left ^- (new_x - old_x) |> round
                            Top = current.Top
                            Right = current.Right ^+ (new_x - old_x) |> round
                            Bottom = current.Bottom
                        }
                else
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
                { Position.BorderR(10.0f) with
                    Top = 0.5f %- 5.0f
                    Bottom = 0.5f %+ 5.0f
                }
        )
        |* SubPositioner(
            (fun (preserve_center, (_, old_y), (_, new_y)) ->
                let current = position.Value

                if preserve_center then
                    this.Position <-
                        {
                            Left = current.Left
                            Top = current.Top ^- (new_y - old_y) |> round
                            Right = current.Right
                            Bottom = current.Bottom ^+ (new_y - old_y) |> round
                        }
                else
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
                { Position.BorderB(10.0f) with
                    Left = 0.5f %- 5.0f
                    Right = 0.5f %+ 5.0f
                }
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =

        let mutable moved = moved

        if this.Focused then
            if SMALL_UP.TappedOrRepeated() then this.Move(0.0f, -1.0f)
            elif SMALL_DOWN.TappedOrRepeated() then this.Move(0.0f, 1.0f)
            elif SMALL_LEFT.TappedOrRepeated() then this.Move(-1.0f, 0.0f)
            elif SMALL_RIGHT.TappedOrRepeated() then this.Move(1.0f, 0.0f)

            if (%%"up").TappedOrRepeated() then this.Move(0.0f, -5.0f)
            if (%%"down").TappedOrRepeated() then this.Move(0.0f, 5.0f)
            if (%%"left").TappedOrRepeated() then this.Move(-5.0f, 0.0f)
            if (%%"right").TappedOrRepeated() then this.Move(5.0f, 0.0f)

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
            elif hover && Mouse.right_click () && HudElement.can_configure elem then
                show_menu elem (fun () -> ctx.Create elem)

        base.Update(elapsed_ms, moved)

    override this.OnSelected by_mouse =
        base.OnSelected by_mouse
        ctx.Selected <- elem

    override this.Draw() =

        if dragging_from.IsSome then
            let pos = position.Value
            let left_axis = this.Parent.Bounds.Left + this.Parent.Bounds.Width * snd pos.Left
            let right_axis = this.Parent.Bounds.Left + this.Parent.Bounds.Width * snd pos.Right

            Render.rect
                (Rect.Create(left_axis - 2.5f, this.Parent.Bounds.Top, right_axis + 2.5f, this.Parent.Bounds.Bottom))
                Colors.red_accent.O1

            Render.rect
                (Rect.Create(right_axis - 2.5f, this.Parent.Bounds.Top, right_axis + 2.5f, this.Parent.Bounds.Bottom))
                Colors.red_accent.O1

            let this_center_x, this_center_y = this.Bounds.Center

            for other_positioner in ctx.Positioners.Values do
                if other_positioner = this then
                    ()
                else

                let other_center_x, other_center_y = other_positioner.Bounds.Center

                if abs (this_center_x - other_center_x) < 5.0f then
                    Render.rect
                        (Rect.Create(
                            other_center_x - 2.5f,
                            min this.Bounds.Top other_positioner.Bounds.Top,
                            other_center_x + 2.5f,
                            max this.Bounds.Bottom other_positioner.Bounds.Bottom
                        ))
                        Colors.green_accent.O1

                if abs (this_center_y - other_center_y) < 5.0f then
                    Render.rect
                        (Rect.Create(
                            min this.Bounds.Left other_positioner.Bounds.Left,
                            other_center_y - 2.5f,
                            max this.Bounds.Right other_positioner.Bounds.Right,
                            other_center_y + 2.5f
                        ))
                        Colors.green_accent.O1

        base.Draw()

        if this.Focused then
            Render.rect (this.Bounds.BorderCornersT Style.PADDING) Colors.yellow_accent
            Render.rect (this.Bounds.BorderCornersB Style.PADDING) Colors.yellow_accent
            Render.rect (this.Bounds.BorderL Style.PADDING) Colors.yellow_accent
            Render.rect (this.Bounds.BorderR Style.PADDING) Colors.yellow_accent
        elif hover then
            Render.rect (this.Bounds.BorderCornersT Style.PADDING) Colors.white.O2
            Render.rect (this.Bounds.BorderCornersB Style.PADDING) Colors.white.O2
            Render.rect (this.Bounds.BorderL Style.PADDING) Colors.white.O2
            Render.rect (this.Bounds.BorderR Style.PADDING) Colors.white.O2

and PositionerContext =
    {
        Screen: Container
        Playfield: Playfield
        State: PlayState
        mutable Selected: HudElement
        mutable Positioners: Map<HudElement, Positioner>
    }
    member this.Create(e: HudElement) =
        match this.Positioners.TryFind e with
        | Some existing -> (this.Playfield.Remove existing || this.Screen.Remove existing) |> ignore
        | None -> ()

        Selection.clear ()
        let enabled = HudElement.enabled_setting e

        if enabled.Value then

            let setting = HudElement.position_setting e

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

            if this.Selected = e then
                if p.Initialised then p.Focus true else GameThread.defer (fun () -> p.Focus true)

    member this.Select(e: HudElement) =
        if this.Selected <> e then
            match this.Positioners.TryFind this.Selected with
            | Some _ -> Selection.clear()
            | None -> ()
            this.Selected <- e
            match this.Positioners.TryFind e with
            | Some existing -> existing.Focus true
            | None -> ()

    member this.ChangePositionRelative(to_playfield: bool, anchor: float32) =
        match this.Positioners.TryFind this.Selected with
        | Some p ->
            let setting = HudElement.position_setting this.Selected
            let current = setting.Value

            let bounds = p.Bounds
            let parent_bounds = if to_playfield then this.Playfield.Bounds else this.Screen.Bounds
            let axis = parent_bounds.Left + parent_bounds.Width * anchor
            setting.Set
                {
                    RelativeToPlayfield = to_playfield
                    Left = anchor %+ (bounds.Left - axis)
                    Top = current.Top
                    Right = anchor %+ (bounds.Right - axis)
                    Bottom = current.Bottom
                }
            this.Create this.Selected
        | None -> ()