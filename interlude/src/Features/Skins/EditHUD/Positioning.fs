namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI
open Interlude.Features.Play
open Interlude.Features.Skins

type SubPositioner(drag: bool * (float32 * float32) * (float32 * float32) -> unit, finish_drag: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false

    let SHIFT = Bind.mk Keys.LeftShift

    override this.Update(elapsed_ms, moved) =

        hover <- (this.Parent :?> Positioner).IsSelectedElement && Mouse.hover this.Bounds

        match dragging_from with
        | Some(x, y) ->
            let new_x, new_y = Mouse.pos ()
            drag (Input.held SHIFT, (x, y), (new_x, new_y))

            if not (Mouse.held Mouse.LEFT) then
                dragging_from <- None
                finish_drag ()
                this.Focus true
        | None ->
            if hover && Mouse.left_clicked () then
                dragging_from <- Some(Mouse.pos ())
                this.Select true

        base.Update(elapsed_ms, moved)

    override this.Draw() =
        if (this.Parent :?> Positioner).IsSelectedElement then
            if hover then
                Render.rect this.Bounds Colors.white.O4
            else
                Render.rect this.Bounds Colors.white.O2

and Positioner(element: HudElement, ctx: PositionerContext) =
    inherit Container(NodeType.None)

    let mutable increment = 5.0f
    let ALT = Bind.mk Keys.LeftAlt

    let round (offset: float32, anchor: float32) =
        System.MathF.Round(offset / increment) * increment, anchor

    let x_axis_align (position: Position) =
        let center = (fst position.Left + fst position.Right) * 0.5f
        let adjustment = System.MathF.Round(center / increment) * increment - center
        { position with
            Left = position.Left ^+ adjustment
            Right = position.Right ^+ adjustment
        }

    let mutable moving_with_keyboard = false
    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false

    let child =
        HudElement.constructor element (Content.HUD, ctx.State)

    let position = HudElement.position_setting element

    let mutable new_unsaved_pos: Position = Position.DEFAULT

    let validate_pos (parent_bounds: Rect) (pos: Position) =
        let bounds = Position.calculate pos parent_bounds

        if bounds.Left + 5.0f > bounds.Right || bounds.Top + 5.0f > bounds.Bottom then
            new_unsaved_pos
        else
            pos

    let save_pos () =
        ctx.AddUndoHistory(element, position.Value)
        position.Set
            { position.Value with
                Left = new_unsaved_pos.Left
                Top = new_unsaved_pos.Top
                Right = new_unsaved_pos.Right
                Bottom = new_unsaved_pos.Bottom
            }

    member this.IsSelectedElement = ctx.Selected = Some element

    override this.Position
        with set value =

            let value =
                if this.Initialised then
                    ctx.OnElementMoved.Trigger()
                    validate_pos this.Parent.Bounds value
                else
                    value

            base.set_Position value
            new_unsaved_pos <- value

    member this.Translate(x, y) =
        this.Position <-
            {
                Left = new_unsaved_pos.Left ^+ x
                Top = new_unsaved_pos.Top ^+ y
                Right = new_unsaved_pos.Right ^+ x
                Bottom = new_unsaved_pos.Bottom ^+ y
            }
            |> x_axis_align

    member this.Resize(x, y) =
        this.Position <-
            {
                Left = new_unsaved_pos.Left
                Top = new_unsaved_pos.Top
                Right = new_unsaved_pos.Right ^+ x
                Bottom = new_unsaved_pos.Bottom ^+ y
            }

    member this.Expand(x, y) =
        this.Position <-
            {
                Left = new_unsaved_pos.Left ^- x
                Top = new_unsaved_pos.Top ^- y
                Right = new_unsaved_pos.Right ^+ x
                Bottom = new_unsaved_pos.Bottom ^+ y
            }

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
            save_pos
        )
            .Position(Position.BorderCornersB(15.0f).SliceR(15.0f).Translate(5.0f, 5.0f))
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
            save_pos
        )
            .Position(Position.BorderCornersB(15.0f).SliceL(15.0f).Translate(-5.0f, 5.0f))
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
            save_pos
        )
            .Position(Position.BorderCornersT(15.0f).SliceR(15.0f).Translate(5.0f, -5.0f))
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
            save_pos
        )
            .Position(Position.BorderCornersT(15.0f).SliceL(15.0f).Translate(-5.0f, -5.0f))

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
            save_pos
        )
            .Position(Position.BorderL(15.0f).SliceY(15.0f).TranslateX(-5.0f))
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
            save_pos
        )
            .Position(Position.BorderT(15.0f).SliceX(15.0f).TranslateY(-5.0f))
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
            save_pos
        )
            .Position(Position.BorderR(15.0f).SliceY(15.0f).TranslateX(5.0f))
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
            save_pos
        )
            .Position(Position.BorderB(15.0f).SliceX(15.0f).TranslateY(5.0f))

        base.Init parent

    member this.KeyboardMovement(dx: float32, dy: float32, (ctrl: bool, alt: bool, shift: bool)) =
        let increment = if alt then 1.0f else 5.0f
        let direction = dx * increment, dy * increment
        if ctrl && shift then
            this.Expand direction
        elif ctrl then
            this.Resize direction
        else
            this.Translate direction
        moving_with_keyboard <- true

    override this.Update(elapsed_ms, moved) =

        let mutable moved = moved

        if this.IsSelectedElement then
            match Input.pop_key_any_modifiers(Keys.Up, InputAction.Press) with
            | ValueSome modifiers -> this.KeyboardMovement(0.0f, -1.0f, modifiers)
            | _ -> ()

            match Input.pop_key_any_modifiers(Keys.Down, InputAction.Press) with
            | ValueSome modifiers -> this.KeyboardMovement(0.0f, 1.0f, modifiers)
            | _ -> ()

            match Input.pop_key_any_modifiers(Keys.Left, InputAction.Press) with
            | ValueSome modifiers -> this.KeyboardMovement(-1.0f, 0.0f, modifiers)
            | _ -> ()

            match Input.pop_key_any_modifiers(Keys.Right, InputAction.Press) with
            | ValueSome modifiers -> this.KeyboardMovement(1.0f, 0.0f, modifiers)
            | _ -> ()

            let hold_up = Input.key_held_any_modifiers Keys.Up
            let hold_down = Input.key_held_any_modifiers Keys.Down
            let hold_left = Input.key_held_any_modifiers Keys.Left
            let hold_right = Input.key_held_any_modifiers Keys.Right

            let any_repeat =
                Input.pop_key_any_modifiers(Keys.Up, InputAction.Repeat)
                |> ValueOption.orElseWith (fun () -> Input.pop_key_any_modifiers(Keys.Down, InputAction.Repeat))
                |> ValueOption.orElseWith (fun () -> Input.pop_key_any_modifiers(Keys.Left, InputAction.Repeat))
                |> ValueOption.orElseWith (fun () -> Input.pop_key_any_modifiers(Keys.Right, InputAction.Repeat))
            match any_repeat with
            | ValueSome modifiers ->
                if hold_up then this.KeyboardMovement(0.0f, -1.0f, modifiers)
                if hold_down then this.KeyboardMovement(0.0f, 1.0f, modifiers)
                if hold_left then this.KeyboardMovement(-1.0f, 0.0f, modifiers)
                if hold_right then this.KeyboardMovement(1.0f, 0.0f, modifiers)
            | _ -> ()

            if
                moving_with_keyboard
                && not (hold_up || hold_down || hold_left || hold_right)
            then
                moving_with_keyboard <- false
                save_pos()

        elif moving_with_keyboard then
            moving_with_keyboard <- false
            save_pos()

        hover <- Mouse.hover this.Bounds

        increment <- if ALT.Held() then 1.0f else 5.0f

        match dragging_from with
        | Some(x, y) ->
            let current = position.Value
            let new_x, new_y = Mouse.pos ()
            moved <- true

            this.Position <-
                {
                    Left = current.Left ^+ (new_x - x)
                    Top = current.Top ^+ (new_y - y) |> round
                    Right = current.Right ^+ (new_x - x)
                    Bottom = current.Bottom ^+ (new_y - y) |> round
                }
                |> x_axis_align

            if not (Mouse.held Mouse.LEFT) then
                dragging_from <- None
                save_pos ()
        | None ->
            if hover && Mouse.left_clicked () then
                if this.IsSelectedElement then
                    dragging_from <- Some(Mouse.pos ())
                else
                    ctx.Select element
            elif hover && Mouse.right_clicked () && HudElement.can_configure element then
                show_menu element (fun () -> ctx.Recreate element)

        base.Update(elapsed_ms, moved)

    override this.Draw() =

        if dragging_from.IsSome then
            let pos = position.Value
            let left_axis = this.Parent.Bounds.Left + this.Parent.Bounds.Width * snd pos.Left
            let right_axis = this.Parent.Bounds.Left + this.Parent.Bounds.Width * snd pos.Right

            Render.rect_edges
                (left_axis - 2.5f)
                this.Parent.Bounds.Top
                (right_axis + 2.5f)
                this.Parent.Bounds.Bottom
                Colors.red_accent.O1

            Render.rect_edges
                (right_axis - 2.5f)
                this.Parent.Bounds.Top
                (right_axis + 2.5f)
                this.Parent.Bounds.Bottom
                Colors.red_accent.O1

            let this_center_x, this_center_y = this.Bounds.Center

            for other_positioner in ctx.Positioners.Values do
                if other_positioner = this then
                    ()
                else

                let other_center_x, other_center_y = other_positioner.Bounds.Center

                if abs (this_center_x - other_center_x) < 5.0f then
                    Render.rect_edges
                        (other_center_x - 2.5f)
                        (min this.Bounds.Top other_positioner.Bounds.Top)
                        (other_center_x + 2.5f)
                        (max this.Bounds.Bottom other_positioner.Bounds.Bottom)
                        Colors.green_accent.O1

                if abs (this_center_y - other_center_y) < 5.0f then
                    Render.rect_edges
                        (min this.Bounds.Left other_positioner.Bounds.Left)
                        (other_center_y - 2.5f)
                        (max this.Bounds.Right other_positioner.Bounds.Right)
                        (other_center_y + 2.5f)
                        Colors.green_accent.O1

        base.Draw()

        if this.IsSelectedElement then
            Render.border Style.PADDING this.Bounds Colors.yellow_accent
        elif hover then
            Render.border Style.PADDING this.Bounds Colors.white.O2

and PositionerContext =
    {
        Screen: Container
        Playfield: Playfield
        State: PlayState
        mutable Selected: HudElement option
        mutable Positioners: Map<HudElement, Positioner>
        mutable UndoHistory: List<HudElement * HudPosition>
        OnElementMoved: Event<unit>
    }
    member this.Recreate(element: HudElement) =
        match this.Positioners.TryFind element with
        | Some existing -> (this.Playfield.Remove existing || this.Screen.Remove existing) |> ignore
        | None -> ()

        let enabled = HudElement.enabled_setting element

        if enabled.Value then

            let setting = HudElement.position_setting element

            let p = Positioner(element, this)
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

            this.Positioners <- this.Positioners.Add(element, p)

    member this.CreateAll() =
        for element in HudElement.FULL_LIST do
            this.Recreate element

    member this.Select(element: HudElement) = this.Selected <- Some element
    member this.ClearSelection() = this.Selected <- None

    member this.ChangeCurrentAnchor(to_playfield: bool, anchor: float32) =
        match this.Selected |> Option.bind this.Positioners.TryFind with
        | Some p ->
            let setting = HudElement.position_setting this.Selected.Value
            let current = setting.Value

            this.AddUndoHistory(this.Selected.Value, current)

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
            this.Recreate this.Selected.Value
        | None -> ()

    member this.ResetCurrentPosition() =
        match this.Selected with
        | Some element ->
            let setting = HudElement.position_setting element
            this.AddUndoHistory(element, setting.Value)
            setting.Set(HudElement.default_position element)
            GameThread.defer (fun () -> this.Recreate element)
        | None -> ()

    member this.AddUndoHistory(element: HudElement, position: HudPosition) =
        this.UndoHistory <- (element, position) :: this.UndoHistory

    member this.Undo() =
        match this.UndoHistory with
        | (element, previous_position) :: xs ->
            this.Selected <- Some element
            HudElement.position_setting(element).Set previous_position
            this.Recreate element
            Style.click.Play()
            this.UndoHistory <- xs
        | _ -> ()

    member this.RemoveElement() =
        match this.Selected with
        | Some element ->
            let enabled = HudElement.enabled_setting element
            enabled.Set false
            this.Recreate element
            this.Selected <- None
            Style.click.Play()
        | None -> ()

    member this.HorizontalFlip() =
        match this.Selected with
        | Some element ->
            (HudElement.position_setting element) |> Setting.app _.FlipHorizontal
            this.Recreate element
        | None -> ()

    member this.VerticalFlip() =
        match this.Selected with
        | Some element ->
            (HudElement.position_setting element) |> Setting.app _.FlipVertical
            this.Recreate element
        | None -> ()

    member this.HorizontalFlipAll() =
        for element in HudElement.FULL_LIST do
            (HudElement.position_setting element) |> Setting.app _.FlipHorizontal
            this.Recreate element
        Notifications.action_feedback(Icons.REPEAT, "All elements flipped horizontally", sprintf "Press %O to flip it back" %%"hud_flip_horizontal_all")

    member this.VerticalFlipAll() =
        for element in HudElement.FULL_LIST do
            (HudElement.position_setting element) |> Setting.app _.FlipVertical
            this.Recreate element
        Notifications.action_feedback(Icons.REPEAT, "All elements flipped vertically", sprintf "Press %O to flip it back" %%"hud_flip_vertical_all")