namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Gameplay
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

type SubPositioner(drag: bool * (float32 * float32) * (float32 * float32) -> unit, finish_drag: unit -> unit) =
    inherit StaticWidget(NodeType.None)

    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false

    let SHIFT = Bind.mk (OpenTK.Windowing.GraphicsLibraryFramework.Keys.LeftShift)

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
                Draw.rect this.Bounds Colors.white.O3
            else
                Draw.rect this.Bounds Colors.white.O1

type Positioner(elem: HudElement, ctx: PositionerContext) =
    inherit Container(NodeType.FocusTrap)

    let round (offset: float32, anchor: float32) =
        System.MathF.Round(offset / 5.0f) * 5.0f, anchor

    let mutable dragging_from: (float32 * float32) option = None
    let mutable hover = false
    let mutable repeat = -1
    let mutable time = 0.0
    let REPEAT_DELAY = 400.0
    let REPEAT_INTERVAL = 40.0

    let SMALL_UP = (%%"up").WithModifiers(false, false, true)
    let SMALL_DOWN = (%%"down").WithModifiers(false, false, true)
    let SMALL_LEFT = (%%"left").WithModifiers(false, false, true)
    let SMALL_RIGHT = (%%"right").WithModifiers(false, false, true)

    let child =
        HudElement.constructor elem (Content.HUD, ctx.State)

    let position = HudElement.position_setting elem

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
            Position = Position.BorderBottomCorners(10.0f).SliceRight(10.0f)
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
            Position = Position.BorderBottomCorners(10.0f).SliceLeft(10.0f)
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
            Position = Position.BorderTopCorners(10.0f).SliceRight(10.0f)
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
            Position = Position.BorderTopCorners(10.0f).SliceLeft(10.0f)
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
                { Position.BorderLeft(10.0f) with
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
                { Position.BorderTop(10.0f) with
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
                { Position.BorderRight(10.0f) with
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
                { Position.BorderBottom(10.0f) with
                    Left = 0.5f %- 5.0f
                    Right = 0.5f %+ 5.0f
                }
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =

        let mutable moved = moved

        if this.Focused then
            if SMALL_UP.Tapped() then this.Move(0.0f, -1.0f)
            elif SMALL_DOWN.Tapped() then this.Move(0.0f, 1.0f)
            elif SMALL_LEFT.Tapped() then this.Move(-1.0f, 0.0f)
            elif SMALL_RIGHT.Tapped() then this.Move(1.0f, 0.0f)

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

        base.Draw()

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
                if p.Initialised then p.Focus true else defer(fun () -> p.Focus true)

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

type PositionerInfo(ctx: PositionerContext) =
    inherit FrameContainer(NodeType.None, Fill = K Colors.shadow_2.O3, Border = K Colors.cyan_accent)

    let mutable bottom = true

    let TOP_POSITION : Position = { Left = 0.5f %- 400.0f; Right = 0.5f %+ 400.0f; Top = 0.0f %- 1.0f; Bottom = 0.0f %+ 60.0f }
    let BOTTOM_POSITION : Position = { Left = 0.5f %- 400.0f; Right = 0.5f %+ 400.0f; Top = 1.0f %- 60.0f; Bottom = 1.0f %+ 1.0f }

    let dropdown_wrapper = 
        DropdownWrapper(fun d ->
            match d with
            | :? DropdownMenu ->
                if bottom then 
                    { 
                        Left = 1.0f %- 370.0f
                        Top = 0.0f %- (10.0f + d.Height)
                        Right = 1.0f %- 30.0f
                        Bottom = 0.0f %- 10.0f
                    }
                else
                    {
                        Left = 1.0f %- 370.0f
                        Top = 1.0f %+ 60.0f
                        Right = 1.0f %- 30.0f
                        Bottom = 1.0f %+ (60.0f + d.Height)
                    }
            | _ ->
                if bottom then 
                    { 
                        Left = 0.0f %+ 30.0f
                        Top = 0.0f %- (10.0f + d.Height)
                        Right = 0.0f %+ 370.0f
                        Bottom = 0.0f %- 10.0f
                    }
                else
                    {
                        Left = 0.0f %+ 30.0f
                        Top = 1.0f %+ 10.0f
                        Right = 0.0f %+ 370.0f
                        Bottom = 1.0f %+ (10.0f + d.Height)
                    }
        )

    override this.Init(parent) =
        NavigationContainer.Row()
        |+ Button(
            (fun () -> HudElement.name ctx.Selected),
            this.ToggleElementDropdown,
            Hotkey = "context_menu",
            Position = Position.SliceLeft(400.0f).Margin(20.0f, 5.0f)
        )
        |+ Button(
            (fun () -> if (HudElement.enabled_setting ctx.Selected).Value then Icons.CHECK_CIRCLE else Icons.CIRCLE),
            (fun () ->
                Setting.app not (HudElement.enabled_setting ctx.Selected)
                defer (fun () -> ctx.Create ctx.Selected)
            ),
            Disabled = (fun () -> HudElement.can_toggle ctx.Selected |> not),
            Position = Position.Column(400.0f, 100.0f).Margin(10.0f, 5.0f)
        )
        |+ Button(
            Icons.REFRESH_CW,
            (fun () ->
                HudElement.position_setting(ctx.Selected).Set(HudElement.default_position ctx.Selected)
                defer (fun () -> ctx.Create ctx.Selected)
            ),
            Position = Position.Column(500.0f, 100.0f).Margin(10.0f, 5.0f)
        )
        |+ Button(
            Icons.SETTINGS,
            (fun () -> show_menu ctx.Selected (fun () -> ctx.Create ctx.Selected)),
            Hotkey = "options",
            Disabled = (fun () -> HudElement.can_configure ctx.Selected |> not),
            Position = Position.Column(600.0f, 100.0f).Margin(10.0f, 5.0f)
        )
        |+ Button(
            Icons.LAYOUT,
            this.ToggleAnchorDropdown,
            Position = Position.Column(700.0f, 100.0f).Margin(10.0f, 5.0f)
        )
        |> this.Add

        this |* dropdown_wrapper

        this.Position <- BOTTOM_POSITION
        base.Init parent

    member private this.ToggleElementDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            Dropdown
                {
                    Items = HudElement.FULL_LIST |> List.map (fun e -> e, HudElement.name e)
                    ColorFunc = K Colors.text
                    Setting =
                        Setting.make
                            (fun v -> ctx.Select v)
                            (fun () -> ctx.Selected)
                }
        )

    member private this.ToggleAnchorDropdown() =
        dropdown_wrapper.Toggle(fun () ->
            DropdownMenu
                {
                    Items = [
                        (fun () -> ctx.ChangePositionRelative(true, Alignment.CENTER)), %"hud.editor.relative_to.playfield_center"
                        (fun () -> ctx.ChangePositionRelative(true, Alignment.LEFT)), %"hud.editor.relative_to.playfield_left"
                        (fun () -> ctx.ChangePositionRelative(true, Alignment.RIGHT)), %"hud.editor.relative_to.playfield_right"
                        (fun () -> ctx.ChangePositionRelative(false, Alignment.CENTER)), %"hud.editor.relative_to.screen_center"
                        (fun () -> ctx.ChangePositionRelative(false, Alignment.LEFT)), %"hud.editor.relative_to.screen_left"
                        (fun () -> ctx.ChangePositionRelative(false, Alignment.RIGHT)), %"hud.editor.relative_to.screen_right"
                    ]
                }
            |+ Text(%"hud.editor.relative_to", Position = Position.BorderTop 40.0f)
        )

    override this.Update(elapsed_ms, moved) =
        let mutable moved = moved

        if ctx.Positioners.ContainsKey ctx.Selected then
            if
                bottom
                && (ctx.Positioners.[ctx.Selected].Bounds.Intersect this.Bounds).Visible
                && ctx.Positioners.[ctx.Selected].Bounds.Top > 60.0f
            then
                bottom <- false
                moved <- true
                this.Position <- TOP_POSITION
            elif
                not bottom
                && (ctx.Positioners.[ctx.Selected].Bounds.Intersect this.Bounds).Visible
                && ctx.Positioners.[ctx.Selected].Bounds.Bottom < this.Parent.Bounds.Bottom - 60.0f
            then
                bottom <- true
                moved <- true
                this.Position <- BOTTOM_POSITION

        base.Update(elapsed_ms, moved)

module EditHudScreen =

    let edit_hud_screen (chart: Chart, with_colors: ColoredChart, on_exit) =

        let replay_data: IReplayProvider =
            StoredReplayProvider.WavingAutoPlay(with_colors.Keys, with_colors.Source.Notes)

        let FIRST_NOTE = with_colors.FirstNote
        let ruleset = Rulesets.current

        let mutable replay_data = replay_data

        let mutable scoring =
            Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes SelectedChart.rate.Value

        let mutable time = -Time.infinity

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider.WavingAutoPlay(with_colors.Keys, with_colors.Source.Notes)
            scoring <- Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes SelectedChart.rate.Value
            screen.State.ChangeScoring scoring

        { new IPlayScreen(chart, with_colors, PacemakerState.None, scoring) with
            override this.AddWidgets() =

                let ctx =
                    {
                        Screen = Container(NodeType.None)
                        Playfield = this.Playfield
                        State = this.State
                        Selected = HudElement.Accuracy
                        Positioners = Map.empty
                    }

                HudElement.FULL_LIST
                |> Seq.iter ctx.Create

                this 
                |+ ctx.Screen 
                |* PositionerInfo ctx
            // todo: way to turn on multiplayer player list

            override this.OnEnter p =
                DiscordRPC.in_menus ("Customising HUD")
                Dialog.close ()
                Background.dim (float32 options.BackgroundDim.Value)
                Toolbar.hide ()
                Song.on_finish <- SongFinishAction.LoopFromBeginning
                Input.remove_listener ()

            override this.OnExit s =
                base.OnExit s
                if s <> Screen.Type.Play then on_exit ()

            override this.OnBack() =
                if Gameplay.continue_endless_mode () then None
                else base.OnBack()

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
