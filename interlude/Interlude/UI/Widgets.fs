namespace Interlude.UI

open System
open System.Drawing
open System.Collections.Generic
open OpenTK.Mathematics
open Prelude.Common
open Interlude.Render
open Interlude
open Interlude.UI.Animation

(*
    Anchorpoints calculate the position of a widget's edge relative to its parent
    They are parameterised by an anchor and an offset

     To calculate the position of an edge (for example the value v for the left edge when given the left and right edges of the parent)
        Anchor is a value from 0-1 representing the percentage of the way across the parent widget to place the edge
        Offset is a value added to that to translate the edge by a fixed number of pixels

        Examples: AnchorPoint(50.0f, 0.0f) places the widget's edge 50 pixels left/down from the parent's left/top edge
                  AnchorPoint(-70.0f, 0.5f) places the widget's edge 70 pixels right/up from the parent's centre
    This can be used to flexibly describe the layout of a UI in terms of parent-child relations and these anchor points
*)

type AnchorPoint(offset, anchor) =
    inherit AnimationFade(offset)
    let mutable anchor_ = anchor
    //calculates the position given lower and upper bounds from the parent
    member this.Position(min, max) = min + base.Value + (max - min) * anchor_
    //snaps to a brand new position as if we constructed a new point
    member this.Reposition(offset, anchor) = this.Value <- offset; this.Target <- offset; anchor_ <- anchor

    member this.MoveRelative(min, max, value) = this.Target <- value - min - (max - min) * anchor_
    member this.RepositionRelative(min, max, value) = this.MoveRelative(min, max, value); this.Value <- this.Target

type WidgetState = Normal = 1 | Active = 2 | Disabled = 3 | Uninitialised = 4

(*
    Widgets are the atomic components of the UI system.
      All widgets can contain other "child" widgets embedded in them that inherit from their position
      What widgets do with their child widgets can be up to implementation, by default all are drawn and updated with the parent.
*)

type Widget() =

    let left = AnchorPoint(0.f, 0.f)
    let top = AnchorPoint(0.f, 0.f)
    let right = AnchorPoint(0.f, 1.f)
    let bottom = AnchorPoint(0.f, 1.f)

    let animation = AnimationGroup()
    do
        animation.Add(left)
        animation.Add(right)
        animation.Add(top)
        animation.Add(bottom)
    let mutable parent = None
    let mutable bounds = Rect.zero
    let mutable initialised = false
    let mutable enable = true
    let children = new List<Widget>()

    member this.Animation = animation
    member this.Bounds = bounds
    member this.Anchors = (left, top, right, bottom)
    member this.Children = children
    member this.Parent = parent.Value
    member this.Enabled with get() = enable and set(value) = enable <- value
    member this.Initialised = initialised

    abstract member Add: Widget -> unit
    default this.Add(c) =
        children.Add(c)
        c.OnAddedTo(this)

    abstract member OnAddedTo: Widget -> unit
    default this.OnAddedTo(c) =
        match parent with
        | None -> parent <- Some c
        | Some _ -> Logging.Error("Tried to add this widget to a container when it is already in one") ""
        
    abstract member Remove: Widget -> unit
    default this.Remove(c) =
        if children.Remove(c) then
            c.OnRemovedFrom(this)
        else Logging.Error("Tried to remove widget that was not in this container") ""

    member private this.OnRemovedFrom(c) =
        match parent with
        | None -> Logging.Error("Tried to remove this widget from a container it isn't in one") ""
        | Some p -> if p = c then parent <- None else Logging.Error("Tried to remove this widget from a container when it is in another") ""

    member this.Synchronized(action) =
        animation.Add(new AnimationAction(action))

    // Draw is called at the framerate of the game (normally unlimited) and should be where the widget performs render calls to draw it on screen
    abstract member Draw: unit -> unit
    default this.Draw() = for c in children do if c.Initialised && c.Enabled then c.Draw()

    // Update is called at a fixed framerate (120Hz) and should be where the widget handles input and other time-based logic
    abstract member Update: float * Rect -> unit
    default this.Update(elapsedTime, struct (l, t, r, b): Rect) =
        animation.Update(elapsedTime) |> ignore
        initialised <- true
        bounds <- Rect.create <| left.Position(l, r) <| top.Position(t, b) <| right.Position(l, r) <| bottom.Position(t, b)
        for i in children.Count - 1 .. -1 .. 0 do
            if (children.[i].Enabled) then children.[i].Update(elapsedTime, bounds)

    //todo: tear these out and replace with nice idiomatic positioners
    member this.Reposition(l, la, t, ta, r, ra, b, ba) =
        left.Reposition(l, la)
        top.Reposition(t, ta)
        right.Reposition(r, ra)
        bottom.Reposition(b, ba)
    
    member this.Reposition(l, t, r, b) = this.Reposition(l, 0.f, t, 0.f, r, 1.f, b, 1.f)

    member this.Move(l, t, r, b) =
        left.Target <- l
        top.Target <- t
        right.Target <- r
        bottom.Target <- b

    // Dispose is called when a widget is going out of scope/about to be garbage collected and allows it to release any resources
    abstract member Dispose: unit -> unit
    default this.Dispose() = for c in children do c.Dispose()

type Logo() as this =
    inherit Widget()

    let counter = AnimationCounter(10000000.0)
    do this.Animation.Add(counter)

    override this.Draw() =
        base.Draw()
        let w = Rect.width this.Bounds
        let struct (l, t, r, b) = this.Bounds

        Draw.quad
        <| Quad.create
            (new Vector2(l + 0.08f * w, t + 0.09f * w))
            (new Vector2(l + 0.5f * w, t + 0.76875f * w))
            (new Vector2(l + 0.5f * w, t + 0.76875f * w))
            (new Vector2(r - 0.08f * w, t + 0.09f * w))
        <| Quad.colorOf(Color.DarkBlue)
        <| (Sprite.DefaultQuad)
        Draw.quad
        <| Quad.create
            (new Vector2(l + 0.08f * w, t + 0.29f * w))
            (new Vector2(l + 0.22f * w, t + 0.29f * w))
            (new Vector2(l + 0.5f * w, t + 0.76875f * w))
            (new Vector2(l + 0.5f * w, t + 0.96875f * w))
        <| Quad.colorOf(Color.DarkBlue)
        <| (Sprite.DefaultQuad)
        Draw.quad
        <| Quad.create
            (new Vector2(r - 0.08f * w, t + 0.29f * w))
            (new Vector2(r - 0.22f * w, t + 0.29f * w))
            (new Vector2(l + 0.5f * w, t + 0.76875f * w))
            (new Vector2(l + 0.5f * w, t + 0.96875f * w))
        <| Quad.colorOf(Color.DarkBlue)
        <| (Sprite.DefaultQuad)

        Stencil.create(true)
        Draw.quad
        <| Quad.create
            (new Vector2(l + 0.1f * w, t + 0.1f * w))
            (new Vector2(l + 0.5f * w, t + 0.75f * w))
            (new Vector2(l + 0.5f * w, t + 0.75f * w))
            (new Vector2(r - 0.1f * w, t + 0.1f * w))
        <| Quad.colorOf(Color.Aqua)
        <| (Sprite.DefaultQuad)
        Draw.quad
        <| Quad.create
            (new Vector2(l + 0.1f * w, t + 0.3f * w))
            (new Vector2(l + 0.2f * w, t + 0.3f * w))
            (new Vector2(l + 0.5f * w, t + 0.7875f * w))
            (new Vector2(l + 0.5f * w, t + 0.95f * w))
        <| Quad.colorOf(Color.Aqua)
        <| (Sprite.DefaultQuad)
        Draw.quad
        <| Quad.create
            (new Vector2(r - 0.1f * w, t + 0.3f * w))
            (new Vector2(r - 0.2f * w, t + 0.3f * w))
            (new Vector2(l + 0.5f * w, t + 0.7875f * w))
            (new Vector2(l + 0.5f * w, t + 0.95f * w))
        <| Quad.colorOf(Color.Aqua)
        <| (Sprite.DefaultQuad)
        Draw.rect this.Bounds Color.White <| Themes.getTexture("logo")

        Stencil.draw()
        //chart background
        Draw.rect this.Bounds <| Color.Aqua <| Sprite.Default
        let rain = Themes.getTexture("rain")
        let v = float32 counter.Time
        let q = Quad.ofRect this.Bounds
        Draw.quad <| q <| Quad.colorOf (Color.FromArgb(80, 0, 0, 255)) <| rain.WithUV(Sprite.tilingUV(0.625f, v * 0.06f, v * 0.07f)rain q)
        Draw.quad <| q <| Quad.colorOf (Color.FromArgb(150, 0, 0, 255)) <| rain.WithUV(Sprite.tilingUV(1.0f, v * 0.1f, v * 0.11f)rain q)
        Draw.quad <| q <| Quad.colorOf (Color.FromArgb(220, 0, 0, 255)) <| rain.WithUV(Sprite.tilingUV(1.5625f, v * 0.15f, v * 0.16f)rain q)

        let mutable prev = 0.0f
        let m = b - w * 0.5f
        for i in 0 .. 31 do
            let level =
                (seq { (i * 8)..(i * 8 + 7) }
                |> Seq.map (fun x -> Audio.waveForm.[x])
                |> Seq.sum) * 0.1f
            let i = float32 i
            Draw.quad
            <| Quad.create
                (new Vector2(l + i * w / 32.0f, m - prev))
                (new Vector2(l + (i + 1.0f) * w / 32.0f, m - level))
                (new Vector2(l + (i + 1.0f) * w / 32.0f, b))
                (new Vector2(l + i * w / 32.0f, b))
            <| Quad.colorOf(Color.FromArgb(127, 0, 0, 255))
            <| (Sprite.DefaultQuad)
            prev <- level

        Stencil.finish()
        Draw.rect this.Bounds Color.White <| Themes.getTexture("logo")

type Screen() =
    inherit Widget()
    abstract member OnEnter: Screen -> unit
    default this.OnEnter(prev: Screen) = ()
    abstract member OnExit: Screen -> unit
    default this.OnExit(next: Screen) = ()

[<AbstractClass>]
type Dialog() as this =
    inherit Widget()

    let fade = new Animation.AnimationFade(0.0f)

    do
        this.Animation.Add(fade)
        fade.Target <- 1.0f

    // Begins closing animation
    member this.Close() =
        fade.Target <- 0.0f

    // Called when dialog actually closes (end of animation)
    abstract member OnClose: unit -> unit

    override this.Draw() =
        Draw.rect(this.Bounds)(Color.FromArgb(int (180.0f * fade.Value), 0, 0, 0))(Sprite.Default)
        base.Draw()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if (fade.Value < 0.02f && fade.Target = 0.0f) then
            this.Enabled <- false
            this.OnClose()

type ScreenTransitionFlag =
| Default = 0
| UnderLogo = 1
//more transition animations go here

type NotificationType =
| Info = 0
| System = 1
| Task = 2
| Error = 3

type ScreenType =
| SplashScreen = 0
| MainMenu = 1
| Import = 2
| LevelSelect = 3
| Play = 4
| Score = 5

(*
    Collection of mutable values to "tie the knot" in mutual dependence
       - Stuff is defined but not inialised here
       - Stuff is then referenced by screen logic
       - Overall screen manager references screen logic AND initialises values, connecting the loop
*)

module Screens =
    // All of these are initialised in ScreensMain.fs
    let mutable internal changeScreen: ScreenType * ScreenTransitionFlag -> unit = ignore
    let mutable internal newScreen: (unit -> Screen) * ScreenType * ScreenTransitionFlag -> unit = ignore
    let mutable internal back: ScreenTransitionFlag -> unit = ignore
    let mutable internal addDialog: Dialog -> unit = ignore

    let mutable internal setToolbarCollapsed: bool -> unit = ignore
    let mutable internal setCursorVisible: bool -> unit = ignore

    let mutable internal addNotification: string * NotificationType -> unit = ignore

    //background fbo
    let parallaxX  = AnimationFade(0.0f)
    let parallaxY  = AnimationFade(0.0f)
    let parallaxZ  = AnimationFade(40.0f)
    let backgroundDim = AnimationFade(1.0f)
    let accentColor = AnimationColorMixer(Themes.accentColor)

    let logo = new Logo()
    
    let accentShade(alpha, brightness, white) =
        let accentColor = accentColor.GetColor()
        let rd = float32 (255uy - accentColor.R) * white
        let gd = float32 (255uy - accentColor.G) * white
        let bd = float32 (255uy - accentColor.B) * white
        Color.FromArgb(alpha,
            int ((float32 accentColor.R + rd) * brightness),
            int ((float32 accentColor.G + gd) * brightness),
            int ((float32 accentColor.B + bd) * brightness))

    let drawBackground(bounds, color, depth) =
        let bg = Themes.background
        let pwidth = Render.vwidth + parallaxZ.Value
        let pheight = Render.vheight + parallaxZ.Value
        let x = -parallaxX.Value * parallaxZ.Value
        let y = -parallaxY.Value * parallaxZ.Value
        let screenaspect = pwidth / pheight
        let bgaspect = float32 bg.Width / float32 bg.Height
        let q = Quad.ofRect bounds
        Draw.quad
            q
            (Quad.colorOf color)
            (bg.WithUV(
                Sprite.tilingUV(
                    if bgaspect > screenaspect then
                        let scale = pheight / float32 bg.Height
                        let left = (float32 bg.Width * scale - pwidth) * -0.5f
                        (scale, left + x, 0.0f + y)
                    else
                        let scale = pwidth / float32 bg.Width
                        let top = (float32 bg.Height * scale - pheight) * -0.5f
                        (scale, 0.0f + x, top + y)) bg q))