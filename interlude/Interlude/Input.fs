namespace Interlude.Input

open System
open OpenTK
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.Render

type Bind =
    | Dummy
    | Key of Keys * modifiers:(bool * bool * bool)
    | Mouse of MouseButton// * modifiers:(bool * bool * bool)
    | Joystick of unit //NYI
    override this.ToString() =
        match this with
        | Dummy -> "NONE"
        | Key (k, m) -> Bind.ModifierString(m) + k.ToString()
        | Mouse b -> "M" + b.ToString()
        | Joystick _ -> "nyi"
    static member private ModifierString((ctrl, alt, shift)) =
        (if ctrl then "Ctrl + " else "")
        + (if alt then "Alt + " else "")
        + (if shift then "Shift + " else "")
    static member DummyBind = new Setting<Bind>(Dummy)

module Bind =
    let inline mk k = Key (k, (false, false, false))
    let inline ctrl k = Key (k, (true, false, false))
    let inline alt k = Key (k, (false, true, false))
    let inline shift k = Key (k, (false, false, true))
    let inline ctrlShift k = Key (k, (true, false, true))
    let inline ctrlAlt k = Key (k, (true, true, false))

type InputEvType =
| Press = 0
| Release = 2

type InputEv = (struct (Bind * InputEvType * float32<ms>))

module Input =
    
    let mutable internal evts: InputEv list = []
    
    let mutable internal mousex = 0.f
    let mutable internal mousey = 0.f
    let mutable internal mousez = 0.f
    let mutable internal oldmousez = 0.f
    let mutable internal ctrl = false
    let mutable internal shift = false
    let mutable internal alt = false

    let mutable internal gw: GameWindow = null

    let mutable internal inputmethod = None
    let mutable internal absorbed = false
    
    let removeInputMethod() =
        match inputmethod with
        | Some (s, callback) -> callback()
        | None -> ()
        inputmethod <- None
    
    let setInputMethod(s: ISettable<string>, callback: unit -> unit) =
        removeInputMethod()
        inputmethod <- Some (s, callback)

    let absorbAll() =
        oldmousez <- mousez
        absorbed <- true
        let e = evts
        evts <- []

    let consumeOne(b: Bind, t: InputEvType) =
        let mutable out = ValueNone
        let rec f evs =
            match evs with
            | [] -> []
            | struct (B, T, time) :: xs when B = b && T = t -> out <- ValueSome time; xs
            | x :: xs -> x :: (f xs)
        evts <- f evts
        out

    let consumeGameplay(b: Bind, t: InputEvType, f) =
        let mutable time = consumeOne(b, t)
        while time.IsSome do
            f time.Value
            time <- consumeOne(b, t)

    let consumeAny(t: InputEvType) =
        let mutable out = ValueNone
        let rec f evs =
            match evs with
            | [] -> []
            | struct (b, T, time) :: xs when T = t -> out <- ValueSome b; xs
            | x :: xs -> x :: (f xs)
        evts <- f evts
        out

    let held(b: Bind) =
        if absorbed then false
        else
        match b with
        | Key (Keys.LeftControl, _) -> ctrl
        | Key (Keys.RightControl, _) -> ctrl
        | Key (Keys.LeftAlt, _) -> alt
        | Key (Keys.RightAlt, _) -> alt
        | Key (Keys.LeftShift, _) -> shift
        | Key (Keys.RightShift, _) -> shift
        | Key (k, m) -> gw.KeyboardState.[k] && m = (ctrl, alt, shift)
        | Mouse m -> gw.MouseState.[m]
        | Dummy -> false
        | Joystick _ -> false

    let poll() =
        let add x = evts <- List.append evts [x]
        let now = Audio.timeWithOffset()

        ctrl <- gw.KeyboardState.IsKeyDown Keys.LeftControl || gw.KeyboardState.IsKeyDown Keys.RightControl
        shift <- gw.KeyboardState.IsKeyDown Keys.LeftShift || gw.KeyboardState.IsKeyDown Keys.RightShift
        alt <- gw.KeyboardState.IsKeyDown Keys.LeftAlt || gw.KeyboardState.IsKeyDown Keys.RightAlt

        // keyboard input handler
        //todo: way of remembering modifier combo for hold/release?
        for k in 0 .. int Keys.LastKey do
            if k < 340 || k > 347 then
                if gw.KeyboardState.IsKeyDown(enum k) then
                    if gw.KeyboardState.WasKeyDown(enum k) |> not then
                        struct((enum k, (ctrl, alt, shift)) |> Key, InputEvType.Press, now) |> add
                elif gw.KeyboardState.WasKeyDown(enum k) then
                    struct((enum k, (false, false, false)) |> Key, InputEvType.Release, now) |> add

        // mouse input handler
        for b in 0 .. int MouseButton.Last do
            if gw.MouseState.IsButtonDown(enum b) then
                if gw.MouseState.WasButtonDown(enum b) |> not then
                    struct(enum b |> Mouse, InputEvType.Press, now) |> add
            elif gw.MouseState.WasButtonDown(enum b) then
                struct(enum b |> Mouse, InputEvType.Release, now) |> add

        // joystick stuff NYI
    
    let init(win : GameWindow) =
        gw <- win
        gw.add_MouseWheel(fun e -> mousez <- e.OffsetY)
        gw.add_MouseMove(
            fun e ->
                mousex <- Math.Clamp(Render.vwidth / float32 Render.rwidth * float32 e.X, 0.0f, Render.vwidth)
                mousey <- Math.Clamp(Render.vheight / float32 Render.rheight * float32 e.Y, 0.0f, Render.vheight)
                removeInputMethod())
        gw.add_TextInput(fun e ->
            match inputmethod with
            | Some (s, c) -> s.Set(s.Get() + e.AsString); absorbAll()
            | None -> ())

    let update() =
        let delete = Bind.mk Keys.Backspace
        let bigDelete = Bind.ctrl Keys.Backspace
        absorbed <- false
        match inputmethod with
        |  Some (s, c) ->
            if consumeOne(delete, InputEvType.Press).IsSome && s.Get().Length > 0 then
                let v = s.Get()
                s.Set(v.Substring(0, v.Length - 1))
            elif consumeOne(bigDelete, InputEvType.Press).IsSome then s.Set("")
            //todo: clipboard support
        | None -> ()

module Mouse = 
    let X() = Input.mousex
    let Y() = Input.mousey
    let Scroll() = let v = Input.mousez - Input.oldmousez in Input.oldmousez <- Input.mousez; v

    let Click(b) = Input.consumeOne(Mouse b, InputEvType.Press).IsSome
    let Held(b) = Input.held(Mouse b)
    let Released(b) = Input.consumeOne(Mouse b, InputEvType.Release).IsSome

    let Hover(struct (l, t, r, b): Interlude.Render.Rect) = let x, y = X(), Y() in x > l && x < r && y > t && y < b

type Bind with
    member this.Pressed() =
        match this with
        | Key _
        | Mouse _ -> Input.held this
        | _ -> false
    member this.Tapped() =
        match this with
        | Key _
        | Mouse _ -> Input.consumeOne(this, InputEvType.Press).IsSome
        | _ -> false
    member this.Released() =
        match this with
        | Key _
        | Mouse _ -> Input.consumeOne(this, InputEvType.Release).IsSome
        | _ -> false