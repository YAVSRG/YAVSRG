namespace Interlude.Input

open System
open OpenTK
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.Graphics

type Bind =
    | Dummy
    | Key of Keys * modifiers: (bool * bool * bool)
    | Mouse of MouseButton
    | Joystick of unit //NYI

    override this.ToString() =
        match this with
        | Dummy -> "NONE"
        | Key (k, m) -> Bind.ModifierString m + k.ToString()
        | Mouse b -> "M" + b.ToString()
        | Joystick _ -> "nyi"

    static member private ModifierString (ctrl, alt, shift) =
        (if ctrl then "Ctrl + " else "")
        + (if alt then "Alt + " else "")
        + (if shift then "Shift + " else "")

    static member DummyBind = Setting.simple Dummy


module Bind =
    let inline mk k = Key (k, (false, false, false))
    let inline ctrl k = Key (k, (true, false, false))
    let inline alt k = Key (k, (false, true, false))
    let inline shift k = Key (k, (false, false, true))
    let inline ctrlShift k = Key (k, (true, false, true))
    let inline ctrlAlt k = Key (k, (true, true, false))

type InputEvType =
| Press = 0
| Release = 1

type InputEv = (struct (Bind * InputEvType * float32<ms>))

[<RequireQualifiedAccess>]
type InputMethod =
    | Text of setting: Setting<string> * callback: (unit -> unit)
    | Bind of callback: (Bind -> unit)
    | None

module Input =
    
    let mutable internal evts: InputEv list = []
    
    let mutable internal mousex = 0.0f
    let mutable internal mousey = 0.0f
    let mutable internal mousez = 0.0f
    let mutable internal oldmousex = 0.0f
    let mutable internal oldmousey = 0.0f
    let mutable internal oldmousez = 0.0f
    let mutable internal ctrl = false
    let mutable internal shift = false
    let mutable internal alt = false

    let mutable internal gw : GameWindow = null

    let mutable internal inputmethod : InputMethod = InputMethod.None
    let mutable internal inputmethod_mousedist = 0f
    let mutable internal absorbed = false
    let mutable internal typed = false
    
    let removeInputMethod() =
        match inputmethod with
        | InputMethod.Text (s, callback) -> callback()
        | InputMethod.Bind _
        | InputMethod.None -> ()
        inputmethod <- InputMethod.None
    
    let setTextInput (s: Setting<string>, callback: unit -> unit) =
        removeInputMethod()
        inputmethod <- InputMethod.Text (s, callback)
        inputmethod_mousedist <- 0f

    let grabNextEvent (callback: Bind -> unit) =
        removeInputMethod()
        inputmethod <- InputMethod.Bind callback

    let absorbAll() =
        oldmousez <- mousez
        inputmethod_mousedist <- inputmethod_mousedist + abs(mousey - oldmousey) + abs (mousex - oldmousex)
        oldmousey <- mousey
        oldmousex <- mousex
        absorbed <- true
        evts <- []

    let consumeOne (b: Bind, t: InputEvType) =
        let mutable out = ValueNone
        let rec f evs =
            match evs with
            | [] -> []
            | struct (B, T, time) :: xs when B = b && T = t -> out <- ValueSome time; xs
            | x :: xs -> x :: (f xs)
        evts <- f evts
        out

    let consumeGameplay (binds: Bind array, callback: int -> Time -> bool -> unit) =
        let bmatch bind target =
            match bind, target with
            | Key (k, _), Key (K, _) when k = K -> true
            | Mouse b, Mouse B when b = B -> true
            | _ -> false
        let rec f evs =
            match evs with
            | [] -> []
            | struct (b, t, time) :: xs ->
                let mutable i = 0
                let mutable matched = false
                while i < binds.Length && not matched do
                    if bmatch binds.[i] b then callback i time (t <> InputEvType.Press); matched <- true
                    i <- i + 1
                if matched then f xs else struct (b, t, time) :: (f xs)
        evts <- f evts

    let consumeAny (t: InputEvType) =
        let mutable out = ValueNone
        let rec f evs =
            match evs with
            | [] -> []
            | struct (b, T, time) :: xs when T = t -> out <- ValueSome b; xs
            | x :: xs -> x :: (f xs)
        evts <- f evts
        out

    let held (b: Bind) =
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
        // todo: way of remembering modifier combo for hold/release?
        for k in 0 .. int Keys.LastKey do
            //if k < 340 || k > 347 then
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
    
    let init (win: GameWindow) =
        gw <- win
        gw.add_MouseWheel(fun e -> mousez <- mousez + e.OffsetY)
        gw.add_MouseMove(
            fun e ->
                mousex <- Math.Clamp(Render.vwidth / float32 Render.rwidth * float32 e.X, 0.0f, Render.vwidth)
                mousey <- Math.Clamp(Render.vheight / float32 Render.rheight * float32 e.Y, 0.0f, Render.vheight))
        gw.add_TextInput(fun e ->
            match inputmethod with
            | InputMethod.Text (s, c) -> Setting.app (fun x -> x + e.AsString) s; typed <- true
            | InputMethod.Bind _
            | InputMethod.None -> ())

    let update() =
        let delete = Bind.mk Keys.Backspace
        let bigDelete = Bind.ctrl Keys.Backspace
        absorbed <- false
        match inputmethod with
        | InputMethod.Text (s, _) ->
            if consumeOne(delete, InputEvType.Press).IsSome && s.Value.Length > 0 then
                Setting.app (fun (x: string) -> x.Substring (0, x.Length - 1)) s
            elif consumeOne(bigDelete, InputEvType.Press).IsSome then
                s.Value <-
                    let parts = s.Value.Split(" ")
                    Array.take (parts.Length - 1) parts |> String.concat " "
            //todo: clipboard support
            if inputmethod_mousedist > 200f then removeInputMethod()
        | InputMethod.Bind cb ->
            match consumeAny InputEvType.Press with
            | ValueSome x -> removeInputMethod(); cb x; 
            | ValueNone -> ()
        | InputMethod.None -> ()
        if typed then absorbAll()
        typed <- false

module Mouse = 
    let X() = Input.mousex
    let Y() = Input.mousey
    let Scroll() = let v = Input.mousez - Input.oldmousez in Input.oldmousez <- Input.mousez; v

    let Click b = Input.consumeOne(Mouse b, InputEvType.Press).IsSome
    let Held b = Input.held (Mouse b)
    let Released b = Input.consumeOne(Mouse b, InputEvType.Release).IsSome
    let Moved() = Input.mousex <> Input.oldmousex || Input.mousey <> Input.oldmousey

    let Hover (r: Rect) = r.Contains(X(), Y())

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