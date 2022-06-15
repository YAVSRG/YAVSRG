namespace Percyqaz.Flux.Input

open System
open OpenTK
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio

type Bind =
    | Dummy
    | Key of Keys * modifiers: (bool * bool * bool)
    | Mouse of MouseButton
    | Joystick of NYI

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
    
    let mutable internal events_buffer : InputEv list = []
    let mutable internal typed_buffer : string = ""
    let mutable internal events_this_frame : InputEv list = []
    let mutable internal typed_this_frame : string = ""

    let private sinceLastTyped = Diagnostics.Stopwatch.StartNew()
    
    module internal ThisFrame =
        let mutable mx = 0.0f
        let mutable my = 0.0f
        let mutable mz = 0.0f

        let mutable ctrl = false
        let mutable shift = false
        let mutable alt = false

        let mutable kb : KeyboardState = null
        let mutable mouse : MouseState = null

        let mutable finished = false

    module internal LastFrame = 
        let mutable mx = 0.0f
        let mutable my = 0.0f
        let mutable mz = 0.0f

        let mutable kb = null
        let mutable mouse = null

    let mutable internal gw : NativeWindow = null

    let mutable internal inputmethod : InputMethod = InputMethod.None
    let mutable internal inputmethod_mousedist = 0f
    
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

    let consumeOne (b: Bind, t: InputEvType) =
        let mutable out = ValueNone
        let rec f evs =
            match evs with
            | [] -> []
            | struct (B, T, time) :: xs when B = b && T = t -> out <- ValueSome time; xs
            | x :: xs -> x :: (f xs)
        events_this_frame <- f events_this_frame
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
        events_this_frame <- f events_this_frame

    let consumeAny (t: InputEvType) =
        let mutable out = ValueNone
        let rec f evs =
            match evs with
            | [] -> []
            | struct (b, T, time) :: xs when T = t -> out <- ValueSome b; xs
            | x :: xs -> x :: (f xs)
        events_this_frame <- f events_this_frame
        out

    let held (b: Bind) =
        if ThisFrame.finished then false
        else
        match b with
        | Key (Keys.LeftControl, _) -> ThisFrame.ctrl
        | Key (Keys.RightControl, _) -> ThisFrame.ctrl
        | Key (Keys.LeftAlt, _) -> ThisFrame.alt
        | Key (Keys.RightAlt, _) -> ThisFrame.alt
        | Key (Keys.LeftShift, _) -> ThisFrame.shift
        | Key (Keys.RightShift, _) -> ThisFrame.shift
        | Key (k, m) -> ThisFrame.kb.[k] && m = (ThisFrame.ctrl, ThisFrame.alt, ThisFrame.shift)
        | Mouse m -> ThisFrame.mouse.[m]
        | Dummy -> false
        | Joystick _ -> false

    let private lockObj = Object()

    let private fetch() =
        lock lockObj (fun () -> 
            events_this_frame <- events_buffer
            events_buffer <- []
            typed_this_frame <- typed_buffer
            typed_buffer <- ""
            ThisFrame.mouse <- gw.MouseState.GetSnapshot()
            ThisFrame.kb <- gw.KeyboardState.GetSnapshot()
        )
        ThisFrame.finished <- false
        ThisFrame.ctrl <- ThisFrame.kb.IsKeyDown Keys.LeftControl || ThisFrame.kb.IsKeyDown Keys.RightControl
        ThisFrame.shift <- ThisFrame.kb.IsKeyDown Keys.LeftShift || ThisFrame.kb.IsKeyDown Keys.RightShift
        ThisFrame.alt <- ThisFrame.kb.IsKeyDown Keys.LeftAlt || ThisFrame.kb.IsKeyDown Keys.RightAlt

    let finish_frame_events() =
        inputmethod_mousedist <- inputmethod_mousedist + abs(ThisFrame.mx - LastFrame.mx) + abs (ThisFrame.my - LastFrame.my)
        LastFrame.mouse <- ThisFrame.mouse
        LastFrame.kb <- ThisFrame.kb
        LastFrame.mx <- ThisFrame.mx
        LastFrame.my <- ThisFrame.my
        LastFrame.mz <- ThisFrame.mz
        ThisFrame.finished <- true
        events_this_frame <- []

    let poll(keyboard: KeyboardState, mouse: MouseState) =
        let add x = if sinceLastTyped.ElapsedMilliseconds > 50l then lock lockObj (fun () -> events_buffer <- List.append events_buffer [x])
        let now = Track.timeWithOffset()

        let ctrl = keyboard.IsKeyDown Keys.LeftControl || keyboard.IsKeyDown Keys.RightControl
        let shift = keyboard.IsKeyDown Keys.LeftShift || keyboard.IsKeyDown Keys.RightShift
        let alt = keyboard.IsKeyDown Keys.LeftAlt || keyboard.IsKeyDown Keys.RightAlt

        // keyboard input handler
        // todo: way of remembering modifier combo for hold/release?
        for k in 0 .. int Keys.LastKey do
            //if k < 340 || k > 347 then
                if keyboard.IsKeyDown(enum k) then
                    if keyboard.WasKeyDown(enum k) |> not then
                        struct((enum k, (ctrl, alt, shift)) |> Key, InputEvType.Press, now) |> add
                elif keyboard.WasKeyDown(enum k) then
                    struct((enum k, (false, false, false)) |> Key, InputEvType.Release, now) |> add

        // mouse input handler
        for b in 0 .. int MouseButton.Last do
            if mouse.IsButtonDown(enum b) then
                if mouse.WasButtonDown(enum b) |> not then
                    struct(enum b |> Mouse, InputEvType.Press, now) |> add
            elif mouse.WasButtonDown(enum b) then
                struct(enum b |> Mouse, InputEvType.Release, now) |> add

        // joystick stuff NYI
    
    let init (win: NativeWindow) =
        gw <- win
        gw.add_MouseWheel(fun e -> ThisFrame.mz <- ThisFrame.mz + e.OffsetY)
        gw.add_MouseMove(
            fun e ->
                ThisFrame.mx <- Math.Clamp(Viewport.vwidth / float32 Viewport.rwidth * float32 e.X, 0.0f, Viewport.vwidth)
                ThisFrame.my <- Math.Clamp(Viewport.vheight / float32 Viewport.rheight * float32 e.Y, 0.0f, Viewport.vheight))
        gw.add_TextInput(fun e ->
            match inputmethod with
            | InputMethod.Text _ -> sinceLastTyped.Restart(); lock lockObj (fun () -> typed_buffer <- typed_buffer + e.AsString)
            | InputMethod.Bind _
            | InputMethod.None -> ())

    let updateIM() =
        
        let deleteChar = Bind.mk Keys.Backspace
        let deleteWord = Bind.ctrl Keys.Backspace
        let copy = Bind.ctrl Keys.C
        let paste = Bind.ctrl Keys.V

        match inputmethod with
        | InputMethod.Text (s, _) ->

            if typed_this_frame <> "" then s.Value <- s.Value + typed_this_frame

            if consumeOne(deleteChar, InputEvType.Press).IsSome && s.Value.Length > 0 then
                Setting.app (fun (x: string) -> x.Substring (0, x.Length - 1)) s
            elif consumeOne(deleteWord, InputEvType.Press).IsSome then
                s.Value <-
                    let parts = s.Value.Split(" ")
                    Array.take (parts.Length - 1) parts |> String.concat " "
            elif consumeOne(copy, InputEvType.Press).IsSome then
                gw.ClipboardString <- s.Value
            elif consumeOne(paste, InputEvType.Press).IsSome then
                s.Value <- s.Value + try gw.ClipboardString with _ -> ""

            if inputmethod_mousedist > 200f then removeInputMethod()

        | InputMethod.Bind callback ->
            match consumeAny InputEvType.Press with
            | ValueSome x -> removeInputMethod(); callback x
            | ValueNone -> ()

        | InputMethod.None -> ()

    let update() =

        fetch()
        updateIM()

module Mouse =

    let LEFT = MouseButton.Left
    let RIGHT = MouseButton.Right

    let pos() = (Input.ThisFrame.mx, Input.ThisFrame.my)
    let x() = fst (pos())
    let scroll() = let v = Input.ThisFrame.mz - Input.LastFrame.mz in Input.LastFrame.mz <- Input.ThisFrame.mz; v

    let private click b = Input.consumeOne(Mouse b, InputEvType.Press).IsSome
    let leftClick () = click LEFT
    let rightClick () = click RIGHT

    let held b = Input.held (Mouse b)
    let released b = Input.consumeOne(Mouse b, InputEvType.Release).IsSome
    let moved() = Input.ThisFrame.mx <> Input.LastFrame.mx || Input.ThisFrame.my <> Input.LastFrame.my

    let hover (r: Rect) = r.Contains(pos())

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