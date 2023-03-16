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
        | Key (k, m) -> Bind.ModifierString m + Bind.FormatKey k
        | Mouse b -> "M" + b.ToString()
        | Joystick _ -> "nyi"

    static member private FormatKey (k: Keys) : string =
        match k with
        | Keys.D0 -> "0"
        | Keys.D1 -> "1"
        | Keys.D2 -> "2"
        | Keys.D3 -> "3"
        | Keys.D4 -> "4"
        | Keys.D5 -> "5"
        | Keys.D6 -> "6"
        | Keys.D7 -> "7"
        | Keys.D8 -> "8"
        | Keys.D9 -> "9"
        | _ -> k.ToString()

    static member private ModifierString (ctrl, alt, shift) =
        (if ctrl then "Ctrl + " else "")
        + (if alt then "Alt + " else "")
        + (if shift then "Shift + " else "")

    static member DummyBind = Setting.simple Dummy

    static member internal IsModifier (k: Keys) =
           k = Keys.LeftShift
        || k = Keys.RightShift 
        || k = Keys.LeftControl 
        || k = Keys.RightControl 
        || k = Keys.LeftAlt 
        || k = Keys.RightAlt

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

type internal FrameEvents =
    {
        MouseX : float32
        MouseY : float32
        MouseZ : float32
        KeyboardState: KeyboardState
        MouseState: MouseState
        Ctrl : bool
        Alt : bool
        Shift : bool
        TypedText : string
    }

module internal InputThread =
    
    let mutable private mouse_x = 0.0f
    let mutable private mouse_y = 0.0f
    let mutable private mouse_z = 0.0f
    let mutable private typed_text = ""
    let mutable private events_buffer : InputEv list = []

    let mutable typing = false
    
    let private sinceLastTyped = Diagnostics.Stopwatch.StartNew()
    
    let private LOCK_OBJ = Object()
    
    let poll(keyboard: KeyboardState, mouse: MouseState) =
        let add x = if sinceLastTyped.ElapsedMilliseconds > 50l then lock LOCK_OBJ (fun () -> events_buffer <- List.append events_buffer [x])
        let now = Song.timeWithOffset()
    
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
                    struct((enum k, (ctrl, alt, shift)) |> Key, InputEvType.Release, now) |> add
    
        // mouse input handler
        for b in 0 .. int MouseButton.Last do
            if mouse.IsButtonDown(enum b) then
                if mouse.WasButtonDown(enum b) |> not then
                    struct(enum b |> Mouse, InputEvType.Press, now) |> add
            elif mouse.WasButtonDown(enum b) then
                struct(enum b |> Mouse, InputEvType.Release, now) |> add
    
        // joystick stuff NYI

    let mutable internal gw : NativeWindow = null
    
    let init (win: NativeWindow) =
        gw <- win
        gw.add_MouseWheel(fun e -> mouse_z <- mouse_z + e.OffsetY)
        gw.add_MouseMove
            (fun e ->
                mouse_x <- Math.Clamp(Viewport.vwidth / float32 Viewport.rwidth * float32 e.X, 0.0f, Viewport.vwidth)
                mouse_y <- Math.Clamp(Viewport.vheight / float32 Viewport.rheight * float32 e.Y, 0.0f, Viewport.vheight)
            )
        gw.add_TextInput
            (fun e ->
                if typing then 
                    sinceLastTyped.Restart()
                    lock LOCK_OBJ (fun () -> typed_text <- typed_text + e.AsString)
            )

    let fetch(events_this_frame : InputEv list byref, this_frame : FrameEvents byref) =
        let a, b = 
            lock LOCK_OBJ (fun () ->
                let kb = gw.KeyboardState.GetSnapshot()
                let ms = gw.MouseState.GetSnapshot()
                let output =
                    {
                        MouseX = mouse_x
                        MouseY = mouse_y
                        MouseZ = mouse_z
                        TypedText = typed_text
                        KeyboardState = kb
                        MouseState = ms
                        Ctrl = kb.IsKeyDown Keys.LeftControl || kb.IsKeyDown Keys.RightControl
                        Shift = kb.IsKeyDown Keys.LeftShift || kb.IsKeyDown Keys.RightShift
                        Alt = kb.IsKeyDown Keys.LeftAlt || kb.IsKeyDown Keys.RightAlt
                    }, events_buffer
                events_buffer <- []
                typed_text <- ""
                output
            )
        this_frame <- a
        events_this_frame <- b

module Input =

    let mutable internal this_frame : FrameEvents = Unchecked.defaultof<_>
    let mutable internal last_frame : FrameEvents = Unchecked.defaultof<_>
    let mutable internal thisFrameFinished = false
    
    let mutable internal events_this_frame : InputEv list = []

    let mutable internal inputmethod : InputMethod = InputMethod.None
    let mutable internal inputmethod_mousedist = 0f

    let mutable internal scrolled_this_frame = 0f
    
    let mutable internal gw : NativeWindow = null
    
    let removeInputMethod() =
        match inputmethod with
        | InputMethod.Text (s, callback) -> InputThread.typing <- false; callback()
        | InputMethod.Bind _
        | InputMethod.None -> ()
        inputmethod <- InputMethod.None
    
    let setTextInput (s: Setting<string>, callback: unit -> unit) =
        removeInputMethod()
        inputmethod <- InputMethod.Text (s, callback)
        InputThread.typing <- true
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
            | struct (b, T, time) :: xs when T = t -> out <- ValueSome (b, time); xs
            | x :: xs -> x :: (f xs)
        events_this_frame <- f events_this_frame
        out

    let finish_frame_events() =
        inputmethod_mousedist <- inputmethod_mousedist + abs(this_frame.MouseX - last_frame.MouseX) + abs(this_frame.MouseY - last_frame.MouseY)
        last_frame <- this_frame
        events_this_frame <- []
        thisFrameFinished <- true

    let held (b: Bind) =
        if thisFrameFinished then false
        else
        match b with
        | Key (Keys.LeftControl, _)
        | Key (Keys.RightControl, _) ->  this_frame.Ctrl
        | Key (Keys.LeftAlt, _)
        | Key (Keys.RightAlt, _) -> this_frame.Alt
        | Key (Keys.LeftShift, _)
        | Key (Keys.RightShift, _) -> this_frame.Shift
        | Key (k, m) -> this_frame.KeyboardState.[k] && m = (this_frame.Ctrl, this_frame.Alt, this_frame.Shift)
        | Mouse m -> this_frame.MouseState.[m]
        | Dummy -> false
        | Joystick _ -> false
    
    let init (win: NativeWindow) =
        gw <- win
        InputThread.init gw

    let updateIM() =
        
        let deleteChar = Bind.mk Keys.Backspace
        let deleteWord = Bind.ctrl Keys.Backspace
        let copy = Bind.ctrl Keys.C
        let paste = Bind.ctrl Keys.V

        match inputmethod with
        | InputMethod.Text (s, _) ->

            if this_frame.TypedText <> "" then s.Value <- s.Value + this_frame.TypedText

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
            | ValueSome (x, _) ->
                match x with
                | Key (k, m) ->
                    if Bind.IsModifier k then ()
                    else removeInputMethod(); callback x
                | _ -> removeInputMethod(); callback x
            | ValueNone ->
                match consumeAny InputEvType.Release with
                | ValueSome (x, _) -> 
                    match x with
                    | Key (k, m) ->
                        if Bind.IsModifier k then removeInputMethod(); callback x
                    | _ -> ()
                | ValueNone -> ()

        | InputMethod.None -> ()

    let begin_frame_events() =
        last_frame <- this_frame
        InputThread.fetch(&events_this_frame, &this_frame)
        if Object.ReferenceEquals(null, last_frame) then last_frame <- this_frame
        scrolled_this_frame <- this_frame.MouseZ - last_frame.MouseZ
        thisFrameFinished <- false
        updateIM()

module Mouse =

    let LEFT = MouseButton.Left
    let RIGHT = MouseButton.Right

    let pos() = (Input.this_frame.MouseX, Input.last_frame.MouseY)
    let x() = fst (pos())
    let y() = snd (pos())
    let scroll() =
        let v = Input.scrolled_this_frame
        Input.scrolled_this_frame <- 0.0f
        v

    let private click b = Input.consumeOne(Mouse b, InputEvType.Press).IsSome
    let leftClick () = click LEFT
    let rightClick () = click RIGHT

    let held b = Input.held (Mouse b)
    let released b = Input.consumeOne(Mouse b, InputEvType.Release).IsSome
    let moved() = Input.this_frame.MouseX <> Input.last_frame.MouseX || Input.this_frame.MouseY <> Input.last_frame.MouseY

    let hover (r: Rect) = not Input.thisFrameFinished && r.Contains(pos())

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