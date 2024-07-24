namespace Percyqaz.Flux.Input

open System
open OpenTK
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio

type Keys = OpenTK.Windowing.GraphicsLibraryFramework.Keys
type MouseButton = OpenTK.Windowing.GraphicsLibraryFramework.MouseButton

type Bind =
    | Dummy
    | Key of Keys * modifiers: (bool * bool * bool)
    | Mouse of MouseButton

    override this.ToString() =
        match this with
        | Dummy -> "NONE"
        | Key(k, m) -> Bind.ModifierString m + Bind.FormatKey k
        | Mouse b -> "M" + b.ToString()

    member this.WithModifiers (ctrl, alt, shift) : Bind =
        match this with
        | Dummy -> Dummy
        | Key (k, m) -> Key (k, (ctrl, alt, shift))
        | Mouse b -> Mouse b

    static member private FormatKey(k: Keys) : string =
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
        | Keys.KeyPad0 -> "N0"
        | Keys.KeyPad1 -> "N1"
        | Keys.KeyPad2 -> "N2"
        | Keys.KeyPad3 -> "N3"
        | Keys.KeyPad4 -> "N4"
        | Keys.KeyPad5 -> "N5"
        | Keys.KeyPad6 -> "N6"
        | Keys.KeyPad7 -> "N7"
        | Keys.KeyPad8 -> "N8"
        | Keys.KeyPad9 -> "N9"
        | Keys.LeftAlt -> "LAlt"
        | Keys.RightAlt -> "RAlt"
        | Keys.LeftSuper -> "LSuper"
        | Keys.RightSuper -> "RSuper"
        | Keys.LeftControl -> "LCtrl"
        | Keys.RightControl -> "RCtrl"
        | Keys.LeftShift -> "LShift"
        | Keys.RightShift -> "RShift"
        | _ -> k.ToString()

    static member private ModifierString(ctrl, alt, shift) =
        (if ctrl then "Ctrl + " else "")
        + (if alt then "Alt + " else "")
        + (if shift then "Shift + " else "")

    static member internal IsModifier(k: Keys) =
        k = Keys.LeftShift
        || k = Keys.RightShift
        || k = Keys.LeftControl
        || k = Keys.RightControl
        || k = Keys.LeftAlt
        || k = Keys.RightAlt

module Bind =

    let inline mk k = Key(k, (false, false, false))
    let inline ctrl k = Key(k, (true, false, false))
    let inline alt k = Key(k, (false, true, false))
    let inline shift k = Key(k, (false, false, true))
    let inline ctrlShift k = Key(k, (true, false, true))
    let inline ctrlAlt k = Key(k, (true, true, false))

type InputEvType =
    | Press = 0
    | Release = 1

type InputEv = (struct (Bind * InputEvType * float32<ms>))

[<RequireQualifiedAccess>]
type InputListener =
    | Text of setting: Setting<string> * on_remove: (unit -> unit)
    | Bind of callback: (Bind -> unit)
    | None

type internal FrameEvents =
    {
        MouseX: float32
        MouseY: float32
        MouseZ: float32
        KeyboardState: KeyboardState
        MouseState: MouseState
        Ctrl: bool
        Alt: bool
        Shift: bool
        TypedText: string
    }

module internal InputThread =

    let mutable private mouse_x = 0.0f
    let mutable private mouse_y = 0.0f
    let mutable private mouse_z = 0.0f
    let mutable private typed_text = ""
    let mutable private events_buffer: InputEv list = []

    let mutable typing = false

    let private since_last_typed = Diagnostics.Stopwatch.StartNew()

    let private LOCK_OBJ = Object()

    let poll (keyboard: KeyboardState, mouse: MouseState) =
        let add x =
            if since_last_typed.ElapsedMilliseconds > 50l then
                lock LOCK_OBJ (fun () -> events_buffer <- List.append events_buffer [ x ])

        let now = Song.time_with_offset ()

        let ctrl =
            keyboard.IsKeyDown Keys.LeftControl || keyboard.IsKeyDown Keys.RightControl

        let shift = keyboard.IsKeyDown Keys.LeftShift || keyboard.IsKeyDown Keys.RightShift
        let alt = keyboard.IsKeyDown Keys.LeftAlt || keyboard.IsKeyDown Keys.RightAlt

        // keyboard input handler
        // todo: way of remembering modifier combo for hold/release?
        for k in 0 .. int Keys.LastKey do
            //if k < 340 || k > 347 then
            if keyboard.IsKeyDown(enum k) then
                if keyboard.WasKeyDown(enum k) |> not then
                    struct ((enum k, (ctrl, alt, shift)) |> Key, InputEvType.Press, now) |> add
            elif keyboard.WasKeyDown(enum k) then
                struct ((enum k, (ctrl, alt, shift)) |> Key, InputEvType.Release, now) |> add

        // mouse input handler
        for b in 0 .. int MouseButton.Last do
            if mouse.IsButtonDown(enum b) then
                if mouse.WasButtonDown(enum b) |> not then
                    struct (enum b |> Mouse, InputEvType.Press, now) |> add
            elif mouse.WasButtonDown(enum b) then
                struct (enum b |> Mouse, InputEvType.Release, now) |> add

    let mutable internal game_window: NativeWindow = null

    let init (win: NativeWindow) =
        game_window <- win
        game_window.add_MouseWheel (fun e -> mouse_z <- mouse_z + e.OffsetY)

        game_window.add_MouseMove (fun e ->
            mouse_x <- Math.Clamp(Viewport.vwidth / float32 Viewport.rwidth * float32 e.X, 0.0f, Viewport.vwidth)
            mouse_y <- Math.Clamp(Viewport.vheight / float32 Viewport.rheight * float32 e.Y, 0.0f, Viewport.vheight)
        )

        game_window.add_TextInput (fun e ->
            if typing then
                since_last_typed.Restart()
                lock LOCK_OBJ (fun () -> typed_text <- typed_text + e.AsString)
        )

    let fetch (events_this_frame: InputEv list byref, this_frame: FrameEvents byref) =
        let a, b =
            lock
                LOCK_OBJ
                (fun () ->
                    let kb = game_window.KeyboardState.GetSnapshot()
                    let ms = game_window.MouseState.GetSnapshot()

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
                        },
                        events_buffer

                    events_buffer <- []
                    typed_text <- ""
                    output
                )

        this_frame <- a
        events_this_frame <- b

module Input =

    let mutable last_time_mouse_moved = 0L
    let mutable last_input_event = 0L
    let mutable internal this_frame: FrameEvents = Unchecked.defaultof<_>
    let mutable internal last_frame: FrameEvents = Unchecked.defaultof<_>
    let mutable internal this_frame_finished = false

    let mutable internal events_this_frame: InputEv list = []

    let mutable internal input_listener: InputListener = InputListener.None
    let mutable internal input_listener_mouse_cancel = 0f

    let mutable internal scrolled_this_frame = 0f

    let mutable internal gw: NativeWindow = null

    /// Stops listening to text input OR to the next button pressed, if system previously was
    let remove_listener () =
        match input_listener with
        | InputListener.Text(_, on_remove) ->
            InputThread.typing <- false
            on_remove ()
        | InputListener.Bind _
        | InputListener.None -> ()

        input_listener <- InputListener.None

    /// Used for UIs that let the user type some text into the provided text buffer
    /// In this mode keybindings do not activate (so something bound to A will not fire, it will type 'a' into the buffer instead)
    /// `mouse_cancel` set to true will cause the listener to automatically disconnect itself when the mouse is moved too much
    /// `on_remove` is called when the text listening mode is exited
    let listen_to_text (s: Setting<string>, mouse_cancel: bool, on_remove: unit -> unit) =
        remove_listener ()
        input_listener <- InputListener.Text(s, on_remove)
        InputThread.typing <- true
        input_listener_mouse_cancel <- if mouse_cancel then 0f else -infinityf

    /// Used for UIs that let the user bind a key to a hotkey
    /// The very next button (or modifier + button combo) they press will be passed to `callback`
    /// Then the listener is removed
    let listen_to_next_key (callback: Bind -> unit) =
        remove_listener ()
        input_listener <- InputListener.Bind callback

    let pop_matching (b: Bind, t: InputEvType) =
        let mutable out = ValueNone

        let rec f evs =
            match evs with
            | [] -> []
            | struct (B, T, time) :: xs when B = b && T = t ->
                out <- ValueSome time
                xs
            | x :: xs -> x :: (f xs)

        events_this_frame <- f events_this_frame
        out

    let pop_gameplay (now: Time) (binds: Bind array) (callback: int -> Time -> bool -> unit) =
        let bind_match bind target =
            match bind, target with
            | Key(k, _), Key(K, _) when k = K -> true
            | Mouse b, Mouse B when b = B -> true
            | _ -> false

        let rec pop_inputs_matching_binds evs =
            match evs with
            | [] -> []
            | struct (b, t, time) :: xs ->
                let mutable i = 0
                let mutable matched = false

                while i < binds.Length && not matched do
                    if bind_match binds.[i] b then
                        if time <= now then callback i time (t <> InputEvType.Press)
                        matched <- true

                    i <- i + 1

                if matched then pop_inputs_matching_binds xs else struct (b, t, time) :: (pop_inputs_matching_binds xs)

        events_this_frame <- pop_inputs_matching_binds events_this_frame

    let pop_any (t: InputEvType) =
        let mutable out = ValueNone

        let rec f evs =
            match evs with
            | [] -> []
            | struct (b, T, time) :: xs when T = t ->
                out <- ValueSome(b, time)
                xs
            | x :: xs -> x :: (f xs)

        events_this_frame <- f events_this_frame
        out

    let finish_frame_events () =
        input_listener_mouse_cancel <-
            input_listener_mouse_cancel
            + abs (this_frame.MouseX - last_frame.MouseX)
            + abs (this_frame.MouseY - last_frame.MouseY)

        last_frame <- this_frame
        events_this_frame <- []
        this_frame_finished <- true

    let held (b: Bind) =
        if this_frame_finished then
            false
        else

        match b with
        | Key(Keys.LeftControl, _)
        | Key(Keys.RightControl, _) -> this_frame.Ctrl
        | Key(Keys.LeftAlt, _)
        | Key(Keys.RightAlt, _) -> this_frame.Alt
        | Key(Keys.LeftShift, _)
        | Key(Keys.RightShift, _) -> this_frame.Shift
        | Key(k, m) ->
            this_frame.KeyboardState.[k]
            && m = (this_frame.Ctrl, this_frame.Alt, this_frame.Shift)
        | Mouse m -> this_frame.MouseState.[m]
        | Dummy -> false

    let init (win: NativeWindow) =
        gw <- win
        InputThread.init gw

    let private DELETE_CHARACTER = Bind.mk Keys.Backspace
    let private DELETE_WORD = Bind.ctrl Keys.Backspace
    let private DELETE_REPEAT_DELAY = 400L
    let private DELETE_REPEAT_TIME = 30L
    let private COPY = Bind.ctrl Keys.C
    let private PASTE = Bind.ctrl Keys.V
    let mutable private delete_held_timestamp = 0L
    let mutable private delete_repeat_counter = 0L

    let update_input_listener () =

        match input_listener with
        | InputListener.Text(s, _) ->

            if this_frame.TypedText <> "" then
                s.Value <- s.Value + this_frame.TypedText

            if pop_matching(DELETE_CHARACTER, InputEvType.Press).IsSome && s.Value.Length > 0 then
                Setting.app (fun (x: string) -> x.Substring(0, x.Length - 1)) s
                delete_held_timestamp <- DateTime.UtcNow.Ticks
                delete_repeat_counter <- -1L

            elif pop_matching(DELETE_WORD, InputEvType.Press).IsSome then
                s.Value <-
                    let parts = s.Value.Split(" ")
                    Array.take (parts.Length - 1) parts |> String.concat " "
                delete_held_timestamp <- DateTime.UtcNow.Ticks
                delete_repeat_counter <- -1L

            elif (held DELETE_CHARACTER || held DELETE_WORD) && s.Value.Length > 0 then
                let rep = ( (DateTime.UtcNow.Ticks - delete_held_timestamp) / 10_000L - DELETE_REPEAT_DELAY) / DELETE_REPEAT_TIME
                if delete_repeat_counter < rep then
                    delete_repeat_counter <- delete_repeat_counter + 1L

                    if this_frame.Ctrl then
                        s.Value <-
                            let parts = s.Value.Split(" ")
                            Array.take (parts.Length - 1) parts |> String.concat " "
                    else
                        Setting.app (fun (x: string) -> x.Substring(0, x.Length - 1)) s

            elif pop_matching(COPY, InputEvType.Press).IsSome then
                gw.ClipboardString <- s.Value

            elif pop_matching(PASTE, InputEvType.Press).IsSome then
                s.Value <-
                    s.Value
                    + try
                        gw.ClipboardString
                      with _ ->
                          ""

            else
                delete_repeat_counter <- Int64.MaxValue

            if input_listener_mouse_cancel > 200f then
                remove_listener ()

        | InputListener.Bind callback ->
            match pop_any InputEvType.Press with
            | ValueSome(x, _) ->
                match x with
                | Key(k, m) ->
                    if Bind.IsModifier k then
                        ()
                    else
                        remove_listener ()
                        callback x
                | _ ->
                    remove_listener ()
                    callback x
            | ValueNone ->
                match pop_any InputEvType.Release with
                | ValueSome(x, _) ->
                    match x with
                    | Key(k, m) ->
                        if Bind.IsModifier k then
                            remove_listener ()
                            callback x
                    | _ -> ()
                | ValueNone -> ()

        | InputListener.None -> ()

    let begin_frame_events () =
        last_frame <- this_frame
        InputThread.fetch (&events_this_frame, &this_frame)

        if Object.ReferenceEquals(null, last_frame) then
            last_frame <- this_frame

        if
            (this_frame.MouseX <> last_frame.MouseX)
            || (this_frame.MouseY <> last_frame.MouseY)
        then
            last_time_mouse_moved <- DateTime.UtcNow.Ticks

        if events_this_frame <> [] then
            last_input_event <- DateTime.UtcNow.Ticks

        scrolled_this_frame <- this_frame.MouseZ - last_frame.MouseZ
        this_frame_finished <- false
        update_input_listener ()

    let button_pressed_recently () =
        (DateTime.UtcNow.Ticks - last_input_event) < 100L * 10_000L

module Mouse =

    let LEFT = MouseButton.Left
    let RIGHT = MouseButton.Right

    let pos () =
        (Input.this_frame.MouseX, Input.last_frame.MouseY)

    let x () = fst (pos ())
    let y () = snd (pos ())

    let scroll () =
        let v = Input.scrolled_this_frame
        Input.scrolled_this_frame <- 0.0f
        v

    let private click b : bool =
        Input.pop_matching(Mouse b, InputEvType.Press).IsSome

    let left_click () : bool = click LEFT
    let right_click () : bool = click RIGHT

    let held b : bool = Input.held (Mouse b)

    let released b : bool =
        Input.pop_matching(Mouse b, InputEvType.Release).IsSome

    let moved_recently () : bool =
        (DateTime.UtcNow.Ticks - Input.last_time_mouse_moved) < 100L * 10_000L

    let hover (r: Rect) : bool =
        not Input.this_frame_finished && r.Contains(pos ())

type Bind with
    member this.Pressed() =
        match this with
        | Key _
        | Mouse _ -> Input.held this
        | _ -> false

    member this.Tapped() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputEvType.Press).IsSome
        | _ -> false

    member this.Released() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputEvType.Release).IsSome
        | _ -> false