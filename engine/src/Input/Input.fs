namespace Percyqaz.Flux.Input

open System
open OpenTK
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Audio

type Keys = Windowing.GraphicsLibraryFramework.Keys
type MouseButton = Windowing.GraphicsLibraryFramework.MouseButton
type InputAction = Windowing.GraphicsLibraryFramework.InputAction

[<RequireQualifiedAccess>]
type Bind =
    | Dummy
    | Key of Keys * modifiers: (bool * bool * bool)
    | Mouse of MouseButton

    override this.ToString() =
        match this with
        | Dummy -> "NONE"
        | Key(k, m) -> Bind.ModifierString m + Bind.FormatKey k
        | Mouse b -> "M" + b.ToString()

    member this.WithModifiers(ctrl: bool, alt: bool, shift: bool) : Bind =
        match this with
        | Dummy -> Dummy
        | Key (k, m) -> Key (k, (ctrl, alt, shift))
        | Mouse b -> Mouse b

    member this.IsKeyWithAnyModifiers(k: Keys) : bool =
        match this with
        | Key (k1, _) when k1 = k -> true
        | _ -> false

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
        | Keys.LeftBracket -> "["
        | Keys.RightBracket -> "]"
        | Keys.Comma -> ","
        | Keys.Period -> "."
        | Keys.GraveAccent -> "`"
        | Keys.Slash -> "/"
        | Keys.Backslash -> "\\"
        | Keys.Semicolon -> ";"
        | Keys.Apostrophe -> "'"
        | Keys.Minus -> "-"
        | Keys.Equal -> "="
        | _ -> k.ToString()

    static member private ModifierString(ctrl: bool, alt: bool, shift: bool) : string =
        (if ctrl then "Ctrl + " else "")
        + (if alt then "Alt + " else "")
        + (if shift then "Shift + " else "")

    static member internal IsModifier(key: Keys) : bool =
        key = Keys.LeftShift
        || key = Keys.RightShift
        || key = Keys.LeftControl
        || key = Keys.RightControl
        || key = Keys.LeftAlt
        || key = Keys.RightAlt

module Bind =

    let inline mk key = Bind.Key(key, (false, false, false))
    let inline ctrl key = Bind.Key(key, (true, false, false))
    let inline alt key = Bind.Key(key, (false, true, false))
    let inline shift key = Bind.Key(key, (false, false, true))
    let inline ctrl_shift key = Bind.Key(key, (true, false, true))
    let inline ctrl_alt key = Bind.Key(key, (true, true, false))

[<Struct>]
type internal InputEvent =
    {
        Bind: Bind
        Action: InputAction
        GLFWTimestamp: float
        AudioPosition: Time
    }

[<RequireQualifiedAccess>]
type internal InputListener =
    | Text of setting: Setting<string> * on_remove: (unit -> unit)
    | Bind of callback: (Bind -> unit)
    | None

type internal FrameEvents =
    {
        MouseX: float32
        MouseY: float32
        MouseZ: float32
        CursorInBounds: bool
        HeldKeys: Set<Keys>
        HeldMouseButtons: Set<MouseButton>
        Ctrl: bool
        Alt: bool
        Shift: bool
        TypedText: string
        Time: float
    }

module internal InputThread =

    let mutable private hide_cursor = false
    let mutable private cursor_in_bounds = true
    let mutable private mouse_x = 0.0f
    let mutable private mouse_y = 0.0f
    let mutable private mouse_z = 0.0f
    let mutable private typed_text = ""
    let mutable private events_buffer: InputEvent list = []
    let mutable private ctrl = false
    let mutable private alt = false
    let mutable private shift = false
    let mutable private held_keys = Set.empty<Keys>
    let mutable private held_mouse_buttons = Set.empty<MouseButton>

    let mutable private typing = false
    let mutable private typing_buffered_input : InputEvent voption = ValueNone

    let private LOCK_OBJ = Object()

    let private char_callback (_: nativeptr<Window>) (char: uint32) : unit =
        if typing then
            lock LOCK_OBJ (fun () ->
                typed_text <- typed_text + Char.ConvertFromUtf32(int32 char)
                typing_buffered_input <- ValueNone
            )
    let private char_callback_d = GLFWCallbacks.CharCallback char_callback

    let private cursor_pos_callback (window: nativeptr<Window>) (x: float) (y: float) : unit =
        lock LOCK_OBJ (fun () ->
            let pc_x = (float32 x - float32 Render._viewport_left) / float32 Render._viewport_width
            mouse_x <- Math.Clamp(Render._width * pc_x, 0.0f, Render._width)

            let pc_y = (float32 y - float32 Render._viewport_top) / float32 Render._viewport_height
            mouse_y <- Math.Clamp(Render._height * pc_y, 0.0f, Render._height)

            let was_in_bounds = cursor_in_bounds
            cursor_in_bounds <- pc_x >= 0.0f && pc_y >= 0.0f && pc_x <= 1.0f && pc_y <= 1.0f
            if cursor_in_bounds <> was_in_bounds && hide_cursor then
                GLFW.SetInputMode(window, CursorStateAttribute.Cursor, if cursor_in_bounds then CursorModeValue.CursorHidden else CursorModeValue.CursorNormal)
        )
    let private cursor_pos_callback_d = GLFWCallbacks.CursorPosCallback(cursor_pos_callback)

    let private scroll_callback (_: nativeptr<Window>) (offset_x: float) (offset_y: float) : unit =
        lock LOCK_OBJ (fun () ->
            mouse_z <- mouse_z + float32 offset_y
        )
    let private scroll_callback_d = GLFWCallbacks.ScrollCallback(scroll_callback)

    let private key_callback (_: nativeptr<Window>) (key: Keys) (scancode: int) (action: InputAction) (modifiers: KeyModifiers) : unit =
        let event =
            {
                Bind =
                    (key,
                        (
                            modifiers &&& KeyModifiers.Control = KeyModifiers.Control,
                            modifiers &&& KeyModifiers.Alt = KeyModifiers.Alt,
                            modifiers &&& KeyModifiers.Shift = KeyModifiers.Shift
                        )
                    ) |> Bind.Key
                Action = action
                GLFWTimestamp = GLFW.GetTime()
                AudioPosition = Song.exact_time_with_offset()
            }
        lock LOCK_OBJ (fun () ->
            if typing then
                typing_buffered_input <- ValueSome event
            else
                events_buffer <- List.append events_buffer [ event ]

            if action = InputAction.Release then
                if key = Keys.LeftControl || key = Keys.RightControl then ctrl <- false
                elif key = Keys.LeftAlt || key = Keys.RightAlt then alt <- false
                elif key = Keys.LeftShift || key = Keys.RightShift then shift <- false

                held_keys <- Set.remove key held_keys

            elif action = InputAction.Press then
                if key = Keys.LeftControl || key = Keys.RightControl then ctrl <- true
                elif key = Keys.LeftAlt || key = Keys.RightAlt then alt <- true
                elif key = Keys.LeftShift || key = Keys.RightShift then shift <- true

                held_keys <- Set.add key held_keys
        )
    let private key_callback_d = GLFWCallbacks.KeyCallback(key_callback)

    let private mouse_button_callback (_: nativeptr<Window>) (button: MouseButton) (action: InputAction) (modifiers: KeyModifiers) : unit =
        let event =
            {
                Bind = Bind.Mouse button
                Action = action
                GLFWTimestamp = GLFW.GetTime()
                AudioPosition = Song.exact_time_with_offset()
            }
        lock LOCK_OBJ (fun () ->
            events_buffer <- List.append events_buffer [ event ]
            if action = InputAction.Release then
                held_mouse_buttons <- Set.remove button held_mouse_buttons
            elif action = InputAction.Press then
                held_mouse_buttons <- Set.add button held_mouse_buttons
        )
    let private mouse_button_callback_d = GLFWCallbacks.MouseButtonCallback(mouse_button_callback)

    let enable_typing (v: bool) : unit =
        lock LOCK_OBJ (fun () -> typing <- v; typing_buffered_input <- ValueNone)

    let set_cursor_hidden (b: bool) (window: nativeptr<Window>) : unit =
        hide_cursor <- b
        if not b then
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorNormal)
        elif OperatingSystem.IsMacOS() && GLFW.RawMouseMotionSupported() then
            GLFW.SetInputMode(window, RawMouseMotionAttribute.RawMouseMotion, true)
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorDisabled)
        else
            GLFW.SetInputMode(window, CursorStateAttribute.Cursor, CursorModeValue.CursorHidden)

    let init (window: nativeptr<Window>) : unit =
        GLFW.SetInputMode(window, LockKeyModAttribute.LockKeyMods, true);
        GLFW.SetCharCallback(window, char_callback_d) |> ignore
        GLFW.SetKeyCallback(window, key_callback_d) |> ignore
        GLFW.SetMouseButtonCallback(window, mouse_button_callback_d) |> ignore
        GLFW.SetScrollCallback(window, scroll_callback_d) |> ignore
        GLFW.SetCursorPosCallback(window, cursor_pos_callback_d) |> ignore

    let fetch (events_this_frame: InputEvent list byref, this_frame: FrameEvents byref) : unit =
        let a, b =
            lock
                LOCK_OBJ
                (fun () ->
                    let output =
                        {
                            MouseX = mouse_x
                            MouseY = mouse_y
                            MouseZ = mouse_z
                            CursorInBounds = cursor_in_bounds
                            TypedText = typed_text
                            HeldKeys = held_keys
                            HeldMouseButtons = held_mouse_buttons
                            Ctrl = ctrl
                            Shift = shift
                            Alt = alt
                            Time = GLFW.GetTime()
                        },
                        if typing then
                            match typing_buffered_input with
                            | ValueSome event ->
                                if GLFW.GetTime() - event.GLFWTimestamp > 0.002 then
                                    typing_buffered_input <- ValueNone
                                    event :: events_buffer
                                else
                                    events_buffer
                            | _ -> events_buffer
                        else events_buffer

                    events_buffer <- []
                    typed_text <- ""
                    output
                )

        this_frame <- a
        events_this_frame <- b

module Input =

    let mutable last_time_mouse_moved = 0.0
    let mutable last_input_event = 0.0
    let mutable internal this_frame: FrameEvents = Unchecked.defaultof<_>
    let mutable internal last_frame: FrameEvents = Unchecked.defaultof<_>
    let mutable internal this_frame_finished = false

    let mutable internal events_this_frame: InputEvent list = []

    let mutable internal input_listener: InputListener = InputListener.None
    let mutable internal input_listener_mouse_cancel = 0f

    let mutable internal scrolled_this_frame = 0f

    let mutable internal window: nativeptr<Window> = Unchecked.defaultof<_>

    /// Stops listening to text input/next button pressed
    let remove_listener () : unit =
        match input_listener with
        | InputListener.Text(_, on_remove) ->
            InputThread.enable_typing false
            on_remove ()
        | InputListener.Bind _
        | InputListener.None -> ()

        input_listener <- InputListener.None

    /// Used for UIs that let the user type some text into the provided text buffer
    /// In this mode keybindings do not activate (so something bound to A will not fire, it will type 'a' into the buffer instead)
    /// `mouse_cancel` set to true will cause the listener to automatically disconnect itself when the mouse is moved too much
    /// `on_remove` is called when the text listening mode is exited
    let listen_to_text (text: Setting<string>, mouse_cancel: bool, on_remove: unit -> unit) : unit =
        remove_listener ()
        input_listener <- InputListener.Text(text, on_remove)
        InputThread.enable_typing true
        input_listener_mouse_cancel <- if mouse_cancel then 0f else -infinityf

    /// Used for UIs that let the user bind a key to a hotkey
    /// The next key (including modifiers) pressed will be passed to `callback` as a Bind
    /// Then the listener is removed
    let listen_to_next_key (callback: Bind -> unit) : unit =
        remove_listener ()
        input_listener <- InputListener.Bind callback

    let key_held_any_modifiers (key: Keys) : bool =
        this_frame.HeldKeys.Contains key

    let pop_key_any_modifiers (key: Keys, action: InputAction) : (bool * bool * bool) voption =
        let mutable out = ValueNone

        let rec f evs =
            match evs with
            | [] -> []
            | event :: xs when event.Action = action ->
                match event.Bind with
                | Bind.Key (k, modifiers) when k = key ->
                    out <- ValueSome modifiers
                    xs
                | _ -> event :: f xs
            | x :: xs -> x :: f xs

        events_this_frame <- f events_this_frame
        out

    let pop_gameplay (now: Time) (binds: Bind array) (callback: int -> Time -> bool -> unit) : unit =

        let bind_match bind target =
            match bind, target with
            | Bind.Key(k, _), Bind.Key(K, _) when k = K -> true
            | Bind.Mouse b, Bind.Mouse B when b = B -> true
            | _ -> false

        let rec pop_inputs_matching_binds events =
            match events with
            | [] -> []
            | event :: xs when event.Action <> InputAction.Repeat ->
                let mutable i = 0
                let mutable matched = false

                while i < binds.Length && not matched do
                    if bind_match binds.[i] event.Bind then
                        callback i (min now event.AudioPosition) (event.Action <> InputAction.Press)
                        matched <- true

                    i <- i + 1

                if matched then pop_inputs_matching_binds xs else event :: (pop_inputs_matching_binds xs)
            | _ :: xs -> pop_inputs_matching_binds xs

        events_this_frame <- pop_inputs_matching_binds events_this_frame

    let internal pop_matching (bind: Bind, action: InputAction) : bool =
        let mutable found = false

        let rec search events =
            match events with
            | [] -> []
            | event :: xs when event.Bind = bind && event.Action = action ->
                found <- true
                xs
            | x :: xs -> x :: (search xs)

        events_this_frame <- search events_this_frame
        found

    let internal pop_any (action: InputAction) : Bind voption =
        let mutable out = ValueNone

        let rec search events =
            match events with
            | [] -> []
            | event :: xs when event.Action = action ->
                out <- ValueSome(event.Bind)
                xs
            | x :: xs -> x :: (search xs)

        events_this_frame <- search events_this_frame
        out

    let button_pressed_recently () : bool = GLFW.GetTime() - last_input_event < 0.100

    let held (bind: Bind) : bool =
        if this_frame_finished then
            false
        else

        match bind with
        | Bind.Key(Keys.LeftControl, _)
        | Bind.Key(Keys.RightControl, _) -> this_frame.Ctrl
        | Bind.Key(Keys.LeftAlt, _)
        | Bind.Key(Keys.RightAlt, _) -> this_frame.Alt
        | Bind.Key(Keys.LeftShift, _)
        | Bind.Key(Keys.RightShift, _) -> this_frame.Shift
        | Bind.Key(k, m) ->
            this_frame.HeldKeys.Contains k
            && m = (this_frame.Ctrl, this_frame.Alt, this_frame.Shift)
        | Bind.Mouse m -> this_frame.HeldMouseButtons.Contains m
        | Bind.Dummy -> false

    let private DELETE_CHARACTER = Bind.mk Keys.Backspace
    let private DELETE_WORD = Bind.ctrl Keys.Backspace
    let private COPY = Bind.ctrl Keys.C
    let private PASTE = Bind.ctrl Keys.V

    let private update_input_listener () : unit =

        match input_listener with
        | InputListener.Text(text, _) ->

            if this_frame.TypedText <> "" then
                text.Value <- text.Value + this_frame.TypedText

            if text.Value.Length > 0 && (pop_matching(DELETE_CHARACTER, InputAction.Press) || pop_matching(DELETE_CHARACTER, InputAction.Repeat)) then
                Setting.app (fun (x: string) -> x.Substring(0, x.Length - 1)) text

            elif text.Value.Length > 0 && (pop_matching(DELETE_WORD, InputAction.Press) || pop_matching(DELETE_WORD, InputAction.Repeat)) then
                text.Value <-
                    let parts = text.Value.Split(" ")
                    Array.take (parts.Length - 1) parts |> String.concat " "

            elif pop_matching(COPY, InputAction.Press) then
                GLFW.SetClipboardString(window, text.Value)

            elif pop_matching(PASTE, InputAction.Press) then
                text.Value <-
                    text.Value
                    + try
                        GLFW.GetClipboardString(window)
                      with _ ->
                          ""

            if input_listener_mouse_cancel > 200f then
                remove_listener ()

        | InputListener.Bind callback ->
            match pop_any InputAction.Press with
            | ValueSome bind ->
                match bind with
                | Bind.Key(key, _) ->
                    if Bind.IsModifier key then
                        ()
                    else
                        remove_listener ()
                        callback bind
                | _ ->
                    remove_listener ()
                    callback bind
            | ValueNone ->
                match pop_any InputAction.Release with
                | ValueSome bind ->
                    match bind with
                    | Bind.Key(key, _) ->
                        if Bind.IsModifier key then
                            remove_listener ()
                            callback bind
                    | _ -> ()
                | ValueNone -> ()

        | InputListener.None -> ()

    /// Fetches all inputs since last frame from the input polling thread
    /// This makes the data available during the next game frame for UI to query
    let begin_frame_events () : unit =
        last_frame <- this_frame
        InputThread.fetch (&events_this_frame, &this_frame)

        if Object.ReferenceEquals(null, last_frame) then
            last_frame <- this_frame

        if
            (this_frame.MouseX <> last_frame.MouseX)
            || (this_frame.MouseY <> last_frame.MouseY)
        then
            last_time_mouse_moved <- GLFW.GetTime()

        if events_this_frame <> [] then
            last_input_event <- GLFW.GetTime()

        scrolled_this_frame <- this_frame.MouseZ - last_frame.MouseZ
        this_frame_finished <- false
        update_input_listener ()

    let finish_frame_events () : unit =
        input_listener_mouse_cancel <-
            input_listener_mouse_cancel
            + abs (this_frame.MouseX - last_frame.MouseX)
            + abs (this_frame.MouseY - last_frame.MouseY)

        last_frame <- this_frame
        events_this_frame <- []
        this_frame_finished <- true

    let init (_window: nativeptr<Window>) : unit =
        window <- _window
        InputThread.init window

module Mouse =

    let LEFT = MouseButton.Left
    let RIGHT = MouseButton.Right
    let MIDDLE = MouseButton.Middle

    /// X and Y coordinates of the mouse relative to the screen
    /// Position is updated at the start of each frame
    /// Calling multiple times during the same frame will always return the same value
    let pos () : float32 * float32 =
        (Input.this_frame.MouseX, Input.this_frame.MouseY)

    /// False if the mouse cursor is not within the rendered area of the game, when in letterboxing mode
    /// Used to hide the game cursor and restore the real one if your mouse leaves the letterboxing area
    let in_bounds () : bool = Input.this_frame.CursorInBounds

    let x () : float32 = fst (pos ())
    let y () : float32 = snd (pos ())

    /// Get how much the mouse wheel was scrolled since last frame
    /// Once called, the caller is now the **sole owner** of this scrolling and subsequent calls will get 0 until next frame
    /// UI components should only call when they have focus/the mouse is in their bounding box
    /// UI update order = first come first served
    let scroll () : float32 =
        let v = Input.scrolled_this_frame
        Input.scrolled_this_frame <- 0.0f
        v

    let private clicked (mouse_button: MouseButton) : bool =
        Input.pop_matching(Bind.Mouse mouse_button, InputAction.Press)

    let left_clicked () : bool = clicked LEFT
    let right_clicked () : bool = clicked RIGHT
    let middle_clicked () : bool = clicked MIDDLE

    let held (mouse_button: MouseButton) : bool = Input.held (Bind.Mouse mouse_button)

    let released (mouse_button: MouseButton) : bool =
        Input.pop_matching(Bind.Mouse mouse_button, InputAction.Release)

    let moved_recently () : bool = GLFW.GetTime() - Input.last_time_mouse_moved < 0.100

    let hover (r: Rect) : bool =
        not Input.this_frame_finished && r.Contains(pos ())

type Bind with
    member this.Held() =
        match this with
        | Key _
        | Mouse _ -> Input.held this
        | _ -> false

    member this.Repeated() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputAction.Repeat)
        | _ -> false

    member this.PressedOrRepeated() =
        match this with
        | Key _
        | Mouse _ ->
            Input.pop_matching(this, InputAction.Press)
            || Input.pop_matching(this, InputAction.Repeat)
        | _ -> false

    member this.Pressed() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputAction.Press)
        | _ -> false

    member this.Released() =
        match this with
        | Key _
        | Mouse _ -> Input.pop_matching(this, InputAction.Release)
        | _ -> false