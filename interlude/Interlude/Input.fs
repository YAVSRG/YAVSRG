namespace Interlude.Input

open OpenTK
open OpenTK.Input
open Prelude.Common
open System.Collections.Generic

module Input = 
    let internal keys = new List<Key>()
    let internal oldKeys = new List<Key>()
    let internal mouse = new List<MouseButton>()
    let internal oldMouse = new List<MouseButton>()

    let mutable internal mousex = 0.f
    let mutable internal mousey = 0.f
    let mutable internal mousescroll = 0
    let mutable internal clickhandled = false
    let mutable private keybindGrabber = ignore
    let mutable private inputmethod: ISettable<string> list = []

    let internal freeIM() = List.isEmpty inputmethod

    let init(game : GameWindow) =
        game.MouseWheel.Add(fun e -> mousescroll <- e.Delta)
        game.MouseMove.Add(fun e -> mousex <- float32 e.X; mousey <- float32 e.Y) //todo: rescale mouse position by virtual pixels rather than window pixels
        game.MouseDown.Add(fun e -> mouse.Add(e.Button))
        game.MouseUp.Add(fun e -> while mouse.Remove(e.Button) do ())

        game.KeyDown.Add(fun e -> 
            keys.Add(e.Key)
            if e.Key <> Key.ControlLeft && e.Key <> Key.ControlRight && e.Key <> Key.ShiftLeft && e.Key <> Key.ShiftRight && e.Key <> Key.AltLeft && e.Key <> Key.AltRight then
                keybindGrabber(e.Key)
                keybindGrabber <- ignore)
        game.KeyPress.Add(
            fun e ->
                match List.tryHead inputmethod with
                | Some s -> s.Set(s.Get() + e.KeyChar.ToString())
                | None -> ())
        game.KeyUp.Add(fun e -> while keys.Remove(e.Key) do ())

    //todo: threaded input
    //ideas on how: f# list of binds that happened this frame including timestamps; list is read by game update loop with an event listener system and wiped
    //use struct DUs and struct tuples to represent binds for immutability/thread safety

    //these methods are internal until further notice as the Bind system should be used instead
    module Keyboard =
        let internal pressedOverride(k) = keys.Contains(k)
        let internal pressed(k, overrideIM) = (overrideIM || freeIM()) && pressedOverride(k)
        //was key pressed this frame?
        let internal tappedOverride(k) = keys.Contains(k) && not(oldKeys.Contains(k))
        let internal tapped(k, overrideIM) = (overrideIM || freeIM()) && tappedOverride(k)
        //was key released this frame?
        let internal released(k) = oldKeys.Contains(k) && not(keys.Contains(k))

    let update() =
        match List.tryHead inputmethod with
        |  Some s ->
            if Keyboard.tappedOverride(Key.BackSpace) && s.Get().Length > 0 then
                if Keyboard.pressedOverride(Key.LControl) then s.Set("") else
                    let v = s.Get()
                    s.Set(v.Substring(0, v.Length - 1))
            //todo: clipboard support
        | None -> ()
        oldKeys.Clear()
        oldKeys.AddRange(keys)
        oldMouse.Clear()
        oldMouse.AddRange(mouse)
        clickhandled <- false
        mousescroll <- 0

    let createInputMethod(s: ISettable<string>) =
        inputmethod <- s :: inputmethod

    let removeInputMethod() =
        inputmethod <- match inputmethod with x::xs -> xs | [] -> []

    let grabKey(callback) = 
        keybindGrabber <- callback

module Mouse = 
    let X() = Input.mousex
    let Y() = Input.mousey
    //retrieving scroll value sets it to zero so all other queries do not receieve it
    //this means the top level widget (highest priority at the time) grabs scrolling before anything else
    let Scroll() = let v = Input.mousescroll in Input.mousescroll <- 0; v
    //same idea. once a click is grabbed it only fires the click behaviour of one widget
    //for this reason, when checking if something has been clicked on it is important to check if the mouse is in the right area BEFORE checking if it has been clicked
    let Click(b) =
        if Input.clickhandled then false else
            Input.clickhandled <- Input.mouse.Contains(b) && not(Input.oldMouse.Contains(b))
            Input.clickhandled
    let internal pressed(b) = Input.mouse.Contains(b)
    let internal release(b) = Input.oldMouse.Contains(b) && not(Input.mouse.Contains(b))
    let Hover(struct (l, t, r, b): Interlude.Render.Rect) = let x, y = X(), Y() in x > l && x < r && y > t && y < b

module Keyboard = Input.Keyboard

type Bind =
    | Dummy
    | Key of Key * modifiers:(bool * bool * bool)
    | Mouse of MouseButton * modifiers:(bool * bool * bool)
    | Joystick of unit //NYI
    with
        override this.ToString() =
            match this with
            | Dummy -> "NONE"
            | Key (k, m) -> Bind.ModifierString(m) + k.ToString()
            | Mouse (b, m) -> Bind.ModifierString(m) + "M"+b.ToString()
            | Joystick _ -> "nyi"
        member this.Pressed(overrideIM) =
            match this with
            | Dummy -> false
            | Key (k, m) -> Bind.ChkModifiers(m) && Keyboard.pressed(k, overrideIM)
            | Mouse (b, m) -> Bind.ChkModifiers(m) && Mouse.pressed(b)
            | _ -> false
        member this.Tapped(overrideIM) =
            match this with
            | Dummy -> false
            | Key (k, m) -> Bind.ChkModifiers(m) && Keyboard.tapped(k, overrideIM)
            | Mouse (b, m) -> Bind.ChkModifiers(m) && Mouse.Click(b)
            | _ -> false
        //note that for the sake of completeness released should take an im override too, but there is no context where it matters
        //todo: maybe put it in later
        member this.Released() =
            match this with
            | Dummy -> false
            | Key (k, m) -> Keyboard.released(k)
            | Mouse (b, m) -> Mouse.release(b)
            | _ -> false
        static member private ModifierString((ctrl, alt, shift)) =
            (if ctrl then "Ctrl + " else "")
            + (if alt then "Alt + " else "")
            + (if shift then "Shift + " else "")
        static member private ChkModifiers((ctrl, alt, shift)) =
            ctrl = (Keyboard.pressedOverride(Key.LControl) || Keyboard.pressedOverride(Key.RControl))
            && shift = (Keyboard.pressedOverride(Key.LShift) || Keyboard.pressedOverride(Key.RShift))
            && alt = (Keyboard.pressedOverride(Key.LAlt) || Keyboard.pressedOverride(Key.RAlt))
        static member DummyBind = new Setting<Bind>(Dummy)

module Bind =
    let inline mk k = Key (k, (false, false, false))
    let inline ctrl k = Key (k, (true, false, false))
    let inline alt k = Key (k, (false, true, false))
    let inline shift k = Key (k, (false, false, true))
    let inline ctrlShift k = Key (k, (true, false, true))
    let inline ctrlAlt k = Key (k, (true, true, false))