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
    let mutable private inputmethod: ISettable<string> list = []

    let internal freeIM = List.isEmpty inputmethod

    let init(game : GameWindow) =
        game.MouseWheel.Add(fun e -> mousescroll <- e.Delta)
        game.MouseMove.Add(fun e -> mousex <- float32 e.X; mousey <- float32 e.Y) //todo: rescale mouse position by virtual pixels rather than window pixels
        game.MouseDown.Add(fun e -> mouse.Add(e.Button))
        game.MouseUp.Add(fun e -> while mouse.Remove(e.Button) do ())

        game.KeyDown.Add(fun e -> keys.Add(e.Key))
        game.KeyPress.Add(
            fun e ->
                match List.tryHead inputmethod with
                | Some s -> s.Set(s.Get() + e.KeyChar.ToString())
                | None -> ())
        game.KeyUp.Add(fun e -> while keys.Remove(e.Key) do ())

    //todo: consider threading input and redesigning this system to use events instead
    //these methods are internal until further notice as the Bind system should be used instead
    module Keyboard =
        let internal pressedOverride(k) = keys.Contains(k)
        let internal pressed(k, overrideIM) = (overrideIM || freeIM) && pressedOverride(k)
        //was key pressed this frame?
        let internal tappedOverride(k) = keys.Contains(k) && not(oldKeys.Contains(k))
        let internal tapped(k, overrideIM) = (overrideIM || freeIM) && tappedOverride(k)
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
        inputmethod <- List.tail inputmethod

module Keyboard = Input.Keyboard

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
            Input.clickhandled <- true
            Input.mouse.Contains(b) && not(Input.oldMouse.Contains(b))
    let internal pressed(b) = Input.mouse.Contains(b)
    let internal release(b) = Input.oldMouse.Contains(b) && not(Input.mouse.Contains(b))
    let Hover(struct (l, t, r, b): Interlude.Render.Rect) = let x, y = X(), Y() in x > l && x < r && y > t && y < b

type Bind =
    | Dummy
    | Key of Key
    | Mouse of MouseButton
    | Shift of Bind
    | Alt of Bind //reserved for hard coded behaviours. users should not be able to create alt binds in settings
    | Ctrl of Bind
    | Joystick of unit //NYI
    with
        override this.ToString() =
            match this with
            | Dummy -> "DUMMY"
            | Key k -> k.ToString()
            | Mouse m -> "M"+m.ToString()
            | Shift b -> "Shift + " + b.ToString()
            | Alt b -> "Alt + " + b.ToString()
            | Ctrl b -> "Ctrl + " + b.ToString()
            | Joystick _ -> "nyi"
        member this.Pressed(overrideIM) =
            match this with
            | Dummy -> false
            | Key k -> Keyboard.pressed(k, overrideIM)
            | Mouse m -> Mouse.pressed(m)
            | Shift b -> (Keyboard.pressedOverride(Key.LShift) || Keyboard.pressedOverride(Key.RShift)) && b.Pressed(overrideIM)
            | Alt b -> (Keyboard.pressedOverride(Key.LAlt) || Keyboard.pressedOverride(Key.RAlt)) && b.Pressed(overrideIM)
            | Ctrl b -> (Keyboard.pressedOverride(Key.LControl) || Keyboard.pressedOverride(Key.RControl)) && b.Pressed(overrideIM)
            | _ -> false
        member this.Tapped(overrideIM) =
            match this with
            | Dummy -> false
            | Key k -> Keyboard.tapped(k, overrideIM)
            | Mouse m -> Mouse.Click(m)
            | Shift b -> (Keyboard.pressedOverride(Key.LShift) || Keyboard.pressedOverride(Key.RShift)) && b.Tapped(overrideIM)
            | Alt b -> (Keyboard.pressedOverride(Key.LAlt) || Keyboard.pressedOverride(Key.RAlt)) && b.Tapped(overrideIM)
            | Ctrl b -> (Keyboard.pressedOverride(Key.LControl) || Keyboard.pressedOverride(Key.RControl)) && b.Tapped(overrideIM)
            | _ -> false
        //note that for the sake of completeness released should take an im override too, but there is no context where it matters
        //todo: maybe put it in later
        member this.Released() =
            match this with
            | Dummy -> false
            | Key k -> Keyboard.released(k)
            | Mouse m -> Mouse.release(m)
            | Shift b | Alt b | Ctrl b -> b.Released()
            | _ -> false
        static member DummyBind = new Setting<Bind>(Dummy)