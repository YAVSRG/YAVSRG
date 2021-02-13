namespace Interlude.Input

open System
open OpenTK
open OpenTK.Windowing.Desktop
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude.Render
open System.Collections.Generic

module Input = 
    let internal keys = new List<Keys>()
    let internal oldKeys = new List<Keys>()
    let internal mouse = new List<MouseButton>()
    let internal oldMouse = new List<MouseButton>()

    let mutable internal mousex = 0.f
    let mutable internal mousey = 0.f
    let mutable internal mousez = 0.f
    let mutable internal oldmousez = 0.f
    let mutable internal clickhandled = false
    let mutable private keybindGrabber = ignore
    let mutable private inputmethod: ISettable<string> list = []

    let internal freeIM() = List.isEmpty inputmethod

    let init(game : GameWindow) =
        game.add_MouseWheel(fun e -> mousez <- e.OffsetY)
        game.add_MouseMove(
            fun e ->
                mousex <- Math.Clamp(Render.vwidth / float32 Render.rwidth * float32 e.X, 0.0f, Render.vwidth)
                mousey <- Math.Clamp(Render.vheight / float32 Render.rheight * float32 e.Y, 0.0f, Render.vheight))
        game.add_MouseDown(fun e -> mouse.Add(e.Button))
        game.add_MouseUp(fun e -> while mouse.Remove(e.Button) do ())

        game.add_KeyDown(fun e ->
            keys.Add(e.Key)
            if e.Key <> Keys.LeftControl && e.Key <> Keys.RightControl && e.Key <> Keys.LeftShift && e.Key <> Keys.RightShift && e.Key <> Keys.LeftAlt && e.Key <> Keys.RightAlt then
                keybindGrabber(e.Key)
                keybindGrabber <- ignore)
        game.add_TextInput(fun e ->
            match List.tryHead inputmethod with
            | Some s -> s.Set(s.Get() + e.AsString)
            | None -> ())
        game.add_KeyUp(fun e -> while keys.Remove(e.Key) do ())

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
            if Keyboard.tappedOverride(Keys.Backspace) && s.Get().Length > 0 then
                if Keyboard.pressedOverride(Keys.LeftControl) then s.Set("") else
                    let v = s.Get()
                    s.Set(v.Substring(0, v.Length - 1))
            //todo: clipboard support
        | None -> ()
        oldKeys.Clear()
        oldKeys.AddRange(keys)
        oldMouse.Clear()
        oldMouse.AddRange(mouse)
        clickhandled <- false
        oldmousez <- mousez

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
    let Scroll() = let v = Input.mousez - Input.oldmousez in Input.oldmousez <- Input.mousez; v
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
    | Key of Keys * modifiers:(bool * bool * bool)
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
            ctrl = (Keyboard.pressedOverride(Keys.LeftControl) || Keyboard.pressedOverride(Keys.RightControl))
            && shift = (Keyboard.pressedOverride(Keys.LeftShift) || Keyboard.pressedOverride(Keys.RightShift))
            && alt = (Keyboard.pressedOverride(Keys.LeftAlt) || Keyboard.pressedOverride(Keys.RightAlt))
        static member DummyBind = new Setting<Bind>(Dummy)

module Bind =
    let inline mk k = Key (k, (false, false, false))
    let inline ctrl k = Key (k, (true, false, false))
    let inline alt k = Key (k, (false, true, false))
    let inline shift k = Key (k, (false, false, true))
    let inline ctrlShift k = Key (k, (true, false, true))
    let inline ctrlAlt k = Key (k, (true, true, false))