namespace Interlude.Input

open OpenTK
open OpenTK.Input
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

    let init(game : GameWindow) =
        game.MouseWheel.Add(fun e -> mousescroll <- e.Delta)
        game.MouseMove.Add(fun e -> mousex <- float32 e.X; mousey <- float32 e.Y) //todo: rescale mouse position by virtual pixels rather than window pixels
        game.MouseDown.Add(fun e -> mouse.Add(e.Button))
        game.MouseUp.Add(fun e -> while mouse.Remove(e.Button) do ())

        game.KeyDown.Add(fun e -> keys.Add(e.Key))
        game.KeyUp.Add(fun e -> while keys.Remove(e.Key) do ())

    let update() =
        oldKeys.Clear()
        oldKeys.AddRange(keys)
        oldMouse.Clear()
        oldMouse.AddRange(mouse)
        clickhandled <- false
        mousescroll <- 0

//todo: consider threading input and redesigning this system to use events instead
//these methods are internal until further notice as the Bind system should be used instead
module Keyboard =
    let internal pressed(k) = Input.keys.Contains(k)
    //was key pressed this frame?
    let internal tapped(k) = Input.keys.Contains(k) && not(Input.oldKeys.Contains(k))
    //was key released this frame?
    let internal released(k) = Input.oldKeys.Contains(k) && not(Input.keys.Contains(k))

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
    let internal released(b) = Input.oldMouse.Contains(b) && not(Input.mouse.Contains(b))

type Bind =
    | Key of Key
    | Mouse of MouseButton
    | Shift of Bind
    | Alt of Bind //reserved for hard coded behaviours. users should not be able to create alt binds in settings
    | Ctrl of Bind
    | Joystick of unit //NYI
    with
        override this.ToString() =
            match this with
            | Key k -> k.ToString()
            | Mouse m -> "M"+m.ToString()
            | Shift b -> "Shift + " + b.ToString()
            | Alt b -> "Alt + " + b.ToString()
            | Ctrl b -> "Ctrl + " + b.ToString()
            | Joystick _ -> "nyi"
        member this.Pressed() =
            match this with
            | Key k -> Keyboard.pressed(k)
            | Mouse m -> Mouse.pressed(m)
            | Shift b -> (Keyboard.pressed(Key.LShift) || Keyboard.pressed(Key.RShift)) && b.Pressed()
            | Alt b -> (Keyboard.pressed(Key.LControl) || Keyboard.pressed(Key.RControl)) && b.Pressed()
            | Ctrl b -> (Keyboard.pressed(Key.LAlt) || Keyboard.pressed(Key.RAlt)) && b.Pressed()
            | _ -> false
        member this.Tapped() =
            match this with
            | Key k -> Keyboard.tapped(k)
            | Mouse m -> Mouse.Click(m)
            | Shift b -> (Keyboard.pressed(Key.LShift) || Keyboard.pressed(Key.RShift)) && b.Tapped()
            | Alt b -> (Keyboard.pressed(Key.LControl) || Keyboard.pressed(Key.RControl)) && b.Tapped()
            | Ctrl b -> (Keyboard.pressed(Key.LAlt) || Keyboard.pressed(Key.RAlt)) && b.Tapped()
            | _ -> false
        member this.Released() =
            match this with
            | Key k -> Keyboard.released(k)
            | Mouse m -> Mouse.released(m)
            | Shift b | Alt b | Ctrl b -> b.Released()
            | _ -> false