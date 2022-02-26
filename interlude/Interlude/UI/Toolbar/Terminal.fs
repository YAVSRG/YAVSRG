namespace Interlude.UI.Toolbar

open System
open Prelude.Common
open Interlude
open Interlude.Graphics
open Interlude.Input
open Interlude.UI

module Terminal =

    let mutable exec_command = fun (c: string) -> ()

    let mutable private log : string list = []

    let add_message(s: string) = 
        match s.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray with
        | [] -> ()
        | xs -> log <- List.rev xs @ log

    let private line = Setting.simple ""

    let private sendKey = Key (OpenTK.Windowing.GraphicsLibraryFramework.Keys.Enter, (false, false, false))

    let mutable private shown = false

    let private hide() = 
        shown <- false
        Input.removeInputMethod()

    let private show() =
        shown <- true
        let rec addInput() = Input.setTextInput (line, fun () -> if shown then Screen.globalAnimation.Add(Animation.AnimationAction(addInput)))
        addInput()

    let draw() =
        if not shown then ()
        else

        let struct (l, t, r, b) = Render.bounds
        Draw.rect (Render.bounds |> Rect.expand (-10.0f, -10.0f)) (Color.FromArgb(127, 255, 255, 255)) Sprite.Default
        Draw.rect (Render.bounds |> Rect.trimBottom 100.0f |> Rect.expand (-20.0f, -20.0f)) (Color.FromArgb(200, 0, 0, 0)) Sprite.Default
        Draw.rect (Render.bounds |> Rect.sliceBottom 100.0f |> Rect.expand (-20.0f, -20.0f)) (Color.FromArgb(200, 0, 0, 0)) Sprite.Default
        Text.drawB(Content.font, line.Value, 30.0f, l + 20.0f, b - 60.0f, (Color.White, Color.Black))

        for i, line in List.indexed log do
            if i < 20 then
                Text.drawB(Content.font, line, 30.0f, l + 20.0f, b - 200.0f - 40f * float32 i, (Color.White, Color.Black))

    let update() =
        if shown && Options.options.Hotkeys.Exit.Value.Tapped() then hide()
        if not shown && Options.options.Hotkeys.Console.Value.Tapped() then show()

        if not shown then ()
        else

        if sendKey.Tapped() then
            exec_command line.Value
            line.Value <- ""