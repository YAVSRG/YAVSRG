namespace Interlude.UI.Toolbar

open System
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.Graphics
open Interlude.Input
open Interlude.UI

module Terminal =

    let mutable exec_command = fun (c: string) -> ()

    module Log = 
        let mutable private pos = 0
        let mutable private log : string list = []
        let upKey = Bind.mk Keys.PageUp
        let downKey = Bind.mk Keys.PageDown
        let homeKey = Bind.mk Keys.End

        let mutable visible : string list = []

        let add(s: string) =
            match s.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray with
            | [] -> ()
            | xs -> log <- List.rev xs @ log; visible <- List.skip pos log

        let up() =
            if log.Length - 15 > pos then
                pos <- pos + 5
                visible <- List.skip pos log

        let down() =
            if pos - 5 >= 0 then
                pos <- pos - 5
                visible <- List.skip pos log

        let home() =
            pos <- 0
            visible <- log

    let add_message(s: string) = Log.add s

    let private line = Setting.simple ""
    let private sendKey = Bind.mk Keys.Enter

    module private History =
        let mutable private pos = -1
        let mutable private history : string list = []
        let upKey = Bind.mk Keys.Up
        let downKey = Bind.mk Keys.Down

        let up() = 
            if history.Length - 1 > pos then
                pos <- pos + 1
                line.Value <- history.[pos]
        let down() = 
            if pos > 0 then
                pos <- pos - 1
                line.Value <- history.[pos]
        let add(l) =
            history <- l :: history
            pos <- -1

    let mutable private shown = false

    let private hide() = 
        shown <- false
        Input.removeInputMethod()

    let private show() =
        shown <- true
        let rec addInput() = Input.setTextInput (line, fun () -> if shown then Screen.globalAnimation.Add(Animation.AnimationAction(addInput)))
        addInput()

    let font = lazy ( Fonts.create "Courier Prime Sans" )

    let draw() =
        if not shown then ()
        else

        let bounds = Rect.expand (-100.0f, -100.0f) Render.bounds
        let struct (l, t, r, b) = bounds

        Draw.rect (bounds |> Rect.expand (5.0f, 5.0f)) (Color.FromArgb(127, 255, 255, 255)) Sprite.Default
        Draw.rect (bounds |> Rect.trimBottom 70.0f) (Color.FromArgb(200, 0, 0, 0)) Sprite.Default
        Draw.rect (bounds |> Rect.sliceBottom 65.0f) (Color.FromArgb(255, 0, 0, 0)) Sprite.Default
        Text.drawB(font.Value, ">  " + line.Value, 30.0f, l + 20.0f, b - 50.0f, (Color.White, Color.Black))

        for i, line in List.indexed Log.visible do
            if i < 19 then
                Text.drawB(font.Value, line, 20.0f, l + 20.0f, b - 60.0f - 60.0f - 40f * float32 i, (Color.White, Color.Black))

    let update() =
        if shown && Options.options.Hotkeys.Exit.Value.Tapped() then hide()
        if 
            Options.options.EnableConsole.Value
            && not shown
            && Screen.currentType <> Screen.Type.Play
            && Options.options.Hotkeys.Console.Value.Tapped()
        then show()

        if not shown then ()
        else

        if sendKey.Tapped() && line.Value <> "" then
            exec_command line.Value
            History.add line.Value
            Log.home()
            line.Value <- ""
        elif History.upKey.Tapped() then History.up()
        elif History.downKey.Tapped() then History.down()
        elif Log.upKey.Tapped() then Log.up()
        elif Log.downKey.Tapped() then Log.down()
        elif Log.homeKey.Tapped() then Log.home()