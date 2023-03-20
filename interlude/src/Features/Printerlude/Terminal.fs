namespace Interlude.Features.Printerlude

open System
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Options
open Interlude.UI

module Terminal =

    let private lockObj = obj()

    let mutable exec_command = fun (c: string) -> ()

    module Log = 
        let mutable private pos = 0
        let mutable private log : ResizeArray<string> = ResizeArray()
        let upKey = Bind.mk Keys.PageUp
        let downKey = Bind.mk Keys.PageDown
        let homeKey = Bind.mk Keys.End

        let mutable visible : string seq = []

        let LINEWIDTH = 113

        let add(s: string) =
            lock lockObj (fun () ->
                try
                    for line in s.Split('\n', StringSplitOptions.RemoveEmptyEntries) do
                        let mutable line = line
                        while line.Length > LINEWIDTH do
                            let split =
                                match line.Substring(0, LINEWIDTH).LastIndexOf(' ') with
                                | -1 -> LINEWIDTH
                                | 0 -> LINEWIDTH
                                | n -> n
                            log.Insert(0, line.Substring(0, split))
                            line <- line.Substring(split)

                        log.Insert(0, line)

                    visible <- Seq.skip pos log
                with x -> printfn "%O" x
            )

        let up() =
            if log.Count - 15 > pos then
                pos <- pos + 5
                visible <- Seq.skip pos log

        let down() =
            if pos - 5 >= 0 then
                pos <- pos - 5
                visible <- Seq.skip pos log

        let home() =
            pos <- 0
            visible <- log

        let clear() =
            log.Clear()
            home()

    let add_message(s: string) = Log.add s

    let private currentLine = Setting.simple ""
    let private sendKey = Bind.mk Keys.Enter

    module private History =
        let mutable private pos = -1
        let mutable private history : string list = []
        let upKey = Bind.mk Keys.Up
        let downKey = Bind.mk Keys.Down

        let up() = 
            if history.Length - 1 > pos then
                pos <- pos + 1
                currentLine.Value <- history.[pos]
        let down() = 
            if pos > 0 then
                pos <- pos - 1
                currentLine.Value <- history.[pos]
        let add(l) =
            history <- l :: history
            pos <- -1

    let mutable shown = false

    let private hide() = 
        shown <- false
        Input.removeInputMethod()

    let private show() =
        shown <- true
        let rec addInput() = Input.setTextInput (currentLine, fun () -> if shown then sync addInput)
        addInput()

    let dropfile(path: string) =
        let path = path.Replace("""\""", """\\""")
        let v = currentLine.Value
        if v.Length = 0 || Char.IsWhiteSpace(v.[v.Length - 1]) then
            currentLine.Set (sprintf "%s\"%s\"" v path)
        else currentLine.Set (sprintf "%s \"%s\"" v path)

    let font = lazy ( Fonts.create "Courier Prime Sans" |> fun x -> x.SpaceWidth <- 0.5f; x )

    let draw() =
        if not shown then ()
        else

        let bounds = Viewport.bounds.Shrink(100.0f)

        Draw.rect (bounds.Expand 5.0f) (Colors.white.O2)
        Draw.rect (bounds.TrimBottom 70.0f) (Colors.shadow_1.O3)
        Draw.rect (bounds.SliceBottom 65.0f) (Colors.shadow_2.O3)
        Text.drawB(font.Value, ">  " + currentLine.Value, 30.0f, bounds.Left + 20.0f, bounds.Bottom - 50.0f, Colors.text)
        
        lock lockObj (fun () -> 
            for i, line in Seq.indexed Log.visible do
                if i < 19 then
                    Text.drawB(font.Value, line, 20.0f, bounds.Left + 20.0f, bounds.Bottom - 60.0f - 60.0f - 40f * float32 i, Colors.text)
        )

    let update() =
        if shown && (!|"exit").Tapped() then hide()
        if 
            options.EnableConsole.Value
            && not shown
            && Screen.currentType <> Screen.Type.Play
            && (!|"console").Tapped()
        then show()

        if not shown then ()
        else
        
        lock lockObj (fun () -> 
            if sendKey.Tapped() && currentLine.Value <> "" then
                exec_command currentLine.Value
                History.add currentLine.Value
                Log.home()
                currentLine.Value <- ""
            elif History.upKey.Tapped() then History.up()
            elif History.downKey.Tapped() then History.down()
            elif Log.upKey.Tapped() then Log.up()
            elif Log.downKey.Tapped() then Log.down()
            elif Log.homeKey.Tapped() then Log.home()
        )