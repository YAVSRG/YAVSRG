namespace Interlude.UI.OptionsMenu

open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude.Graphics
open Interlude.Options
open Interlude.Input
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module Keybinds = 

    type GameplayKeybinder(keymode: Setting<Keymode>) as this =
        inherit Selectable()

        let mutable progress = 0

        let mutable text = options.GameplayBinds.[int keymode.Value - 3] |> Seq.map (sprintf "%O") |> String.concat ",  "
        let refreshText() : unit =
            let binds = options.GameplayBinds.[int keymode.Value - 3]
            if not this.Selected then
                text <- binds |> Seq.map (sprintf "%O") |> String.concat ",  "
            else
                text <- ""
                for i = 0 to progress - 1 do
                    text <- text + binds.[i].ToString() + ",  "
                text <- text + "..."

        let rec inputCallback(b) =
            let binds = options.GameplayBinds.[int keymode.Value - 3]
            match b with
            | Key (k, _) ->
                binds.[progress] <- Key (k, (false, false, false))
                progress <- progress + 1
                if progress = int keymode.Value then this.Selected <- false
                else Input.grabNextEvent inputCallback
                refreshText()
            | _ -> Input.grabNextEvent inputCallback

        do
            TextBox(
                (fun () -> text),
                (fun () -> (if this.Selected then Style.accentShade(255, 1.0f, 0.5f) else Color.White), Color.Black),
                0.0f)
            |> this.Add

            Clickable(
                (fun () -> if not this.Selected then this.Selected <- true),
                fun b -> if b then this.Hover <- true)
            |> this.Add

        override this.OnSelect() =
            base.OnSelect()
            progress <- 0
            refreshText()
            Input.grabNextEvent inputCallback

        override this.OnDeselect() =
            base.OnDeselect()
            Input.removeInputMethod()
            text <- options.GameplayBinds.[int keymode.Value - 3] |> Seq.map (sprintf "%O") |> String.concat ",  "

        member this.OnKeymodeChanged() = refreshText()

    type KeyBinder(hotkey: Hotkey) as this =
        inherit Selectable()
        do
            TextBox((fun () -> (!|hotkey).ToString()), (fun () -> (if this.Selected then Style.accentShade(255, 1.0f, 0.0f) else Color.White), Color.Black), 0.0f)
            |> positionWidgetA(20.0f, 0.0f, 0.0f, 0.0f)
            |> this.Add
            Clickable((fun () -> if not this.Selected then this.Selected <- true), fun b -> if b then this.Hover <- true)
            |> this.Add

        let set = fun v -> options.Hotkeys.[hotkey] <- v
    
        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.5f)) Sprite.Default
            elif this.Hover then Draw.rect this.Bounds (Style.accentShade(120, 1.0f, 0.8f)) Sprite.Default
            Draw.rect (Rect.expand(0.0f, -40.0f) this.Bounds) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default
            base.Draw()
    
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected then
                match Input.consumeAny InputEvType.Press with
                | ValueNone -> ()
                | ValueSome b ->
                    match b with
                    | Key (k, (ctrl, _, shift)) ->
                        if k = Keys.Escape then set Dummy
                        else set (Key (k, (ctrl, false, shift)))
                        this.Selected <- false
                    | _ -> ()

    let hotkeysPage() : SelectionPage =
        let container = FlowSelectable(80.0f, 10.0f)
        for o in System.Enum.GetValues typeof<Hotkey> do
            let h = o :?> Hotkey
            if h <> Hotkey.NONE then
                container.Add( PrettySetting("hotkeys." + h.ToString().ToLower(), KeyBinder h) )
        container.Reposition(100.0f, 200.0f, -100.0f, 0.0f)
        {
            Content = fun add -> container
            Callback = ignore
        }
        
    let page() : SelectionPage = 
        let keycount = Setting.simple options.KeymodePreference.Value

        let binds = GameplayKeybinder(keycount)
                    
        {
            Content = fun add ->
                column [
                    PrettySetting("generic.keymode", Selector<Keymode>.FromEnum(keycount |> Setting.trigger (ignore >> binds.OnKeymodeChanged))).Position(200.0f)
                    PrettySetting("keybinds.gameplay", binds).Position(280.0f, Render.vwidth - 200.0f)
                    PrettyButton("keybinds.hotkeys", (fun () -> add (N"keybinds.hotkeys", hotkeysPage()))).Position(400.0f)
                ]
            Callback = ignore
        }