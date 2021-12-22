namespace Interlude.UI.OptionsMenu

open System.Drawing
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
        

    let icon = "⌨"
    let page() : SelectionPage = 
        let keycount = Setting.simple options.KeymodePreference.Value

        let binds = GameplayKeybinder(keycount)
                    
        {
            Content = fun add ->
                column [
                    PrettySetting("Keymode", Selector.FromEnum<Keymode>(keycount |> Setting.trigger (ignore >> binds.OnKeymodeChanged))).Position(200.0f)
                    PrettySetting("GameplayBinds", binds).Position(280.0f, Render.vwidth - 200.0f)
                    PrettyButton("Hotkeys", ignore).Position(400.0f)
                ] :> Selectable
            Callback = ignore
        }