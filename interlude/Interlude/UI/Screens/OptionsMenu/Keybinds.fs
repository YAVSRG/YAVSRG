namespace Interlude.UI.OptionsMenu

open Prelude.Common
open Interlude.Graphics
open Interlude.Options
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module Keybinds = 

    let icon = "⌨"
    let page() : SelectionPage = 
        let keycount = Setting.simple options.KeymodePreference.Value
        
        let f k i =
            Setting.make
                (fun v -> options.GameplayBinds.[k - 3].[i] <- v)
                (fun () -> options.GameplayBinds.[k - 3].[i])

        let binds, refreshBinds =
            refreshRow
                (fun () -> int keycount.Value)
                (fun i k ->
                    let x = -60.0f * float32 k
                    let n = float32 i
                    { new KeyBinder(f (int keycount.Value) i, false) with
                        override this.OnDeselect() =
                            base.OnDeselect()
                            if i + 1 < k then
                                match this.SParent.Value with
                                | :? ListSelectable as s -> s.Synchronized(fun () -> if s.Selected && s.HoverChild.IsSome then s.Next(); s.HoverChild.Value.Selected <- true)
                                | _ -> failwith "impossible"
                    }
                    |> positionWidget(x + 120.0f * n, 0.5f, 0.0f, 0.0f, x + 120.0f * n + 120.0f, 0.5f, 0.0f, 1.0f))
                    
        {
            Content = fun add ->
                column [
                    PrettySetting("Keymode", Selector.FromEnum<Keymode>(keycount |> Setting.trigger (ignore >> refreshBinds))).Position(200.0f)
                    PrettySetting("GameplayBinds", binds).Position(280.0f, Render.vwidth - 200.0f, 120.0f)
                    PrettyButton("Hotkeys", ignore).Position(400.0f)
                ] :> Selectable
            Callback = ignore
        }