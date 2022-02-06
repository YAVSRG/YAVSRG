namespace Interlude.UI.Screens.LevelSelect

open Prelude.Common
open Prelude.Gameplay.Mods
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers
open Interlude.UI.Components.Selection.Buttons
open Interlude.UI.Components.Selection.Menu
open Interlude.Utils
open Interlude.Gameplay
open Interlude.Options

module private ModSelect =

    let page() =
        {
            Content = fun add ->
                let select = FlowSelectable(75.0f, 5.0f)
                CardButton(
                    ModState.getModName "auto",
                    ModState.getModDesc "auto",
                    (fun () -> autoplay),
                    fun () -> autoplay <- not autoplay)
                |> select.Add
                for name in modList.Keys do
                    CardButton(
                        ModState.getModName name,
                        ModState.getModDesc name,
                        (fun () -> selectedMods.Value.ContainsKey name),
                        fun () -> Setting.app (ModState.cycleState name) selectedMods
                    )
                    |> select.Add
                select.Reposition(50.0f, 0.0f, 200.0f, 0.0f, 650.0f, 0.0f, -50.0f, 1.0f)
                select :> Selectable
            Callback = ignore
        }
    
type ModSelect() as this =
    inherit Widget()

    do StylishButton ((fun () -> SelectionMenu(N"mods", ModSelect.page()).Show()), K "Mods", (fun () -> Style.accentShade(100, 0.8f, 0.2f)), options.Hotkeys.Mods) |> this.Add

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if options.Hotkeys.Autoplay.Value.Tapped() then
            autoplay <- not autoplay