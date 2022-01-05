namespace Interlude.UI.OptionsMenu

open System.Drawing
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Screens.LevelSelect
open Interlude.UI.Components.Selection.Buttons
open Interlude.UI.Components.Selection.Menu

module OptionsMenuRoot =

    type private BigButton(label, icon, onClick) as this =
        inherit ButtonBase(onClick)
        do
            this.Add(Frame((fun () -> Style.accentShade(180, 0.9f, 0.0f)), (fun () -> if this.Hover then Color.White else Color.Transparent)))
            this.Add(TextBox(K icon, K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.05f, 0.0f, 1.0f, 0.0f, 0.7f))
            this.Add(TextBox(K label, K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.65f, 0.0f, 1.0f, 0.0f, 0.9f))

    let page() : SelectionPage =
        {
            Content = fun add ->
                row [
                    BigButton(localiseOption "System", Icons.system, fun () -> add ( "System", System.page () ))
                    |> positionWidget(-790.0f, 0.5f, -150.0f, 0.5f, -490.0f, 0.5f, 150.0f, 0.5f);

                    BigButton(localiseOption "Themes", Icons.themes, fun () -> add ( "Themes", Themes.page () ))
                    |> positionWidget(-470.0f, 0.5f, -150.0f, 0.5f, -170.0f, 0.5f, 150.0f, 0.5f);

                    BigButton(localiseOption "Gameplay", Icons.gameplay, fun () -> add ( "Gameplay", Gameplay.page () ))
                    |> positionWidget(-150.0f, 0.5f, -150.0f, 0.5f, 150.0f, 0.5f, 150.0f, 0.5f);

                    BigButton(localiseOption "Keybinds", Icons.binds, fun () -> add ( "Keybinds", Keybinds.page () ))
                    |> positionWidget(170.0f, 0.5f, -150.0f, 0.5f, 470.0f, 0.5f, 150.0f, 0.5f);

                    BigButton(localiseOption "Debug", Icons.debug, fun () -> add ( "Debug", Debug.page () ))
                    |> positionWidget(490.0f, 0.5f, -150.0f, 0.5f, 790.0f, 0.5f, 150.0f, 0.5f);
                ] :> Selectable
            Callback = fun () -> LevelSelect.refresh <- true
        }

    let show() = SelectionMenu(page()).Show()