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
            this.Add(TextBox(K icon, K (Color.White, Color.Black), 0.5f).Position { Left = 0.0f %+ 0.0f; Top = 0.05f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 0.7f %+ 0.0f })
            this.Add(TextBox(K label, K (Color.White, Color.Black), 0.5f).Position { Left = 0.0f %+ 0.0f; Top = 0.65f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 0.9f %+ 0.0f })

    let page() : SelectionPage =
        {
            Content = fun add ->
                row [
                    BigButton( N"system", Icons.system, fun () -> add ( N"system", System.page () ))
                        .Position ( Position.Box(0.5f, 0.5f, -790.0f, -150.0f, 300.0f, 300.0f) )

                    BigButton( N"themes", Icons.themes, fun () -> add ( N"themes", Themes.page () ))
                        .Position ( Position.Box(0.5f, 0.5f, -470.0f, -150.0f, 300.0f, 300.0f) )

                    BigButton( N"gameplay", Icons.gameplay, fun () -> add ( N"gameplay", Gameplay.page () ))
                        .Position ( Position.Box(0.5f, 0.5f, -150.0f, -150.0f, 300.0f, 300.0f) )

                    BigButton( N"keybinds", Icons.binds, fun () -> add ( N"keybinds", Keybinds.page () ))
                        .Position ( Position.Box(0.5f, 0.5f, 170.0f, -150.0f, 300.0f, 300.0f) )

                    BigButton( N"debug", Icons.debug, fun () -> add ( N"debug", Debug.page () ))
                        .Position ( Position.Box(0.5f, 0.5f, 490.0f, -150.0f, 300.0f, 300.0f) )
                ] :> Selectable
            Callback = fun () -> LevelSelect.refresh <- true
        }

    let show() = SelectionMenu(L"options.name", page()).Show()