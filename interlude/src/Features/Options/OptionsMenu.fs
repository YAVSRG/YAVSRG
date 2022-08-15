namespace Interlude.Features.OptionsMenu

open System.Drawing
open Percyqaz.Flux.UI
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components.Selection.Menu
open Interlude.Features.LevelSelect

module OptionsMenuRoot =

    type private BigButton(label: string, icon: string, onClick) as this =
        inherit Frame(NodeType.Button onClick)

        do
            this.Fill <- fun () -> Style.color(180, 0.9f, 0.0f)
            this.Border <- fun () -> if this.Focused then Color.White else Color.Transparent
            this
            |+ Text(icon, Position = { Left = Position.min; Top = 0.05f %+ 0.0f; Right = Position.max; Bottom = 0.7f %+ 0.0f })
            |+ Text(label, Position = { Left = Position.min; Top = 0.65f %+ 0.0f; Right = Position.max; Bottom = 0.9f %+ 0.0f })
            |* Clickable(this.Select, OnHover = fun b -> if b then this.Focus())

    type OptionsPage() as this =
        inherit Page()

        do
            this.Content(
                row()
                |+ BigButton( N"system", Icons.system, (fun () -> Menu.ShowPage System.SystemPage),
                    Position = Position.Box(0.5f, 0.5f, -790.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"themes", Icons.themes, (fun () -> Menu.ShowPage Themes.ThemesPage),
                    Position = Position.Box(0.5f, 0.5f, -470.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"gameplay", Icons.gameplay, (fun () -> Menu.ShowPage Gameplay.GameplayPage),
                    Position = Position.Box(0.5f, 0.5f, -150.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"keybinds", Icons.binds, (fun () -> Menu.ShowPage Keybinds.KeybindsPage),
                    Position = Position.Box(0.5f, 0.5f, 170.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"debug", Icons.debug, (fun () -> Menu.ShowPage Debug.DebugPage),
                    Position = Position.Box(0.5f, 0.5f, 490.0f, -150.0f, 300.0f, 300.0f) )
            )
        override this.Title = L"options.name"
        override this.OnClose() = LevelSelect.refresh <- true

    let show() = Menu.ShowPage OptionsPage