namespace Interlude.UI.OptionsMenu

open System.Drawing
open Percyqaz.Flux.UI
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Screens.LevelSelect
open Interlude.UI.Components.Selection.Menu

module OptionsMenuRoot =

    type private BigButton(label: string, icon: string, onClick) as this =
        inherit Frame(NodeType.Leaf)

        do
            this.Fill <- fun () -> Style.accentShade(180, 0.9f, 0.0f)
            this.Border <- fun () -> if this.Focused then Color.White else Color.Transparent
            this
            |+ Text(icon, Position = { Left = Position.min; Top = 0.05f %+ 0.0f; Right = Position.max; Bottom = 0.7f %+ 0.0f })
            |* Text(label, Position = { Left = Position.min; Top = 0.65f %+ 0.0f; Right = Position.max; Bottom = 0.9f %+ 0.0f })
        
        override this.OnSelected() =
            base.OnSelected()
            onClick()

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if this.Selected then this.Focus()

    type OptionsPage(m) as this =
        inherit Page(m)

        do
            this.Content(
                row()
                |+ BigButton( N"system", Icons.system, (fun () -> m.ChangePage System.SystemPage),
                        Position = Percyqaz.Flux.UI.Position.Box(0.5f, 0.5f, -790.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"themes", Icons.themes, (fun () -> m.ChangePage Themes.ThemesPage),
                        Position = Percyqaz.Flux.UI.Position.Box(0.5f, 0.5f, -470.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"gameplay", Icons.gameplay, (fun () -> m.ChangePage Gameplay.GameplayPage),
                        Position = Percyqaz.Flux.UI.Position.Box(0.5f, 0.5f, -150.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"keybinds", Icons.binds, (fun () -> m.ChangePage Keybinds.KeybindsPage),
                        Position = Percyqaz.Flux.UI.Position.Box(0.5f, 0.5f, 170.0f, -150.0f, 300.0f, 300.0f) )
                |+ BigButton( N"debug", Icons.debug, (fun () -> m.ChangePage Debug.DebugPage),
                        Position = Percyqaz.Flux.UI.Position.Box(0.5f, 0.5f, 490.0f, -150.0f, 300.0f, 300.0f) )
            )
        override this.Title = L"options.name"
        override this.OnClose() = LevelSelect.refresh <- true

    let show() = Menu(fun m -> OptionsPage(m)).Show()