namespace Interlude.Features.OptionsMenu

open System.Drawing
open Percyqaz.Flux.UI
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
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

        let system =
            GoodButton(
                Callout.Normal
                    .Icon(Icons.system)
                    .Title(L"options.system.name"),
                fun () -> Menu.ShowPage System.SystemPage)

        let gameplay =
            GoodButton(
                Callout.Normal
                    .Icon(Icons.gameplay)
                    .Title(L"options.gameplay.name"),
                fun () -> Menu.ShowPage Gameplay.GameplayPage)
                
        let themes =
            GoodButton(
                Callout.Normal
                    .Icon(Icons.themes)
                    .Title(L"options.themes.name"),
                fun () -> Menu.ShowPage Themes.ThemesPage)

        do
            this.Content(
                GridContainer(1, 3,
                    Spacing = (50.0f, 0.0f),
                    Position = Position.SliceTop(300.0f).SliceBottom(system.Height).Margin(100.0f, 0.0f))
                |+ system
                |+ gameplay
                |+ themes
            )
        override this.Title = L"options.name"
        override this.OnClose() = LevelSelect.refresh <- true

    let show() = Menu.ShowPage OptionsPage