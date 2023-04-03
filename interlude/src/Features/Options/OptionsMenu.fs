namespace Interlude.Features.OptionsMenu

open System.Drawing
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.LevelSelect

module OptionsMenuRoot =

    type private TileButton(body: Callout, onclick: unit -> unit) =
        inherit StaticContainer(NodeType.Button (onclick))
    
        let body_height = snd <| Callout.measure body
    
        member val Disabled = false with get, set
        member val Margin = (0.0f, 20.0f) with get, set
        member this.Height = body_height + snd this.Margin * 2.0f
    
        override this.Init(parent) =
            this |* Clickable.Focus(this)
            base.Init(parent)
    
        override this.Draw() =
            let color, dark = 
                if this.Disabled then Colors.shadow_1, false
                elif this.Focused then Colors.pink_accent, false
                else Colors.shadow_1, false
            Draw.rect this.Bounds (Color.FromArgb(180, color))
            Draw.rect (this.Bounds.Expand(0.0f, 5.0f).SliceBottom(5.0f)) color
            Callout.draw (this.Bounds.Left + fst this.Margin, this.Bounds.Top + snd this.Margin, body_height, Colors.text, body)
    
    type OptionsPage() as this =
        inherit Page()

        let system =
            TileButton(
                Callout.Normal
                    .Icon(Icons.system)
                    .Title(L"system.name"),
                fun () -> Menu.ShowPage System.SystemPage)

        let gameplay =
            TileButton(
                Callout.Normal
                    .Icon(Icons.gameplay)
                    .Title(L"gameplay.name"),
                fun () -> Menu.ShowPage Gameplay.GameplayPage)
                
        let themes =
            TileButton(
                Callout.Normal
                    .Icon(Icons.themes)
                    .Title(L"themes.name"),
                fun () -> Menu.ShowPage Themes.ThemesPage)
                
        let debug =
            TileButton(
                Callout.Normal
                    .Icon(Icons.debug)
                    .Title(L"debug.name"),
                fun () -> Menu.ShowPage Debug.DebugPage)

        do
            this.Content(
                GridContainer(1, 4,
                    Spacing = (50.0f, 0.0f),
                    Position = Position.SliceTop(400.0f).SliceBottom(system.Height).Margin(200.0f, 0.0f))
                |+ system
                |+ gameplay
                |+ themes
                |+ debug
            )
        override this.Title = L"options.name"
        override this.OnClose() = LevelSelect.refresh <- true

    let show() = Menu.ShowPage OptionsPage