namespace Interlude.Features.OptionsMenu

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.LevelSelect

module OptionsMenuRoot =

    type private TileButton(body: Callout, onclick: unit -> unit) =
        inherit StaticContainer(NodeType.Button (onclick))
    
        let body_width, body_height = Callout.measure body
    
        member val Disabled = false with get, set
        member val Margin = (0.0f, 20.0f) with get, set
        member this.Height = body_height + snd this.Margin * 2.0f
    
        override this.Init(parent) =
            this |* Clickable.Focus(this)
            base.Init(parent)
    
        override this.Draw() =
            let color = 
                if this.Disabled then Colors.shadow_1
                elif this.Focused then Colors.pink_accent
                else Colors.shadow_1
            Draw.rect this.Bounds color.O3
            Draw.rect (this.Bounds.Expand(0.0f, 5.0f).SliceBottom(5.0f)) color
            Callout.draw (this.Bounds.Left + fst this.Margin, this.Bounds.Top + snd this.Margin, body_width, body_height, Colors.text, body)
    
    type OptionsPage() as this =
        inherit Page()

        let button_size =
            let _, h = Callout.measure (Callout.Normal.Title("Example"))
            h + 40.0f

        let tooltip_hint = 
            Callout.Normal
                .Icon(Icons.info)
                .Title(L"options.ingame_help.name")
                .Body(L"options.ingame_help.hint")
                .Hotkey("tooltip")

        do
            let _, h = Callout.measure tooltip_hint

            this.Content(
                GridContainer(button_size, 3,
                    Spacing = (50.0f, h + 120.0f),
                    Position = 
                        { 
                            Left = 0.0f %+ 200.0f
                            Right = 1.0f %- 200.0f
                            Top = 0.5f %- (60.0f + h * 0.5f + button_size)
                            Bottom = 0.5f %+ (60.0f + h * 0.5f + button_size) })
                |+ TileButton(Callout.Normal
                        .Icon(Icons.gameplay)
                        .Title(L"gameplay.name"),
                    fun () -> Gameplay.GameplayPage().Show())
                |+ TileButton(Callout.Normal
                        .Icon(Icons.themes)
                        .Title(L"noteskins.name"),
                    fun () -> Noteskins.NoteskinsPage().Show())
                |+ TileButton(Callout.Normal
                        .Icon(Icons.mods)
                        .Title(L"hud.name"),
                    fun () -> HUD.EditHUDPage().Show())
                |+ TileButton(Callout.Normal
                        .Icon(Icons.system)
                        .Title(L"system.name"),
                    fun () -> System.SystemPage().Show())
                |+ TileButton(Callout.Normal
                        .Icon(Icons.heart)
                        .Title(L"advanced.name"),
                    ignore)
                |+ TileButton(Callout.Normal
                        .Icon(Icons.debug)
                        .Title(L"debug.name"),
                    fun () -> Debug.DebugPage().Show())
            )
            this |* Callout.frame (tooltip_hint) 
                ( fun (w, h) -> { Left = 0.0f %+ 200.0f; Right = 1.0f %- 200.0f; Top = 0.5f %- (20.0f + h * 0.5f); Bottom = 0.5f %+ (20.0f + h * 0.5f) } )

        override this.Title = L"options.name"
        override this.OnClose() = LevelSelect.refresh_all()

    let show() = Menu.ShowPage OptionsPage