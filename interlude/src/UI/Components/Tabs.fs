namespace Interlude.UI.Components

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI

module Tabs =

    type private TabButton(name, isOpen, onClick) =
        inherit Button(name, onClick, "none")

        override this.Draw() =
            Draw.rect this.Bounds (if isOpen() then !*Palette.SELECTED else !*Palette.HOVER)
            base.Draw()

    type Container() as this =
        inherit StaticWidget(NodeType.Switch(fun _ -> this.WhoIsSelected()))
        let mutable selectedItem = None
    
        let TABHEIGHT = 60.0f
        let TABWIDTH = 250.0f

        let buttons = FlowContainer.LeftToRight(TABWIDTH, Position = Position.SliceTop TABHEIGHT)

        let init_tabs = ResizeArray<Widget>()

        member private this.WhoIsSelected() = selectedItem.Value
    
        member this.AddTab(name, widget: Widget) =
            buttons
            |* TabButton(name, (fun() -> match selectedItem with Some x -> x = widget | None -> false), fun () -> selectedItem <- Some widget)

            match selectedItem with
            | None -> selectedItem <- Some widget
            | _ -> ()

            widget.Position <- Position.TrimTop TABHEIGHT

            if this.Initialised then widget.Init this
            else init_tabs.Add widget
    
        member this.WithTab(name, widget) =
            this.AddTab(name, widget); this
    
        override this.Draw() =
            buttons.Draw()
            selectedItem.Value.Draw()
    
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            buttons.Update(elapsedTime, moved)
            selectedItem.Value.Update(elapsedTime, moved)

        override this.Init(parent: Widget) =
            base.Init parent
            buttons.Init this
            for tab in init_tabs do tab.Init this