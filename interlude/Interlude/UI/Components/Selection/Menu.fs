namespace Interlude.UI.Components.Selection.Menu

open System
open System.Drawing
open OpenTK
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Buttons

type SelectionPage =
    {
        Content: (string * SelectionPage -> unit) -> Selectable
        Callback: unit -> unit
    }

[<AutoOpen>]
module Helpers =
    let localiseOption s = Localisation.localise("options.name." + s)
    let localiseTooltip s = Localisation.localise("options.tooltip." + s)

    let row xs =
        let r = ListSelectable(true)
        List.iter r.Add xs; r

    let column xs =
        let c = ListSelectable(false)
        List.iter c.Add xs; c

    let refreshRow number cons =
        let r = ListSelectable(true)
        let refresh() =
            r.Clear()
            let n = number()
            for i in 0 .. (n - 1) do
                r.Add(cons i n)
        refresh()
        r, refresh

    let refreshChoice (options: string array) (widgets: Widget array array) (setting: Setting<int>) =
        let rec newSetting =
            {
                Set =
                    fun x ->
                        for w in widgets.[setting.Value] do if w.Parent.IsSome then selector.SParent.Value.SParent.Value.Remove w
                        for w in widgets.[x] do selector.SParent.Value.SParent.Value.Add w
                        setting.Value <- x
                Get = setting.Get
                Config = setting.Config
            }
        and selector : Selector = new Selector(options, newSetting)
        selector.Synchronized(fun () -> newSetting.Value <- newSetting.Value)
        selector

    let PRETTYTEXTWIDTH = 500.0f
    let PRETTYHEIGHT = 80.0f
    let PRETTYWIDTH = 1200.0f

type Divider() =
    inherit Widget()

    member this.Position(y) =
        this |> positionWidget(100.0f, 0.0f, y - 5.0f, 0.0f, 100.0f + PRETTYWIDTH, 0.0f, y + 5.0f, 0.0f)

    override this.Draw() =
        base.Draw()
        Draw.quad (Quad.ofRect this.Bounds) (struct(Color.White, Color.FromArgb(0, 255, 255, 255), Color.FromArgb(0, 255, 255, 255), Color.White)) Sprite.DefaultQuad

type PrettySetting(name, widget: Selectable) as this =
    inherit Selectable()

    let mutable widget = widget

    do
        widget
        |> positionWidgetA(PRETTYTEXTWIDTH, 0.0f, 0.0f, 0.0f)
        |> this.Add

        TextBox(K (localiseOption name + ":"), (fun () -> ((if this.Selected then Style.accentShade(255, 1.0f, 0.2f) else Color.White), Color.Black)), 0.0f)
        |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, PRETTYTEXTWIDTH, 0.0f, PRETTYHEIGHT, 0.0f)
        |> this.Add

        TooltipRegion(localiseTooltip name) |> this.Add
    
    member this.Position(y, width, height) =
        this |> positionWidget(100.0f, 0.0f, y, 0.0f, 100.0f + width, 0.0f, y + height, 0.0f)
    
    member this.Position(y, width) = this.Position(y, width, PRETTYHEIGHT)
    member this.Position(y) = this.Position(y, PRETTYWIDTH)

    override this.Draw() =
        if this.Selected then Draw.rect this.Bounds (Color.FromArgb(180, 0, 0, 0)) Sprite.Default
        elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 0, 0, 0)) Sprite.Default
        base.Draw()
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if widget.Hover && not widget.Selected && this.Selected then this.HoverChild <- None; this.Hover <- true
        
    override this.OnSelect() = if not widget.Hover then widget.Selected <- true
    override this.OnDehover() = base.OnDehover(); widget.OnDehover()

    member this.Refresh(w: Selectable) =
        widget.Destroy()
        widget <- w
        this.Add(widget |> positionWidgetA(PRETTYTEXTWIDTH, 0.0f, 0.0f, 0.0f))

type PrettyButton(name, action) as this =
    inherit Selectable()
    do
        TextBox(K (localiseOption name + "  >"), (fun () -> ((if this.Hover then Style.accentShade(255, 1.0f, 0.5f) else Color.White), Color.Black)), 0.0f) |> this.Add
        Clickable((fun () -> this.Selected <- true), (fun b -> if b then this.Hover <- true)) |> this.Add
        TooltipRegion(localiseTooltip name) |> this.Add
    override this.OnSelect() = action(); this.Selected <- false
    override this.Draw() =
        if this.Hover then Draw.rect this.Bounds (Color.FromArgb(120, 0, 0, 0)) Sprite.Default
        base.Draw()
    member this.Position(y) = this |> positionWidget(100.0f, 0.0f, y, 0.0f, 100.0f + PRETTYWIDTH, 0.0f, y + PRETTYHEIGHT, 0.0f)

type SelectionMenu(topLevel: SelectionPage) as this =
    inherit Dialog()
    
    let stack: (Selectable * (unit -> unit)) option array = Array.create 12 None
    let mutable namestack = []
    let mutable name = ""
    let body = Widget()

    let wrapper main =
        let mutable disposed = false
        let w = 
            { new Selectable() with

                override this.Update(elapsedTime, bounds) =
                    if disposed then this.HoverChild <- None
                    base.Update(elapsedTime, bounds)
                    if not disposed then
                        Input.absorbAll()

                override this.VisibleBounds = this.Bounds
                override this.Dispose() = base.Dispose(); disposed <- true
            }
        w.Add main
        w.SelectedChild <- Some main
        w
    
    let rec add (label, page) =
        let n = List.length namestack
        namestack <- localiseOption label :: namestack
        name <- String.Join(" > ", List.rev namestack)
        let w = wrapper (page.Content add)
        match stack.[n] with
        | None -> ()
        | Some (x, _) -> x.Destroy()
        stack.[n] <- Some (w, page.Callback)
        body.Add w
        let n = float32 n + 1.0f
        w.Reposition(0.0f, Render.vheight * n, 0.0f, Render.vheight * n)
        body.Move(0.0f, -Render.vheight * n, 0.0f, -Render.vheight * n)
    
    let back() =
        namestack <- List.tail namestack
        name <- String.Join(" > ", List.rev namestack)
        let n = List.length namestack
        let (w, callback) = stack.[n].Value in w.Dispose(); callback()
        let n = float32 n
        body.Move(0.0f, -Render.vheight * n, 0.0f, -Render.vheight * n)
    
    do
        this.Add body
        TextBox((fun () -> name), K (Color.White, Color.Black), 0.0f)
        |> positionWidget(20.0f, 0.0f, 20.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        |> this.Add
        add ("Options", topLevel)
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        match List.length namestack with
        | 0 -> this.BeginClose()
        | n -> if (fst stack.[n - 1].Value).SelectedChild.IsNone then back()
    
    override this.OnClose() = ()

type ConfirmDialog(prompt, callback: unit -> unit) as this =
    inherit Dialog()

    let mutable confirm = false

    let options =
        row [ 
            LittleButton(K "Yes", fun () ->  this.BeginClose(); confirm <- true)
            |> position (WPos.leftSlice 200.0f);
            LittleButton(K "No", this.BeginClose)
            |> position (WPos.rightSlice 200.0f)
        ]

    do
        TextBox(K prompt, K (Color.White, Color.Black), 0.5f)
        |> positionWidget(200.0f, 0.0f, -200.0f, 0.5f, -200.0f, 1.0f, -50.0f, 0.5f)
        |> this.Add
        options.OnSelect()
        options
        |> positionWidget(-300.0f, 0.5f, 0.0f, 0.5f, 300.0f, 0.5f, 100.0f, 0.5f)
        |> this.Add

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if not options.Selected then this.BeginClose()

    override this.OnClose() = if confirm then callback()