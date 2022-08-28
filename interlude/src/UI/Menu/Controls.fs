namespace Interlude.UI.Menu

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components

type Divider() =
    inherit StaticWidget(NodeType.None)

    member this.Pos(y) =
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y - 5.0f, PRETTYWIDTH, 10.0f)
        this

    override this.Draw() =
        Draw.quad (Quad.ofRect this.Bounds) (struct(Color.White, Color.FromArgb(0, 255, 255, 255), Color.FromArgb(0, 255, 255, 255), Color.White)) Sprite.DefaultQuad

type PrettySetting(name, widget: Widget) as this =
    inherit StaticContainer(NodeType.Switch (fun _ -> this.Child))

    let mutable widget = widget

    do
        this
        |+ Text(
            K (N name + ":"),
            Color = (fun () -> ((if this.Selected then Style.color(255, 1.0f, 0.2f) else Color.White), Color.Black)),
            Align = Alignment.LEFT,
            Position = Position.Box(0.0f, 0.0f, PRETTYTEXTWIDTH, PRETTYHEIGHT).Margin(Style.padding))
        |* TooltipRegion(T name)

    member this.Child
        with get() = widget
        and set(w: Widget) =
            let old_widget = widget
            widget <- w
            w.Position <- Position.TrimLeft(PRETTYTEXTWIDTH).Margin(Style.padding)
            if this.Initialised then 
                w.Init this
                if old_widget.Focused then w.Focus()
    
    member this.Pos(y, width, height) =
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y, width, height) 
        this
    
    member this.Pos(y, width) = this.Pos(y, width, PRETTYHEIGHT)
    member this.Pos(y) = this.Pos(y, PRETTYWIDTH)

    override this.Init(parent) =
        base.Init parent
        widget.Position <- Position.TrimLeft(PRETTYTEXTWIDTH).Margin(Style.padding)
        widget.Init this

    override this.Draw() =
        if widget.Selected then Draw.rect this.Bounds (!*Palette.SELECTED)
        elif widget.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        base.Draw()
        widget.Draw()
    
    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        widget.Update(elapsedTime, moved)

type PrettyButton(name, action) as this =
    inherit StaticContainer(NodeType.Button (fun _ -> if this.Enabled then action()))

    member val Icon = "" with get, set

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (if this.Icon <> "" then sprintf "%s %s  >" this.Icon (N name) else sprintf "%s  >" (N name)),
            Color = ( 
                fun () -> 
                    if this.Enabled then
                        ( (if this.Focused then Style.color(255, 1.0f, 0.5f) else Color.White), Color.Black )
                    else (Color.Gray, Color.Black)
            ),
            Align = Alignment.LEFT,
            Position = Position.Margin(Style.padding))
        |+ Clickable(this.Select, OnHover = fun b -> if b then this.Focus())
        |* TooltipRegion(T name)
        base.Init parent

    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        base.Draw()

    member this.Pos(y) = 
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y, PRETTYWIDTH, PRETTYHEIGHT)
        this

    member val Enabled = true with get, set
    
    static member Once(name, action) =
        let mutable ref = Unchecked.defaultof<PrettyButton>
        let button = PrettyButton(name, fun () ->
                if ref.Enabled then action()
                ref.Enabled <- false
            )
        ref <- button
        button

    static member Once(name, action, notifText, notifType) =
        let mutable ref = Unchecked.defaultof<PrettyButton>
        let button = PrettyButton(name, fun () ->
                if ref.Enabled then action(); Notifications.add (notifText, notifType)
                ref.Enabled <- false
            )
        ref <- button
        button

type CaseSelector(name: string, cases: string array, controls: Widget array array, setting: Setting<int>) as this =
    inherit StaticWidget(NodeType.Switch(fun _ -> this._selector()))
    
    let selector = PrettySetting(name, Selector<int>(Array.indexed cases, setting)).Pos(200.0f)

    member this._selector() = selector

     member private this.WhoIsFocused : int option =
        if selector.Focused then Some -1
        else Seq.tryFindIndex (fun (c: Widget) -> c.Focused) controls.[setting.Value]

    member this.Previous() =
        match this.WhoIsFocused with
        | Some n ->
            let current_controls = controls.[setting.Value]
            if n = -1 then
                current_controls.[current_controls.Length - 1].Focus()
            elif n = 0 then selector.Focus()
            else current_controls.[n - 1].Focus()
        | None -> ()

    member this.Next() =
        match this.WhoIsFocused with
        | Some n ->
            let current_controls = controls.[setting.Value]
            if n = -1 then
                current_controls.[0].Focus()
            elif n = current_controls.Length - 1 then selector.Focus()
            else current_controls.[n + 1].Focus()
        | None -> ()

    member this.SelectFocusedChild() =
        match this.WhoIsFocused with
        | Some -1 -> selector.Select()
        | Some n -> controls.[setting.Value].[n].Select()
        | None -> ()

    override this.Draw() =
        let current_controls = controls.[setting.Value]
        selector.Draw()
        for c in current_controls do
            c.Draw()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if moved then
            selector.Update(elapsedTime, true)
            for case in controls do
                for control in case do
                    control.Update(elapsedTime, true)
        else
            selector.Update(elapsedTime, false)
            for control in controls.[setting.Value] do
                control.Update(elapsedTime, false)

        if this.Focused then
            if (!|"up").Tapped() then this.Previous()
            elif (!|"down").Tapped() then this.Next()
            elif (!|"select").Tapped() then this.SelectFocusedChild()

    override this.Init(parent: Widget) =
        base.Init parent
        selector.Init this
        for case in controls do
            for control in case do
                 control.Init this