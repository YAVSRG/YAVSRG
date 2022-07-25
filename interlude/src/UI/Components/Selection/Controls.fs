namespace Interlude.UI.Components.Selection.Controls

open System
open OpenTK
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers

module Dropdown =

    let ITEMSIZE = 60.0f
    
    type Item(label: string, onclick: unit -> unit) as this =
        inherit Selectable()

        do
            this.Add(Clickable((fun () -> this.Selected <- true), (fun b -> if b then this.Hover <- true), Float = true))
            this.Add(
                TextBox(K label, K (Color.White, Color.Black), 0.0f)
                    .Position { Left = 0.0f %+ 10.0f; Top = 0.0f %+ 5.0f; Right = 1.0f %- 10.0f; Bottom = 1.0f %- 5.0f }
            )

        override this.Draw() =
            if this.Hover then Draw.rect this.Bounds (Style.color(127, 1.0f, 0.4f))
            base.Draw()

        override this.Update(elapsedTime, bounds) =
            if this.Hover && (!|"select").Tapped() then this.Selected <- true
            base.Update(elapsedTime, bounds)

        override this.OnSelect() =
            onclick()
            this.Selected <- false

    type Container(items: (string * (unit -> unit)) seq, onclose: unit -> unit) as this =
        inherit Selectable()

        let fc = FlowSelectable(ITEMSIZE, 0.0f)

        do
            Frame(Color.FromArgb(180, 0, 0, 0), Color.FromArgb(100, 255, 255, 255))
            |> this.Add
            let items = Seq.map (fun (label, action) -> Item(label, fun () -> action(); this.Close())) items |> Array.ofSeq
            this.Add fc
            for i in items do fc.Add i
            fc.Selected <- true

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if 
                this.HoverChild.IsNone
                || Mouse.leftClick()
                || Mouse.rightClick()
            then this.Close()

        member this.Close() =
            onclose()
            this.Destroy()

    let create (items: (string * (unit -> unit)) seq) (onclose: unit -> unit) =
        Container(items, onclose)

    let create_selector (items: 'T seq) (labelFunc: 'T -> string) (selectFunc: 'T -> unit) (onclose: unit -> unit) =
        create (Seq.map (fun item -> (labelFunc item, fun () -> selectFunc item)) items) onclose

type DropdownSelector<'T>(items: 'T array, labelFunc: 'T -> string, setting: Setting<'T>) as this =
    inherit Selectable()

    let mutable dropdown : Dropdown.Container option = None
    
    do
        this.Add(new TextBox((fun () -> labelFunc setting.Value), K (Color.White, Color.Black), 0.0f))
        this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
        this.Add(new Clickable((fun () -> if not this.Selected then this.Selected <- true), fun b -> if b then this.Hover <- true))

    override this.OnSelect() =
        let d = Dropdown.create_selector items labelFunc setting.Set (fun () -> this.Selected <- false)
        dropdown <- Some d
        d.Position { Left = 0.0f %+ 0.0f; Top = 1.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ (Dropdown.ITEMSIZE * float32 (min 3 items.Length)) }
        |> this.Add
        base.OnSelect()

    override this.OnDeselect() =
        dropdown.Value.Destroy()
        dropdown <- None
        base.OnDeselect()

    static member FromEnum(setting: Setting<'T>) =
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        DropdownSelector(values, (fun x -> Enum.GetName(typeof<'T>, x)), setting)

type NoteColorPicker(color: Setting<byte>) as this =
    inherit StaticContainer(NodeType.Leaf)

    let sprite = Content.getTexture "note"
    let n = byte sprite.Rows

    let fd() = Setting.app (fun x -> (x + n - 1uy) % n) color
    let bk() = Setting.app (fun x -> (x + 1uy) % n) color

    do 
        this
        |* Percyqaz.Flux.UI.Clickable((fun () -> (if not this.Selected then this.Select()); fd ()), OnHover = fun b -> if b then this.Focus())

    override this.Draw() =
        base.Draw()
        if this.Selected then Draw.rect this.Bounds (Style.color(180, 1.0f, 0.5f))
        elif this.Focused then Draw.rect this.Bounds (Style.color(120, 1.0f, 0.8f))
        Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf Color.White) (Sprite.gridUV (3, int color.Value) sprite)

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        
        if this.Selected then
            if (!|"up").Tapped() then fd()
            elif (!|"down").Tapped() then bk()
            elif (!|"left").Tapped() then bk()
            elif (!|"right").Tapped() then fd()