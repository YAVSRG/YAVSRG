namespace Interlude.UI

open Prelude.Common
open Interlude.Options
open Interlude.Render
open Interlude
open Interlude.Utils
open Interlude.Options
open Interlude.UI.Components
open OpenTK
open FSharp.Reflection
open System.Collections.Generic

[<AbstractClass>]
type ISelectionWheelItem() =
    inherit Widget()

    let mutable selected = false
    member this.Selected = selected

    abstract member Select: unit -> unit
    default this.Select() = selected <- true
    abstract member Deselect: unit -> unit
    default this.Deselect() = selected <- false

type SelectionWheel() as this  =
    inherit ISelectionWheelItem()

    let WIDTH = 500.0f

    let mutable index = 0
    let items = new List<ISelectionWheelItem>()
    let collapse = new Animation.AnimationFade(1.0f)
    do this.Animation.Add(collapse)
    override this.Select() = base.Select(); collapse.SetTarget(0.0f)
    override this.Deselect() = base.Deselect(); collapse.SetTarget(1.0f)

    override this.Add(w) = failwith "don't use this, use AddItem"
    member this.AddItem(w) = items.Add(w); w.AddTo(this)

    override this.Draw() =
        let o = WIDTH * collapse.Value
        let struct (left, top, right, bottom) = this.Bounds
        Draw.rect(this.Bounds |> Rect.sliceLeft(WIDTH - o))(Color.FromArgb(180, 30, 30, 30))(Sprite.Default)
        Draw.rect(this.Bounds |> Rect.sliceLeft(WIDTH - o) |> Rect.sliceRight(5.0f))(Color.White)(Sprite.Default)
        let mutable t = top
        for i in 0 .. (items.Count - 1) do
            let w = items.[i]
            let h = w.Bounds |> Rect.height
            if index = i then
                Draw.quad
                    (Rect.create (left - o) t (left + WIDTH - o) (t + h) |> Quad.ofRect)
                    (Color.FromArgb(255,180,180,180), Color.FromArgb(0,180,180,180), Color.FromArgb(0,180,180,180), Color.FromArgb(255,180,180,180))
                    Sprite.DefaultQuad
            w.Draw()
            t <- t + h

    override this.Update(elapsedTime, bounds) =
        let struct (left, _, right, bottom) = bounds
        base.Update(elapsedTime, struct (left, 0.0f, right, bottom))
        let o = WIDTH * collapse.Value
        let struct (left, _, _, bottom) = this.Bounds
        let mutable flag = true
        let mutable t = 0.0f
        for i in 0 .. (items.Count-1) do
            let w = items.[i]
            if w.Selected then flag <- false
            w.Update(elapsedTime, Rect.create (left - o) t (left + WIDTH - o) bottom)
            let h = w.Bounds |> Rect.height
            t <- t + h
        if flag && this.Selected then
            if options.Hotkeys.Select.Get().Tapped(false) then items.[index].Select()
            elif options.Hotkeys.Exit.Get().Tapped(false) then this.Deselect()
            elif options.Hotkeys.Next.Get().Tapped(false) then index <- (index + 1) % items.Count
            elif options.Hotkeys.Previous.Get().Tapped(false) then index <- (index + items.Count - 1) % items.Count

module SelectionWheel =

    type DummyItem(name) as this =
        inherit ISelectionWheelItem()
        do
            this.Add(new TextBox(K name, (fun () -> if this.Selected then Color.Yellow else Color.White), 0.5f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f)
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected && options.Hotkeys.Exit.Get().Tapped(false) then this.Deselect()

    let fromRecord<'T>(record: 'T) =
        let t = typeof<'T>
        let fields = FSharpType.GetRecordFields(t)
        let sw = new SelectionWheel()
        fields |> Array.iter (fun f -> sw.AddItem(new DummyItem(f.Name)))
        sw

type ConfigEditor<'T>(data: 'T) as this =
    inherit FlowContainer()

    do
        let typeName = data.GetType().Name
        let fields = FSharpType.GetRecordFields(data.GetType())
        Array.choose (
            fun p ->
                let value = FSharpValue.GetRecordField(data, p)
                match value with
                | :? FloatSetting as s -> new Slider<float>(s, Localisation.localise(typeName + ".name." + p.Name)) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 80.0f, 0.0f) |> Some
                | :? IntSetting as s -> new Slider<int>(s, Localisation.localise(typeName + ".name." + p.Name)) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 80.0f, 0.0f) |> Some
                | :? Setting<bool> as s -> Selector.FromBool(s, Localisation.localise(typeName + ".name." + p.Name)) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 80.0f, 0.0f) |> Some
                //| :? Setting<'S> as s -> Selector.FromEnum(s, localise(typeName + ".name." + p.Name)) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 80.0f, 0.0f) |> Some
                | _ -> None
            ) fields
        |> Array.iter this.Add
        this.Reposition(100.0f, 40.0f, -100.0f, -40.0f)

type OptionsMenu() as this =
    inherit Dialog()
    let sw = SelectionWheel.fromRecord(options)
    do  
        sw.Select()
        this.Add(sw)
    override this.OnClose() = ()
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if not sw.Selected then this.Close()