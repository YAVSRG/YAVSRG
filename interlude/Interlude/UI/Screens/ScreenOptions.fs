namespace Interlude.UI

open Prelude.Common
open Interlude.Options
open Interlude.Render
open Interlude
open Interlude.UI.Components
open OpenTK
open FSharp.Reflection
open System.Collections.Generic

[<AbstractClass>]
type ISelectionWheelItem() =
    inherit Widget()

    let mutable selected = false

    abstract member Left: unit -> unit
    abstract member Right: unit -> unit
    abstract member Select: unit -> unit
    abstract member Deselect: unit -> unit


type SelectionWheel() =
    inherit Widget()

    let index = 0
    let items = new List<string * Widget>()
    member this.Add(name, w) =
        items.Add((name, w))

    override this.Draw() =
        Draw.rect(this.Bounds |> Rect.sliceLeft(100.0f))(Color.Black)(Sprite.Default)
        let struct (left, top, right, bottom) = this.Bounds
        for i in 0..(items.Count-1) do
            let (n, w) = items.[i]
            Text.draw(Themes.font(), n, 30.0f, left + 50.0f, top + 50.0f + float32 i * 50.0f, Color.White)
            if index = i then w.Draw()


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

type ScreenOptions() as this =
    inherit Screen()

    do  
        this.Add(new ConfigEditor<GameOptions>(Options.options))