namespace Interlude.UI.Components

open System
open OpenTK
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.Input
open Interlude.Graphics
open Interlude.Options
open Interlude.UI.Animation

type TooltipRegion(localisedText) =
    inherit Widget()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Mouse.Hover this.Bounds && options.Hotkeys.Tooltip.Value.Tapped() then
            Tooltip.tooltip (options.Hotkeys.Tooltip.Value, localisedText)

    static member Create(localisedText) = fun (w: #Widget) -> let t = TooltipRegion localisedText in t.Add w; t

type TextEntry(s: Setting<string>, bind: Setting<Bind> option, prompt: string) as this =
    inherit Widget()

    let color = AnimationFade(0.5f)

    let mutable active = false
    let toggle() =
        active <- not active
        if active then
            color.Target <- 1.0f
            Input.setTextInput(s, fun () -> active <- false; color.Target <- 0.5f)
        else
            color.Target <- 0.5f
            Input.removeInputMethod()

    do
        this.Animation.Add(color)
        if Option.isNone bind then toggle() else this.Add(new Clickable(toggle, ignore))
        Frame(
            Style.main 100,
            fun () -> Style.highlightF 100 color.Value
        )
        |> this.Add
        TextBox(
            (fun () ->
                match bind with
                | Some b ->
                    match s.Value with
                    | "" -> Localisation.localiseWith [b.Value.ToString(); prompt] "misc.search"
                    | text -> text
                | None -> match s.Value with "" -> prompt | text -> text),
            (fun () -> Style.highlightF 255 color.Value), 0.0f)
        |> positionWidgetA(10.0f, 0.0f, -10.0f, 0.0f)
        |> this.Add

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        match bind with
        | Some b -> if b.Value.Tapped() then toggle()
        | None -> if active = false then toggle()

    override this.Dispose() =
        if active then Input.removeInputMethod()

type SearchBox(s: Setting<string>, callback: unit -> unit) as this =
    inherit Widget()
    let searchTimer = new Diagnostics.Stopwatch()
    do
        TextEntry ( Setting.trigger (fun s -> searchTimer.Restart()) s, Some options.Hotkeys.Search, "search" )
        |> this.Add

    new(s: Setting<string>, callback: Filter -> unit) = SearchBox(s, fun () -> callback(Filter.parse s.Value))

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if searchTimer.ElapsedMilliseconds > 400L then searchTimer.Reset(); callback()

type TextInputDialog(bounds: Rect, prompt, callback) as this =
    inherit Dialog()
    let buf = Setting.simple ""
    let tb = TextEntry(buf, None, prompt)
    do
        let struct (l, t, r, b) = bounds
        this.Add(tb |> positionWidget(l, 0.0f, t, 0.0f, r, 0.0f, b, 0.0f))
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if options.Hotkeys.Select.Value.Tapped() || options.Hotkeys.Exit.Value.Tapped() then tb.Dispose(); this.BeginClose()
    override this.OnClose() = callback buf.Value

module SlideDialog =

    type Direction =
        | Left = 0
        | Up = 1

type SlideDialog(direction: SlideDialog.Direction, distance: float32) as this =
    inherit Dialog()

    do
        if direction = SlideDialog.Direction.Left then
            this.Reposition(0.0f, 0.0f, distance, 0.0f)
        else this.Reposition(0.0f, 0.0f, 0.0f, distance)
        this.Move(0.0f, 0.0f, 0.0f, 0.0f)

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if options.Hotkeys.Exit.Value.Tapped() then this.BeginClose()

    override this.BeginClose() =
        base.BeginClose()
        if direction = SlideDialog.Direction.Left then
            this.Move(0.0f, 0.0f, distance, 0.0f)
        else this.Move(0.0f, 0.0f, 0.0f, distance)

    override this.OnClose() = ()

module Dropdown =

    let ITEMSIZE = 60.0f
    let WIDTH = 300.0f
    
    type Item(label: string, onclick: unit -> unit) as this =
        inherit Widget()

        let mutable hover = false

        do
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, ITEMSIZE, 0.0f)
            this.Add(Clickable(onclick, (fun b -> hover <- b), Float = true))
            this.Add(
                TextBox(K label, (fun () -> if hover then Style.accentShade(255, 1.0f, 0.5f), Color.Black else Color.White, Color.Black), 0.0f)
                |> positionWidget(10.0f, 0.0f, 5.0f, 0.0f, -10.0f, 1.0f, -5.0f, 1.0f)
            )

        override this.Draw() =
            if hover then Draw.rect this.Bounds (Color.FromArgb(100, 100, 100, 100)) Sprite.Default
            base.Draw()

    type Container(items: (string * (unit -> unit)) seq) as this =
        inherit Frame(Color.FromArgb(180, 0, 0, 0), Color.FromArgb(100, 255, 255, 255))

        do
            let fc = FlowContainer(Spacing = 0.0f)
            let items = Seq.map (fun (label, action) -> Item(label, fun () -> action(); this.Destroy())) items |> Array.ofSeq

            for i in items do fc.Add i
            this.Add fc

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if options.Hotkeys.Exit.Value.Tapped() || Mouse.Click(Windowing.GraphicsLibraryFramework.MouseButton.Left) then this.Destroy()

    let create (items: (string * (unit -> unit)) seq) =
        Container(items)

    let create_selector (items: 'T seq) (labelFunc: 'T -> string) (selectFunc: 'T -> unit) =
        create (Seq.map (fun item -> (labelFunc item, fun () -> selectFunc item)) items)