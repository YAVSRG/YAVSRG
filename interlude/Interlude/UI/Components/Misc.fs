namespace Interlude.UI.Components

open System
open OpenTK
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude
open Interlude.UI
open Interlude.Input
open Interlude.Graphics
open Interlude.Options
open Interlude.UI.Animation

type TooltipRegion(localisedText) =
    inherit Widget()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Mouse.Hover this.Bounds && (!|Hotkey.Tooltip).Tapped() then
            Tooltip.tooltip ((!|Hotkey.Tooltip), localisedText)

    static member Create(localisedText) = fun (w: #Widget) -> let t = TooltipRegion localisedText in t.Add w; t

type TextEntry(s: Setting<string>, bind: Hotkey option, prompt: string) as this =
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
                    | "" -> Localisation.localiseWith [(!|b).ToString(); prompt] "misc.search"
                    | text -> text
                | None -> match s.Value with "" -> prompt | text -> text),
            (fun () -> Style.highlightF 255 color.Value), 0.0f)
        |> positionWidgetA(10.0f, 0.0f, -10.0f, 0.0f)
        |> this.Add

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        match bind with
        | Some b -> if (!|b).Tapped() then toggle()
        | None -> if active = false then toggle()

    override this.Dispose() =
        if active then Input.removeInputMethod()

type SearchBox(s: Setting<string>, callback: unit -> unit) as this =
    inherit Widget()
    let searchTimer = new Diagnostics.Stopwatch()
    do
        TextEntry ( Setting.trigger (fun s -> searchTimer.Restart()) s, Some Hotkey.Search, "search" )
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
        if (!|Hotkey.Select).Tapped() || (!|Hotkey.Exit).Tapped() then tb.Dispose(); this.BeginClose()
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
        if (!|Hotkey.Exit).Tapped() then this.BeginClose()

    override this.BeginClose() =
        base.BeginClose()
        if direction = SlideDialog.Direction.Left then
            this.Move(0.0f, 0.0f, distance, 0.0f)
        else this.Move(0.0f, 0.0f, 0.0f, distance)

    override this.OnClose() = ()