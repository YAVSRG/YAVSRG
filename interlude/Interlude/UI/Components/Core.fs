namespace Interlude.UI.Components

open System
open System.Drawing
open OpenTK
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude
open Interlude.UI
open Interlude.Input
open Interlude.Graphics
open Interlude.Options
open Interlude.UI.Animation

type TextBox(textFunc, color, just) =
    inherit Widget()

    new(textFunc, scolor, just) = TextBox(textFunc, (fun () -> scolor(), Color.Transparent), just)

    override this.Draw() = 
        Text.drawFillB(Themes.font(), textFunc(), this.Bounds, color(), just)
        base.Draw()

type TooltipRegion(localisedText) =
    inherit Widget()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Mouse.Hover(this.Bounds) && options.Hotkeys.Tooltip.Value.Tapped() then
            Tooltip.add (options.Hotkeys.Tooltip.Value, localisedText, infinity, ignore)

type Dropdown(options: string array, index, func, label, buttonSize) as this =
    inherit Widget()

    let color = AnimationFade 0.5f
    let mutable index = index

    do
        this.Animation.Add color
        let fr = new Frame(Enabled = false)
        this.Add((Clickable((fun () -> fr.Enabled <- not fr.Enabled), fun b -> color.Target <- if b then 0.8f else 0.5f)) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, buttonSize, 0.0f))
        this.Add(
            let fc = FlowContainer(Spacing = 0.0f)
            fr.Add fc
            Array.iteri
                (fun i o -> fc.Add(Button((fun () -> index <- i; func i), o) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 40.0f, 0.0f)))
                options
            fr |> positionWidgetA(0.0f, buttonSize, 0.0f, 0.0f))
            
    override this.Draw() =
        let bbounds = Rect.sliceTop buttonSize this.Bounds
        Draw.rect (Rect.expand (5.0f, 5.0f) bbounds) (Style.accentShade(127, 0.5f, 0.0f)) Sprite.Default
        Draw.rect bbounds (Style.accentShade(255, 0.6f, 0.0f)) Sprite.Default
        Text.drawFill(Themes.font(), label, Rect.sliceTop 20.0f bbounds, Color.White, 0.5f)
        Text.drawFill(Themes.font(), options.[index], bbounds |> Rect.trimTop 20.0f, Color.White, 0.5f)
        base.Draw()

type TextEntry(s: Setting<string>, bind: Setting<Bind> option, prompt: string) as this =
    inherit Frame()

    let color = AnimationFade(0.5f)

    let mutable active = false
    let toggle() =
        active <- not active
        if active then
            color.Target <- 1.0f
            Input.setInputMethod(s, fun () -> active <- false; color.Target <- 0.5f)
        else
            color.Target <- 0.5f
            Input.removeInputMethod()

    do
        this.Animation.Add(color)
        if Option.isNone bind then toggle() else this.Add(new Clickable(toggle, ignore))
        this.Add(
            TextBox(
                (fun () ->
                    match bind with
                    | Some b ->
                        match s.Value with
                        //todo: localise
                        | "" -> sprintf "Press %s to %s" (b.Value.ToString()) prompt
                        | text -> text
                    | None -> match s.Value with "" -> prompt | text -> text),
                (fun () -> Style.accentShade(255, 1.0f, color.Value)), 0.0f))

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        match bind with
        | Some b -> if b.Value.Tapped() then toggle()
        | None -> if active = false then toggle()

    override this.Dispose() =
        if active then Input.removeInputMethod()

type SearchBox(s: Setting<string>, callback: Filter -> unit) as this =
    inherit Widget()
    //todo: this seems excessive. replace with two variables?
    let searchTimer = new System.Diagnostics.Stopwatch()
    do
        TextEntry ( Setting.trigger (fun s -> searchTimer.Restart()) s, Some options.Hotkeys.Search, "search" )
        |> this.Add

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if searchTimer.ElapsedMilliseconds > 400L then searchTimer.Reset(); callback(Filter.parse s.Value)

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
        if Options.options.Hotkeys.Exit.Value.Tapped() then this.BeginClose()

    override this.BeginClose() =
        base.BeginClose()
        if direction = SlideDialog.Direction.Left then
            this.Move(0.0f, 0.0f, distance, 0.0f)
        else this.Move(0.0f, 0.0f, 0.0f, distance)

    override this.OnClose() = ()