namespace Interlude.UI.Components

open System
open OpenTK
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude.UI
open Interlude.UI.Components

type TooltipRegion(localisedText) =
    inherit Widget1()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Mouse.hover this.Bounds && (!|"tooltip").Tapped() then
            Tooltip.tooltip ((!|"tooltip"), localisedText)

    static member Create(localisedText) = fun (w: #Widget1) -> let t = TooltipRegion localisedText in t.Add w; t

type TooltipRegion2(localisedText) =
    inherit StaticWidget(NodeType.None)

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if Mouse.hover this.Bounds && (!|"tooltip").Tapped() then
            Tooltip.tooltip ((!|"tooltip"), localisedText)

    override this.Draw() = ()

// todo: TooltipContainer that contains both a region and another widget

[<AutoOpen>]
module Tooltip =
    type Widget1 with
        member this.Tooltip(localisedText) = TooltipRegion.Create localisedText this

type TextEntry(s: Setting<string>, bind: Hotkey option, prompt: string) as this =
    inherit Widget1()

    let color = Animation.Fade 0.5f

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
        this
        |-* color
        |-+ Frame(Style.main 100, fun () -> Style.highlightF 100 color.Value)
        |-+ TextBox(
                (fun () ->
                    match bind with
                    | Some b ->
                        match s.Value with
                        | "" -> Localisation.localiseWith [(!|b).ToString(); prompt] "misc.search"
                        | text -> text
                    | None -> match s.Value with "" -> prompt | text -> text),
                (fun () -> Style.highlightF 255 color.Value),
                0.0f
            ).Position( Position.Margin(10.0f, 0.0f) )
        |> fun this -> if Option.isNone bind then toggle() else this.Add(new Clickable(toggle, ignore))

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        match bind with
        | Some b -> if (!|b).Tapped() then toggle()
        | None -> if active = false then toggle()

    override this.Dispose() =
        if active then Input.removeInputMethod()

type SearchBox(s: Setting<string>, callback: unit -> unit) as this =
    inherit Widget1()
    let searchTimer = new Diagnostics.Stopwatch()
    do
        TextEntry ( Setting.trigger (fun s -> searchTimer.Restart()) s, Some "search", "search" )
        |> this.Add

    new(s: Setting<string>, callback: Filter -> unit) = SearchBox(s, fun () -> callback(Filter.parse s.Value))

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if searchTimer.ElapsedMilliseconds > 400L then searchTimer.Reset(); callback()