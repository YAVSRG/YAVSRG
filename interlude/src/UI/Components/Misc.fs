namespace Interlude.UI.Components

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude.UI

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

type TextEntryBox(setting: Setting<string>, bind: Hotkey, prompt: string) as this =
    inherit Frame(NodeType.Switch(fun _ -> this.TextEntry))

    let textEntry = TextEntry(setting, bind, Position = Position.Margin(10.0f, 0.0f))

    do
        this
        |+ textEntry
        |* Text(
                fun () ->
                    match bind with
                    | "none" -> match setting.Value with "" -> prompt | _ -> ""
                    | b ->
                        match setting.Value with
                        | "" -> Localisation.localiseWith [(!|b).ToString(); prompt] "misc.search"
                        | _ -> ""
                ,
                Color = textEntry.TextColor,
                Align = Alignment.LEFT,
                Position = Position.Margin(10.0f, 0.0f))

    member private this.TextEntry = textEntry

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

type SearchBox(s: Setting<string>, callback: unit -> unit) as this =
    inherit TextEntryBox(s |> Setting.trigger(fun _ -> this.StartSearch()), "search", "search")
    let searchTimer = new Diagnostics.Stopwatch()

    member val DebounceTime = 400L with get, set

    new(s: Setting<string>, callback: Filter -> unit) = SearchBox(s, fun () -> callback(Filter.parse s.Value))

    member private this.StartSearch() = searchTimer.Restart()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if searchTimer.ElapsedMilliseconds > this.DebounceTime then searchTimer.Reset(); callback()