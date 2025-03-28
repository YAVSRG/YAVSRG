namespace Interlude.UI

open System
open System.Runtime.CompilerServices
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Data.Library

// todo: consider composition over inheriting the framecontainer!
// todo: reconsider fragile composition with TextColor on TextEntry
type SearchBox(query_text: Setting<string>, callback: string -> unit) as this =
    inherit FrameContainer(NodeType.Container(fun _ -> Some this.TextEntry))
    let search_timer = new Diagnostics.Stopwatch()

    let text_entry =
        TextEntry(
            query_text |> Setting.trigger (fun _ -> this.StartSearch()),
            "search",
            true
        )

    static member HEIGHT = 60.0f

    new(callback: string -> unit) = SearchBox(Setting.simple "", callback)
    new(query_text: Setting<string>, callback: FilteredSearch -> unit) = SearchBox(query_text, (fun (query: string) -> callback (FilterParts.parse query |> FilteredSearch.Build)))

    member private this.StartSearch() = search_timer.Restart()

    member private this.TextEntry: TextEntry = text_entry

    member val DebounceTime = 400L with get, set
    member val TextColor : unit -> Color * Color = fun () -> !*Palette.LIGHT, !*Palette.DARKER with get, set
    member val KeyboardAutoSelect : bool = false with get, set

    override this.Init(parent) =
        text_entry.TextColor <-
            fun () ->
                if this.TextEntry.Selected then
                    Colors.white, Colors.shadow_2
                else
                    this.TextColor()

        this.Fill <-
            let existing = this.Fill
            fun () ->
                if this.TextEntry.Selected then
                    Colors.yellow_accent.O1
                else
                    existing()

        this.Border <-
            let existing = this.Border
            fun () ->
                if this.TextEntry.Selected then
                    Colors.yellow_accent
                else
                    existing()

        this
            .Add(
                text_entry
                    .Position(Position.Shrink(10.0f, 0.0f)),

                Text(fun () ->
                    match query_text.Value with
                    | "" -> Icons.SEARCH + " " + [ (%%"search").ToString() ] %> "misc.search"
                    | _ -> ""
                )
                    .Color(text_entry.TextColor)
                    .Align(Alignment.LEFT)
                    .Position(Position.ShrinkX(10.0f))
            )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if search_timer.ElapsedMilliseconds > this.DebounceTime then
            search_timer.Reset()
            callback query_text.Value

        // Some users' instinct is to hit enter the moment they're done typing
        // This would likely activate some other button they don't actually mean to
        // Main example being hitting enter in level select = play the selected chart
        // So if they do this, eat the input - they will believe it responded to them hitting enter all the same :)
        elif search_timer.IsRunning && (%%"select").Pressed() then
            ignore ()

    override this.OnFocus (by_mouse: bool) : unit =
        base.OnFocus(by_mouse: bool)
        if this.KeyboardAutoSelect && not by_mouse then GameThread.defer (fun () -> this.Select false)

[<Extension>]
type SearchBoxExtensions() =

    [<Extension>]
    static member inline TextColor(container: #SearchBox, color: Color) : #SearchBox =
        container.TextColor <- K (color, Colors.shadow_2)
        container

    [<Extension>]
    static member inline TextColor(container: #SearchBox, color: unit -> Color) : #SearchBox =
        container.TextColor <- fun () -> (color(), Colors.shadow_2)
        container

    [<Extension>]
    static member inline TextColor(container: #SearchBox, color: Color * Color) : #SearchBox =
        container.TextColor <- K color
        container

    [<Extension>]
    static member inline TextColor(container: #SearchBox, color: unit -> Color * Color) : #SearchBox =
        container.TextColor <- color
        container

    [<Extension>]
    static member inline KeyboardAutoSelect(container: #SearchBox) : #SearchBox =
        container.KeyboardAutoSelect <- true
        container

    // todo: consider an extension to set all cyan settings at once