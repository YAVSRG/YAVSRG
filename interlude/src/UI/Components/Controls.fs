namespace Interlude.UI.Components

open System
open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude.UI

type StylishButton(onClick, labelFunc: unit -> string, colorFunc, bind: Hotkey) as this =
    inherit StaticContainer(NodeType.Button onClick)
    
    let color = Animation.Fade 0.3f

    do
        this
        |+ Clickable.Focus this
        |* HotkeyAction(bind, onClick) // todo: create this at init-time and make hotkey optional
    
    // todo: remove this and make hotkey optional
    new(onClick, labelFunc, colorFunc) = StylishButton(onClick, labelFunc, colorFunc, "none")

    member val TiltLeft = true with get, set
    member val TiltRight = true with get, set

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        color.Update elapsedTime

    override this.OnFocus() = base.OnFocus(); color.Target <- 0.7f
    override this.OnUnfocus() = base.OnUnfocus(); color.Target <- 0.3f

    override this.Draw() =
        let h = this.Bounds.Height
        Draw.quad
            ( Quad.create
                <| Vector2(this.Bounds.Left, this.Bounds.Top)
                <| Vector2(this.Bounds.Right + (if this.TiltRight then h * 0.5f else 0.0f), this.Bounds.Top)
                <| Vector2(this.Bounds.Right, this.Bounds.Bottom)
                <| Vector2(this.Bounds.Left - (if this.TiltLeft then h * 0.5f else 0.0f), this.Bounds.Bottom)
            ) (colorFunc () |> Quad.colorOf)
            Sprite.DefaultQuad
        Text.drawFillB(Style.baseFont, labelFunc(), this.Bounds, (Style.highlightF 255 color.Value, Style.color(255, 0.4f, color.Value)), 0.5f)
        base.Draw()

    static member FromEnum<'T when 'T: enum<int>>(label: string, setting: Setting<'T>, colorFunc) =
        let names = Enum.GetNames(typeof<'T>)
        let values = Enum.GetValues(typeof<'T>) :?> 'T array
        let mutable i = array.IndexOf(values, setting.Value)
        StylishButton(
            (fun () -> i <- (i + 1) % values.Length; setting.Value <- values.[i]), 
            (fun () -> sprintf "%s: %s" label names.[i]),
            colorFunc
        )

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