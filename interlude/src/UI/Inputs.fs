namespace Interlude.UI

open System
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Sorting

type TextEntry(setting: Setting<string>, hotkey: Hotkey, focus_trap: bool) as this =
    inherit Container(if focus_trap then NodeType.FocusTrap else NodeType.Leaf)

    let ticker = Animation.Counter(600.0)

    let toggle () =
        if this.Selected then
            this.Focus false
        else
            this.Select false

    member val Clickable = true with get, set

    member val ColorFunc =
        fun () ->
            Colors.white,
            (if this.Selected then
                 Colors.pink_shadow
             else
                 Colors.shadow_1) with get, set

    override this.Init(parent) =
        base.Init parent

        this
        |+ Text(
            (fun () -> setting.Get() + if this.Selected && ticker.Loops % 2 = 0 then "_" else ""),
            Align = Alignment.LEFT,
            Color = this.ColorFunc
        )
        |* HotkeyAction(hotkey, toggle)

        if this.Clickable then
            this.Add(
                Clickable.Focus(
                    this,
                    OnHover =
                        (fun b ->
                            if b && not this.Focused then
                                this.Focus true
                            elif not b && not focus_trap && this.FocusedByMouse then
                                Selection.up true
                        ),
                    OnRightClick = (fun () -> setting.Set "")
                )
            )

    override this.OnSelected(by_mouse: bool) =
        base.OnSelected by_mouse
        Style.text_open.Play()

        Input.listen_to_text (
            setting |> Setting.trigger (fun v -> Style.key.Play()),
            not by_mouse,
            fun () ->
                if this.Selected then
                    this.Focus true
        )

    override this.OnDeselected(by_mouse: bool) =
        base.OnDeselected by_mouse
        Style.text_close.Play()
        Input.remove_listener ()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        ticker.Update(elapsed_ms)

type SearchBox(s: Setting<string>, callback: unit -> unit) as this =
    inherit FrameContainer(NodeType.Container(fun _ -> Some this.TextEntry))
    let search_timer = new Diagnostics.Stopwatch()

    let text_entry =
        TextEntry(
            s |> Setting.trigger (fun _ -> this.StartSearch()),
            "search",
            true,
            Position = Position.Margin(10.0f, 0.0f),
            ColorFunc =
                fun () ->
                    (if this.TextEntry.Selected then
                         Colors.white
                     else
                         !*Palette.LIGHT),
                    !*Palette.DARKER
        )

    member val DebounceTime = 400L with get, set

    new(s: Setting<string>, callback: Filter -> unit) = SearchBox(s, (fun () -> callback (Filter.parse s.Value)))

    member private this.StartSearch() = search_timer.Restart()

    member private this.TextEntry: TextEntry = text_entry

    override this.Init(parent) =
        this.Fill <-
            fun () ->
                if this.TextEntry.Selected then
                    Colors.yellow_accent.O1
                else
                    !*Palette.DARK

        this.Border <-
            fun () ->
                if this.TextEntry.Selected then
                    Colors.yellow_accent
                else
                    !*Palette.LIGHT

        this |+ text_entry
        |* Text(
            fun () ->
                match s.Value with
                | "" -> Icons.SEARCH + " " + [ (%%"search").ToString() ] %> "misc.search"
                | _ -> ""
            , Color = text_entry.ColorFunc
            , Align = Alignment.LEFT
            , Position = Position.Margin(10.0f, 0.0f)
        )

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if search_timer.ElapsedMilliseconds > this.DebounceTime then
            search_timer.Reset()
            callback ()
        // eat a button press for some trigger happy users
        elif search_timer.IsRunning && (%%"select").Tapped() then
            ()