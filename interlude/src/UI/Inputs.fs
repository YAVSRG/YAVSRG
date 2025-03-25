namespace Interlude.UI

open System
open System.Globalization
open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library

type TextEntry(setting: Setting<string>, hotkey: Hotkey, focus_trap: bool) as this =
    inherit Container(if focus_trap then NodeType.FocusTrap else NodeType.Leaf)

    let ticker = Animation.Counter(600.0)

    let toggle () =
        if this.Selected then
            this.Focus false
        else
            this.Select false

    member val Clickable = true with get, set

    member val ColorFunc : unit -> Color * Color =
        fun () ->
            Colors.white,
            (if this.Selected then
                 Colors.pink_shadow
             else
                 Colors.shadow_1) with get, set

    override this.Init(parent) =
        this
            .With(
                Text(fun () -> setting.Get() + if this.Selected && ticker.Loops % 2 = 0 then "_" else "")
                    .Align(Alignment.LEFT)
                    .Color(this.ColorFunc),
                HotkeyListener(hotkey, toggle)
            )
            .AddConditional(
                this.Clickable,
                MouseListener()
                    .SelectOnClick(this)
                    .OnHover(fun now_focused ->
                        if now_focused && not this.Focused then
                                this.Focus true
                            elif not now_focused && not focus_trap && this.FocusedByMouse then
                                Selection.up true
                    )
                    .OnRightClick(fun () -> setting.Set "")
            )

        base.Init parent

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

type SearchBox(query_text: Setting<string>, callback: string -> unit) as this =
    inherit FrameContainer(NodeType.Container(fun _ -> Some this.TextEntry))
    let search_timer = new Diagnostics.Stopwatch()

    let text_entry =
        TextEntry(
            query_text |> Setting.trigger (fun _ -> this.StartSearch()),
            "search",
            true).Position(Position.Shrink(10.0f, 0.0f))

    member val DebounceTime = 400L with get, set

    new(query_text: Setting<string>, callback: FilterPart list -> unit) = SearchBox(query_text, (fun (query: string) -> callback (FilterParts.parse query)))
    new(query_text: Setting<string>, callback: FilteredSearch -> unit) = SearchBox(query_text, (fun (query: string) -> callback (FilterParts.parse query |> FilteredSearch.Build)))

    member private this.StartSearch() = search_timer.Restart()

    member private this.TextEntry: TextEntry = text_entry

    member val TextColor : unit -> Color * Color = fun () -> !*Palette.LIGHT, !*Palette.DARKER with get, set

    override this.Init(parent) =
        text_entry.ColorFunc <-
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

        this |+ text_entry
        |* Text(
            fun () ->
                match query_text.Value with
                | "" -> Icons.SEARCH + " " + [ (%%"search").ToString() ] %> "misc.search"
                | _ -> ""
            , Color = text_entry.ColorFunc
            , Align = Alignment.LEFT
            ).Position(Position.Shrink(10.0f, 0.0f))

        base.Init parent

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if search_timer.ElapsedMilliseconds > this.DebounceTime then
            search_timer.Reset()
            callback query_text.Value
        // eat a button press for some trigger happy users
        elif search_timer.IsRunning && (%%"select").Pressed() then
            ()

module NumberEntry =

    let create_int (setting: Setting<int, _>) : FrameContainer =

        let try_parse (s: string) =
            match Int32.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set v
            | false, _ -> ()

        let text_setting =
            Setting.simple (setting.Value.ToString())
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '-') |> Array.ofSeq |> System.String)

        let entry =
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true, Position = Position.ShrinkX 15.0f) with
                override this.OnSelected by_mouse =
                    text_setting.Set ""
                    base.OnSelected by_mouse

                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    Style.hover.Play()

                override this.Update(elapsed_ms, moved) =
                    if not this.Selected then
                        text_setting.Set ((float32 setting.Value).ToString())
                    base.Update(elapsed_ms, moved)
            }

        FrameContainer(
            NodeType.Container (fun () -> Some entry),
            Position = Position.ExpandX 15.0f,
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
        |+ entry

    let create (setting: Setting<float, _>) : FrameContainer =

        let try_parse (s: string) =
            match Double.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set v
            | false, _ -> ()

        let text_setting =
            Setting.simple (setting.Value.ToString("R"))
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '.' || c = '-') |> Array.ofSeq |> System.String)

        let entry =
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true, Position = Position.ShrinkX 15.0f) with
                override this.OnSelected by_mouse =
                    text_setting.Set ""
                    base.OnSelected by_mouse

                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    Style.hover.Play()

                override this.Update(elapsed_ms, moved) =
                    if not this.Selected then
                        text_setting.Set ((float32 setting.Value).ToString("R"))
                    base.Update(elapsed_ms, moved)
            }

        FrameContainer(
            NodeType.Container (fun () -> Some entry),
            Position = Position.ExpandX 15.0f,
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
        |+ entry

    let create_uom (units: string) (setting: Setting<float32<'u>, _>) : FrameContainer =

        let try_parse (s: string) =
            match Single.TryParse(s, CultureInfo.InvariantCulture) with
            | true, v -> setting.Set (v |> LanguagePrimitives.Float32WithMeasure)
            | false, _ -> ()

        let text_setting =
            Setting.simple ((float32 setting.Value).ToString("R"))
            |> Setting.map id (fun s -> s |> Seq.filter (fun c -> Char.IsAsciiDigit c || c = '.' || c = '-') |> Array.ofSeq |> System.String)

        let entry =
            { new TextEntry(text_setting |> Setting.trigger try_parse, "none", true, Position = Position.ShrinkX(15.0f).ShrinkR 100.0f) with
                override this.OnSelected by_mouse =
                    text_setting.Set ""
                    base.OnSelected by_mouse

                override this.OnFocus by_mouse =
                    base.OnFocus by_mouse
                    Style.hover.Play()

                override this.Update(elapsed_ms, moved) =
                    if not this.Selected then
                        text_setting.Set ((float32 setting.Value).ToString("R"))
                    base.Update(elapsed_ms, moved)
            }

        FrameContainer(
            NodeType.Container (fun () -> Some entry),
            Position = Position.ExpandX 15.0f,
            Fill = K Color.Transparent,
            Border =
                fun () ->
                    if entry.Selected then Colors.pink_accent
                    elif entry.Focused then Colors.yellow_accent
                    else Colors.grey_2
        )
        |+ entry
        |+ Text(units, Position = Position.SliceR 100.0f, Color = K Colors.text_subheading)