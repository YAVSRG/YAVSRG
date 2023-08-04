namespace Interlude.UI.Components

open System
open OpenTK.Mathematics
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude.Utils

type TextEntry(setting: Setting<string>, hotkey: Hotkey) as this =
    inherit StaticContainer(NodeType.Leaf)

    let ticker = Animation.Counter(600.0)

    let toggle() = if this.Selected then this.Focus() else this.Select()

    member val Clickable = true with get, set
    member val ColorFunc = fun () -> Colors.white, (if this.Selected then Colors.pink_shadow else Colors.shadow_1) with get, set

    override this.Init(parent) =
        base.Init parent
        this
        |+ Text(
            (fun () -> setting.Get() + if this.Selected && ticker.Loops % 2 = 0 then "_" else ""),
            Align = Alignment.LEFT, 
            Color = this.ColorFunc)
        |* HotkeyAction(hotkey, toggle)
        if this.Clickable then this.Add (Clickable.Focus this)

    override this.OnSelected() =
        base.OnSelected()
        Style.text_open.Play()
        Input.setTextInput(setting |> Setting.trigger(fun v -> Style.key.Play()), fun () -> if this.Selected then this.Focus())

    override this.OnDeselected() =
        base.OnDeselected()
        Style.text_close.Play()
        Input.removeInputMethod()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        ticker.Update(elapsedTime)

type StylishButton(onClick, labelFunc: unit -> string, colorFunc) as this =
    inherit StaticContainer(NodeType.Button (fun () -> Style.click.Play(); onClick()))

    member val Hotkey : Hotkey = "none" with get, set
    member val TiltLeft = true with get, set
    member val TiltRight = true with get, set
    member val TextColor = fun () -> (if this.Focused then Colors.yellow_accent else Colors.grey_1), Colors.shadow_2 with get, set

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
        Text.drawFillB(Style.font, labelFunc(), this.Bounds, this.TextColor(), 0.5f)
        base.Draw()

    override this.Init(parent: Widget) =
        this
        |+ Clickable.Focus this
        |* HotkeyAction(this.Hotkey, fun () -> Style.click.Play(); onClick())
        base.Init parent

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()

    static member Selector<'T>(label: string, values: ('T * string) array, setting: Setting<'T>, colorFunc) =
        let mutable current = array.IndexOf(values |> Array.map fst, setting.Value)
        current <- max 0 current
        StylishButton(
            (fun () -> current <- (current + 1) % values.Length; setting.Value <- fst values.[current]), 
            (fun () -> sprintf "%s %s" label (snd values.[current])),
            colorFunc
        )

type TextEntryBox(setting: Setting<string>, bind: Hotkey, prompt: string) as this =
    inherit Frame(NodeType.Switch(fun _ -> this.TextEntry))

    let textEntry = 
        TextEntry(setting, bind, 
            Position = Position.Margin(10.0f, 0.0f),
            ColorFunc = fun () -> (if this.TextEntry.Selected then Colors.white else !*Palette.LIGHT), !*Palette.DARKER
        )

    do
        this.Fill <- fun () -> if this.TextEntry.Selected then Colors.yellow_accent.O1 else !*Palette.DARK
        this.Border <- fun () -> if this.TextEntry.Selected then Colors.yellow_accent else !*Palette.LIGHT

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
                Color = textEntry.ColorFunc,
                Align = Alignment.LEFT,
                Position = Position.Margin(10.0f, 0.0f))

    member private this.TextEntry : TextEntry = textEntry

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

type WIP() as this =
    inherit StaticWidget(NodeType.None)

    let text = L"misc.wip"

    do
        this.Position <- Position.SliceBottom(100.0f)

    override this.Draw() =
        Draw.rect this.Bounds (Color.FromArgb(127, Color.Yellow))
        let w = this.Bounds.Width / 20.0f
        for i = 0 to 19 do
            Draw.rect (Rect.Box (this.Bounds.Left + w * float32 i, this.Bounds.Top, w, 10.0f)) (if i % 2 = 0 then Color.Yellow else Color.Black)
            Draw.rect (Rect.Box (this.Bounds.Left + w * float32 i, this.Bounds.Bottom - 10.0f, w, 10.0f)) (if i % 2 = 1 then Color.Yellow else Color.Black)
        Text.drawFillB(Style.font, text, this.Bounds.Shrink(20.0f), Colors.text, Alignment.CENTER)
