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
        Input.setTextInput(setting, fun () -> if this.Selected then this.Focus())

    override this.OnDeselected() =
        base.OnDeselected()
        Input.removeInputMethod()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        ticker.Update(elapsedTime)

type StylishButton(onClick, labelFunc: unit -> string, colorFunc) =
    inherit StaticContainer(NodeType.Button onClick)
    
    let color = Animation.Fade(0.0f)

    member val Hotkey : Hotkey = "none" with get, set
    member val TiltLeft = true with get, set
    member val TiltRight = true with get, set
    member val TextColor = Palette.text (Palette.transition color Palette.LIGHTER Palette.WHITE) (!%Palette.DARKER) with get, set

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        color.Update elapsedTime

    override this.OnFocus() = base.OnFocus(); color.Target <- 1.0f
    override this.OnUnfocus() = base.OnUnfocus(); color.Target <- 0.0f

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
        Text.drawFillB(Style.baseFont, labelFunc(), this.Bounds, this.TextColor(), 0.5f)
        base.Draw()

    override this.Init(parent: Widget) =
        this
        |+ Clickable.Focus this
        |* HotkeyAction(this.Hotkey, onClick)
        base.Init parent

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
            ColorFunc = fun () -> (if this.TextEntry.Selected then !*Palette.WHITE else !*Palette.LIGHT), !*Palette.DARKER
        )

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
        Text.drawFillB(Style.baseFont, text, this.Bounds.Shrink(20.0f), Style.text(), Alignment.CENTER)

type GoodButton(content: Callout, action) as this =
    inherit Frame(NodeType.Button action)

    let width, height = Callout.measure content

    do
        this.Fill <- fun () -> if this.Focused then Colors.pink.O3 else Colors.cyan.O3
        this.Border <- fun () -> if this.Focused then Colors.pink_accent else Colors.cyan_accent

        this |* Clickable.Focus this

    member this.Width = width
    member this.Height = height + 40.0f

    override this.Draw() =
        base.Draw()
        Callout.draw(this.Bounds.Left, this.Bounds.Top + 20.0f, height, (if this.Focused then Colors.text_yellow_2 else Colors.text), content)