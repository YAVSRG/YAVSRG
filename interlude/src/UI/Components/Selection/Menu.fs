namespace Interlude.UI.Components.Selection.Menu

open System
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components

type IMenu =
    abstract member ChangePage: (IMenu -> #Page) -> unit
    abstract member Back: unit -> unit

and [<AbstractClass>] Page(p: IMenu) =
    inherit DynamicContainer(NodeType.None)

    let mutable isCurrent = false
    let mutable content = None

    member this.Parent = p

    abstract member Title : string
    abstract member OnClose : unit -> unit
    abstract member OnDestroy : unit -> unit
    default this.OnDestroy() = ()

    member this.Content(w: Widget) =
        this.Add(w)
        content <- Some w

    override this.Update(elapsedTime, moved) =
        if isCurrent && not content.Value.Focused then p.Back()
        base.Update(elapsedTime, moved)

    member this.View() =
        this.Position <- Position.Default
        content.Value.Focus()
        isCurrent <- true

    member this.MoveDown() =
        this.Position <- { Position.Default with Top = 1.0f %+ 0.0f; Bottom = 2.0f %+ 0.0f }
        isCurrent <- false

    member this.MoveUp() =
        this.Position <- { Position.Default with Top = -1.0f %+ 0.0f; Bottom = 0.0f %+ 0.0f }
        isCurrent <- false

    override this.Init(parent: Widget) =
        if content.IsNone then failwithf "Call Content() to provide page content"
        this.MoveUp()
        base.Init parent
        this.View()

[<AutoOpen>]
module Helpers =

    let column() = SwitchContainer.Column<Widget>()
    let row() = SwitchContainer.Row<Widget>()

    let refreshRow number cons =
        let r = SwitchContainer.Row()
        let refresh() =
            r.Clear()
            let n = number()
            for i in 0 .. (n - 1) do
                r.Add(cons i n)
        refresh()
        r, refresh

    /// N for Name -- Shorthand for getting localised name of a setting `s`
    let N (s: string) = L ("options." + s + ".name")
    /// T for Tooltip -- Shorthand for getting localised tooltip of a setting `s`
    let T (s: string) = L ("options." + s + ".tooltip")
    /// E for Editing -- Shorthand for getting localised title of page when editing object named `name`
    let E (name: string) = Localisation.localiseWith [name] "misc.edit"

    //let refreshChoice (options: string array) (widgets: Widget1 array array) (setting: Setting<int>) =
    //    let rec newSetting =
    //        {
    //            Set =
    //                fun x ->
    //                    for w in widgets.[setting.Value] do if w.Parent.IsSome then selector.SParent.Value.SParent.Value.Remove w
    //                    for w in widgets.[x] do selector.SParent.Value.SParent.Value.Add w
    //                    setting.Value <- x
    //            Get = setting.Get
    //            Config = setting.Config
    //        }
    //    and selector : Percyqaz.Flux.UI.Selector<int> = Percyqaz.Flux.UI.Selector(Array.indexed options, newSetting)
    //    selector.Synchronized(fun () -> newSetting.Value <- newSetting.Value)
    //    selector

    let PRETTYTEXTWIDTH = 500.0f
    let PRETTYHEIGHT = 80.0f
    let PRETTYWIDTH = 1200.0f

type Divider() =
    inherit StaticWidget(NodeType.None)

    member this.Pos(y) =
        this.Position <- Percyqaz.Flux.UI.Position.Box(0.0f, 0.0f, 100.0f, y - 5.0f, PRETTYWIDTH, 10.0f)
        this

    override this.Draw() =
        Draw.quad (Quad.ofRect this.Bounds) (struct(Color.White, Color.FromArgb(0, 255, 255, 255), Color.FromArgb(0, 255, 255, 255), Color.White)) Sprite.DefaultQuad

type PrettySetting(name, widget: Widget) as this =
    inherit StaticContainer(NodeType.Switch (fun _ -> this.Child))

    let mutable widget = widget

    do
        this
        |+ Text(
            K (N name + ":"),
            Color = (fun () -> ((if this.Selected then Style.accentShade(255, 1.0f, 0.2f) else Color.White), Color.Black)),
            Align = Alignment.LEFT,
            Position = Position.SliceLeft PRETTYTEXTWIDTH)
        |* TooltipRegion2(T name)

    member this.Child
        with get() = widget
        and set(w: Widget) =
            widget <- w
            w.Position <- Position.TrimLeft PRETTYTEXTWIDTH
            if this.Initialised then w.Init this
    
    member this.Pos(y, width, height) =
        this.Position <- Position.Box(0.0f, 0.0f, 100.0f, y, width, height) 
        this
    
    member this.Pos(y, width) = this.Pos(y, width, PRETTYHEIGHT)
    member this.Pos(y) = this.Pos(y, PRETTYWIDTH)

    override this.Init(parent) =
        base.Init parent
        widget.Position <- Position.TrimLeft PRETTYTEXTWIDTH
        widget.Init this

    override this.Draw() =
        if this.Selected then Draw.rect this.Bounds (Style.accentShade(120, 0.4f, 0.0f))
        elif this.Focused then Draw.rect this.Bounds (Style.accentShade(100, 0.4f, 0.0f))
        base.Draw()
        widget.Draw()
    
    override this.Update(elapsedTime, moved) =
        widget.Update(elapsedTime, moved)
        base.Update(elapsedTime, moved)

type PrettyButton(name, action) as this =
    inherit StaticContainer(NodeType.Leaf)

    do
        this
        |+ Text(
            K (N name + "  >"),
            Color = ( 
                fun () -> 
                    if this.Enabled then
                        ( (if this.Focused then Style.accentShade(255, 1.0f, 0.5f) else Color.White), Color.Black )
                    else (Color.Gray, Color.Black)
            ),
            Align = Alignment.LEFT )
        |+ Percyqaz.Flux.UI.Clickable(this.Select, OnHover = fun b -> if b then this.Focus())
        |* TooltipRegion2(T name)

    override this.OnSelected() =
        base.OnSelected()
        if this.Enabled then action()
        this.Focus()
    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds (Style.accentShade(120, 0.4f, 0.0f))
        base.Draw()
    member this.Pos(y) = 
        this.Position <- Percyqaz.Flux.UI.Position.Box(0.0f, 0.0f, 100.0f, y, PRETTYWIDTH, PRETTYHEIGHT)
        this

    member val Enabled = true with get, set

    static member Once(name, action, notifText, notifType) =
        { new PrettyButton(name, action) with
            override this.OnSelected() =
                base.OnSelected()
                if base.Enabled then Notification.add (notifText, notifType)
                base.Enabled <- false
        }

type Menu(topLevel: IMenu -> Page) as this =
    inherit Percyqaz.Flux.UI.Dialog()

    let MAX_PAGE_DEPTH = 12
    
    // Everything left of the current page is Some
    // Everything past the current page could be Some from an old backed out page
    let stack: Page option array = Array.create MAX_PAGE_DEPTH None
    let mutable namestack = []
    let mutable name = ""

    // todo: add a back button
    
    let add (page: IMenu -> #Page) =

        let page = page (this :> IMenu) :> Page
        page.Init this

        let n = List.length namestack
        namestack <- page.Title :: namestack
        name <- String.Join(" > ", List.rev namestack)

        match stack.[n] with
        | None -> ()
        | Some page -> page.OnDestroy()

        stack.[n] <- Some page
        if n > 0 then stack.[n - 1].Value.MoveDown()
    
    let back() =
        namestack <- List.tail namestack
        name <- String.Join(" > ", List.rev namestack)
        let n = List.length namestack
        let page = stack.[n].Value
        page.OnClose()
        page.MoveUp()
        if n > 0 then stack.[n - 1].Value.View()

    override this.Init(parent) =
        base.Init(parent)
        add topLevel

    override this.Draw() =
        let mutable i = 0
        while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
            stack.[i].Value.Draw()
            i <- i + 1
        Text.drawFillB(Style.baseFont, name, this.Bounds.SliceTop(100.0f).Shrink(20.0f), Style.text(), 0.0f)
    
    override this.Update(elapsedTime, moved) =
        let mutable i = 0
        while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
            stack.[i].Value.Update(elapsedTime, moved)
            i <- i + 1

        if List.isEmpty namestack then
            let mutable i = 0
            while i < MAX_PAGE_DEPTH && stack.[i].IsSome do
                stack.[i].Value.OnDestroy()
                i <- i + 1
            this.Close()

    interface IMenu with
        member this.ChangePage(p: IMenu -> #Page) = add p
        member this.Back() = back()

//type ConfirmDialog(prompt, callback: unit -> unit) as this =
//    inherit Dialog()

//    let mutable confirm = false

//    let options =
//        row [ 
//            LittleButton(
//                K "Yes",
//                fun () -> this.BeginClose(); confirm <- true
//            ).Position(Position.SliceLeft 200.0f)
//            LittleButton(
//                K "No", 
//                this.BeginClose
//            ).Position(Position.SliceRight 200.0f)
//        ]

//    do
//        TextBox(K prompt, K (Color.White, Color.Black), 0.5f)
//            .Position { Left = 0.0f %+ 200.0f; Top = 0.5f %- 200.0f; Right = 1.0f %- 200.0f; Bottom = 0.5f %- 50.0f }
//        |> this.Add
//        options.OnSelect()
//        options.Position { Left = 0.5f %- 300.0f; Top = 0.5f %+ 0.0f; Right = 0.5f %+ 300.0f; Bottom = 0.5f %+ 100.0f }
//        |> this.Add

//    override this.Update(elapsedTime, bounds) =
//        base.Update(elapsedTime, bounds)
//        if not options.Selected then this.BeginClose()

//    override this.OnClose() = if confirm then callback()