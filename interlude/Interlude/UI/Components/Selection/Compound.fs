namespace Interlude.UI.Components.Selection.Compound

open System.Drawing
open Prelude.Common
open Interlude.Utils
open Interlude.Graphics
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers
open Interlude.UI.Components.Selection.Buttons
open Interlude.UI.Components.Selection.Menu

module ListOrderedSelect =
    type ListOrderedItem(name, selector: ListOrderedSelector) as this =
        inherit NavigateSelectable()

        do
            this.Add(TextBox(K name, K (Color.White, Color.Black), 0.5f))
            this.Add(Clickable((fun () -> (if not this.Selected then this.Selected <- true); this.Left()), fun b -> if b then this.Hover <- true))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 60.0f, 0.0f)

        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.4f)) Sprite.Default
            elif this.Hover then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.1f)) Sprite.Default
            else Draw.rect this.Bounds (Style.accentShade(180, 0.6f, 0.0f)) Sprite.Default
            base.Draw()

        override this.SParent = Some (selector :> Selectable)

        member this.Name = name

        override this.Up() =
            let p = this.Parent.Value
            match p with
            | e when e = selector.Chosen ->
                let c = p.Children
                match c.IndexOf this with
                | 0 -> ()
                | n -> p.Synchronized(fun () -> c.Reverse(n - 1, 2))
            | _ -> ()

        override this.Down() =
            let p = this.Parent.Value
            match p with
            | e when e = selector.Chosen ->
                let c = p.Children
                match c.IndexOf this with
                | x when x + 1 = c.Count -> ()
                | n -> p.Synchronized(fun () -> c.Reverse(n, 2))
            | _ -> ()

        override this.Left() =
            let p = this.Parent.Value
            let o =
                match p with
                | e when e = selector.Chosen -> selector.Available
                | a when a = selector.Available -> selector.Chosen
                | _ -> failwith "impossible"
            p.Synchronized(fun () -> p.Remove this; o.Add this)
        override this.Right() = this.Left()

    and ListOrderedSelector(setting: Setting<ResizeArray<string>>, items: ResizeArray<string>) as this =
        inherit NavigateSelectable()

        let available = new FlowContainer() :> Widget
        let selected = new FlowContainer() :> Widget

        do
            this.Add(TextBox(K (Localisation.localise "options.select.Available"), K (Color.White, Color.Black), 0.5f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.5f, 50.0f, 0.0f) )
            this.Add(
                available |> Frame.Create
                |> positionWidget(20.0f, 0.0f, 50.0f, 0.0f, -20.0f, 0.5f, -20.0f, 1.0f) )
            this.Add(TextBox(K (Localisation.localise "options.select.Selected"), K (Color.White, Color.Black), 0.5f)
                |> positionWidget(0.0f, 0.5f, 0.0f, 0.0f, 0.0f, 1.0f, 50.0f, 0.0f) )
            this.Add(
                selected |> Frame.Create
                |> positionWidget(20.0f, 0.5f, 50.0f, 0.0f, -20.0f, 1.0f, -20.0f, 1.0f) )
            let enabled = setting.Value
            for s in items do
                if enabled.Contains s |> not then
                    available.Add(ListOrderedItem(s, this))
            for s in enabled do
                selected.Add(ListOrderedItem(s, this))

        override this.OnSelect() =
            if available.Children.Count > 0 then
                this.HoverChild <- Some (available.Children.[0] :?> Selectable)
        override this.OnDeselect() =
            base.OnDeselect()
            this.HoverChild <- None
            setting.Value <- 
                this.Chosen.Children
                |> Seq.map (fun c -> (c :?> ListOrderedItem).Name)
                |> ResizeArray

        override this.Up() =
            match this.HoverChild with
            | Some c ->
                let l =
                    match c.Parent.Value with
                    | a when a = available -> a.Children
                    | e when e = selected -> e.Children
                    | _ -> failwith "impossible"
                let i = l.IndexOf(c)
                this.HoverChild <- Some (l.[(i + l.Count - 1) % l.Count] :?> Selectable)
            | None -> ()

        override this.Down() =
            match this.HoverChild with
            | Some c ->
                let l =
                    match c.Parent.Value with
                    | a when a = available -> a.Children
                    | e when e = selected -> e.Children
                    | _ -> failwith "impossible"
                let i = l.IndexOf(c)
                this.HoverChild <- Some (l.[(i + 1) % l.Count] :?> Selectable)
            | None -> ()

        override this.Left() =
            match this.HoverChild with
            | Some c ->
                let l =
                    match c.Parent.Value with
                    | a when a = available -> selected.Children
                    | e when e = selected -> available.Children
                    | _ -> failwith "impossible"
                //maybe todo: index matching when moving across?
                if l.Count > 0 then this.HoverChild <- Some (l.[0] :?> Selectable)
            | None -> ()
        override this.Right() = this.Left()

        member this.Chosen = selected
        member this.Available = available

module CardSelect =
    
        let markedIcon = "◆"
        let unmarkedIcon = "◇"
    
        let addIcon = "➕"
        let editIcon = "✎"
        let deleteIcon = "✕"

        let private h = 75.0f
    
        type Config<'T> =
            {
                NameFunc: 'T -> string
                CreateFunc: (unit -> unit) option
                DuplicateFunc: ('T -> unit) option
                EditFunc: ('T -> SelectionPage) option
                DeleteFunc: ('T -> unit) option
                ReorderFunc: ('T * bool -> unit) option
                MarkFunc: 'T * bool -> unit

                mutable Refresh: unit -> unit
            }
            static member Default : Config<'T> = 
                {
                    NameFunc = fun o -> o.ToString()
                    CreateFunc = None
                    DuplicateFunc = None
                    EditFunc = None
                    DeleteFunc = None
                    ReorderFunc = None
                    MarkFunc = ignore

                    Refresh = ignore
                }

        type HeaderCard<'T>(config: Config<'T>, parent: NavigateSelectable) as this =
            inherit NavigateSelectable()
            let mutable buttons = []
            let mutable index = -1
            do
                let addButton (b: Widget) =
                    let b = (b :?> Selectable)
                    buttons <- b :: buttons
                    this.Add b

                if Option.isSome config.CreateFunc then
                    new LittleButton(K addIcon, fun () -> config.CreateFunc.Value (); config.Refresh())
                    |> positionWidget(-h, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                    |> addButton
                    this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, h, 0.0f)
                else this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f)

            override this.SParent = Some (parent :> Selectable)
    
            override this.Up() = parent.Up()
            override this.Down() = parent.Down()

            override this.Left() =
                if index < 0 then index <- buttons.Length - 1
                else index <- index - 1
                if index < 0 then this.HoverChild <- None else this.HoverChild <- Some (buttons.[index])

            override this.Right() =
                if index = buttons.Length - 1 then index <- -1
                else index <- index + 1
                if index < 0 then this.HoverChild <- None else this.HoverChild <- Some (buttons.[index])
                
            override this.Draw() =
                if this.Selected then Draw.rect this.Bounds (Color.FromArgb(120, 255, 255, 255)) Sprite.Default
                elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 255, 255, 255)) Sprite.Default
                base.Draw()
    
        type Card<'T>(item: 'T, marked: bool, config: Config<'T>, add: string * SelectionPage -> unit, parent: NavigateSelectable) as this =
            inherit NavigateSelectable()
            let mutable buttons = []
            let mutable index = -1
            do
    
                let addButton (b: Widget) =
                    let b = (b :?> Selectable)
                    buttons <- b :: buttons
                    this.Add b
    
                new TextBox((fun () -> config.NameFunc item), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add
    
                let mutable x = -h
    
                new TextBox((fun () -> if marked then markedIcon else unmarkedIcon), K (Color.White, Color.Black), 0.5f)
                |> positionWidget(x, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add
    
                if Option.isSome config.DeleteFunc then
                    x <- x - h
                    new LittleButton(K deleteIcon, fun () -> config.DeleteFunc.Value item; config.Refresh())
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton
    
                if Option.isSome config.EditFunc then
                    x <- x - h
                    new LittleButton(
                        K editIcon,
                        fun () -> 
                            let page = config.EditFunc.Value item
                            add("EditItem", { page with Callback = fun () -> page.Callback(); config.Refresh() })
                        )
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton
    
                if Option.isSome config.DuplicateFunc then
                    x <- x - h
                    new LittleButton(K addIcon, fun () -> config.DuplicateFunc.Value item; config.Refresh())
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton
    
                this |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, h, 0.0f) |> ignore

            override this.SParent = Some (parent :> Selectable)
    
            override this.Up() = parent.Up()
            override this.Down() = parent.Down()

            override this.Left() =
                if index < 0 then index <- buttons.Length - 1
                else index <- index - 1
                if index < 0 then this.HoverChild <- None else this.HoverChild <- Some (buttons.[index])

            override this.Right() =
                if index = buttons.Length - 1 then index <- -1
                else index <- index + 1
                if index < 0 then this.HoverChild <- None else this.HoverChild <- Some (buttons.[index])
                
            override this.Update(elapsedTime, bounds) =
                if this.Selected && this.HoverChild = None && options.Hotkeys.Select.Value.Tapped() then
                    if marked then config.MarkFunc (item, false) else config.MarkFunc (item, true)
                    config.Refresh()
                base.Update(elapsedTime, bounds)
    
            override this.Draw() =
                if marked then Draw.rect this.Bounds (Style.accentShade(80, 1.0f, 0.0f)) Sprite.Default
                if this.Selected then Draw.rect this.Bounds (Color.FromArgb(120, 255, 255, 255)) Sprite.Default
                elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 255, 255, 255)) Sprite.Default
                base.Draw()
    
        type Selector<'T>(source: Setting<('T * bool) seq>, config: Config<'T>, add: string * SelectionPage -> unit) as this =
            inherit NavigateSelectable()
    
            let fc = FlowContainer()

            let refresh () =
                let index = match this.HoverChild with None -> 0 | Some x -> fc.Children.IndexOf x
                fc.Clear()

                fc.Add (HeaderCard(config, this))
                let items = source.Value |> Seq.map (fun (item, marked) -> Card<'T>(item, marked, config, add, this))
                items |> Seq.iter fc.Add

                let index = if index >= Seq.length items || index < 0 then 0 else index
                if this.Selected then this.HoverChild <- Some (fc.Children.[index] :?> Selectable)

            do
                this.Add fc
                config.Refresh <- fun () -> this.Synchronized refresh
                refresh()
    
            override this.Up() =
                match this.HoverChild with
                | Some s ->
                    let i = fc.Children.IndexOf s
                    this.HoverChild <- fc.Children.[(i - 1 + fc.Children.Count) % fc.Children.Count] :?> Selectable |> Some
                | None -> ()
            override this.Down() =
                match this.HoverChild with
                | Some s ->
                    let i = fc.Children.IndexOf s
                    this.HoverChild <- fc.Children.[(i + 1) % fc.Children.Count] :?> Selectable |> Some
                | None -> ()
    
            override this.OnSelect() =
                base.OnSelect()
                this.HoverChild <- Some (fc.Children.[0] :?> Selectable)

//todo: remove this and use new (above) system
module WatcherSelect =

    type WatcherSelectorItem<'T>(item: 'T, name, selector: WatcherSelector<'T>) as this =
        inherit ListSelectable(true)
        do
            new TextBox((fun () -> name ((this.Setting : Setting<'T>).Value)), K (Color.White, Color.Black), 0.0f)
            |> positionWidget(0.0f, 0.0f, 10.0f, 0.0f, 0.0f, 1.0f, -40.0f, 1.0f)
            |> this.Add

            new LittleButton(K <| Localisation.localise "options.wselect.Edit", fun () -> selector.EditItem this)
            |> positionWidget(20.0f, 0.0f, -40.0f, 1.0f, 140.0f, 0.0f, -10.0f, 1.0f)
            |> this.Add

            new LittleButton(K <| Localisation.localise "options.wselect.Duplicate", fun () -> this.Parent.Value.Synchronized(fun () -> this.Parent.Value.Add(WatcherSelectorItem(this.Setting.Value, name, selector))))
            |> positionWidget(160.0f, 0.0f, -40.0f, 1.0f, 280.0f, 0.0f, -10.0f, 1.0f)
            |> this.Add

            new LittleButton(K <| Localisation.localise "options.wselect.MakeMain", fun () -> selector.Main <- this)
            |> positionWidget(300.0f, 0.0f, -40.0f, 1.0f, 420.0f, 0.0f, -10.0f, 1.0f)
            |> this.Add

            new LittleButton(K <| Localisation.localise "options.wselect.Delete", fun () -> if selector.Main <> this then this.Destroy(); this.SParent.Value.SelectedChild <- None)
            |> positionWidget(440.0f, 0.0f, -40.0f, 1.0f, 560.0f, 0.0f, -10.0f, 1.0f)
            |> this.Add

            this |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 120.0f, 0.0f) |> ignore
        member val Setting = Setting.simple item
        override this.SParent = Some (selector :> Selectable)

        override this.Draw() =
            if selector.Main = this then Draw.rect this.Bounds (Style.accentShade(80, 1.0f, 0.0f)) Sprite.Default
            if this.Selected then Draw.rect this.Bounds (Color.FromArgb(120, 255, 255, 255)) Sprite.Default
            elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 255, 255, 255)) Sprite.Default
            base.Draw()

    and WatcherSelector<'T>(source: Setting<WatcherSelection<'T>>, editor: Setting<'T> -> SelectionPage, name: 'T -> string, add: string * SelectionPage -> unit) as this =
        inherit NavigateSelectable()
        let items = source.Value |> fun (a, b) -> a :: b |> List.map (fun x -> WatcherSelectorItem<'T>(x, name, this))
        let mutable currentMain = items.Head

        let fc = FlowContainer()
        do
            this.Add(fc)
            items |> List.iter fc.Add

        override this.Up() =
            match this.HoverChild with
            | Some s ->
                let i = fc.Children.IndexOf s
                this.HoverChild <- fc.Children.[(i - 1 + fc.Children.Count) % fc.Children.Count] :?> Selectable |> Some
            | None -> ()
        override this.Down() =
            match this.HoverChild with
            | Some s ->
                let i = fc.Children.IndexOf s
                this.HoverChild <- fc.Children.[(i + 1) % fc.Children.Count] :?> Selectable |> Some
            | None -> ()

        member this.EditItem(item: WatcherSelectorItem<'T>) = add("EditItem", editor item.Setting)
        member this.Main with get() = currentMain and set(v) = currentMain <- v

        override this.OnSelect() =
            this.HoverChild <- Some (currentMain :> Selectable)

        override this.OnDeselect() =
            base.OnDeselect()
            let x = currentMain.Setting.Value
            let xs = fc.Children |> Seq.map (fun w -> w :?> WatcherSelectorItem<'T>) |> List.ofSeq |> List.choose (fun i -> if currentMain = i then None else Some i.Setting.Value)
            source.Value <- (x, xs)