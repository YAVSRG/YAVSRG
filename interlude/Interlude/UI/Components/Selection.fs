namespace Interlude.UI.Components

open System
open System.Drawing
open OpenTK
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Interlude
open Interlude.Utils
open Interlude.Graphics
open Interlude.Input
open Interlude.Options
open Interlude.UI
open Interlude.UI.Animation
open Interlude.UI.Components

module Selection =

    (*
        Fancy selection framework
    *)

    type Selectable() =
        inherit Widget()

        (*
            - There is only one item hovered per widget
            - This one item can be marked as selected
            Invariants:
                - (1) Only at most 1 leaf can be hovered
                - (2) Selected leaves are a subset, so at most 1 leaf can be selected
                - (3) The leaf that is hovered, if it exists, implies all its ancestors are selected
                - (4) The leaf that is hovered, if it exists, implies all its non-ancestors are not hovered
        *)

        let mutable hoverChild: Selectable option = None
        let mutable hoverSelected: bool = false

        abstract member SParent: Selectable option
        default this.SParent =
            match this.Parent with
            | Some p ->
                match p with
                | :? Selectable as p -> Some p
                | _ -> None
            | None -> None

        member this.SelectedChild
            with get() = if hoverSelected then hoverChild else None
            and set(value) =
                match value with
                | Some v ->
                    match this.SelectedChild with
                    | Some c ->
                        if v <> c then
                            c.OnDeselect()
                            c.OnDehover()
                            hoverChild <- value
                            v.OnSelect()
                    | None ->
                        hoverChild <- value
                        hoverSelected <- true
                        match this.SParent with
                        | Some p -> p.SelectedChild <- Some this
                        | None -> ()
                        v.OnSelect()
                | None -> this.HoverChild <- None

        member this.HoverChild
            with get() = hoverChild
            and set(value) =
                match this.HoverChild with
                | Some c ->
                    if hoverSelected then c.OnDeselect()
                    if Some c <> value then c.OnDehover()
                | None -> ()
                hoverChild <- value
                hoverSelected <- false
                if value.IsSome then
                    match this.SParent with
                    | Some p -> p.SelectedChild <- Some this
                    | None -> ()

        member this.Selected
            with get() =
                match this.SParent with
                | Some p -> p.SelectedChild = Some this
                | None -> true
            and set(value) =
                match this.SParent with
                | Some p -> if value then p.SelectedChild <- Some this elif this.Hover then p.HoverChild <- Some this
                | None -> ()

        member this.Hover
            with get() =
                match this.SParent with
                | Some p -> p.HoverChild = Some this
                | None -> true
            and set(value) =
                match this.SParent with
                | Some p -> if value then p.HoverChild <- Some this elif this.Hover then p.HoverChild <- None
                | None -> ()

        abstract member OnSelect: unit -> unit
        default this.OnSelect() = ()

        abstract member OnDeselect: unit -> unit
        default this.OnDeselect() = ()

        abstract member OnDehover: unit -> unit
        default this.OnDehover() = this.HoverChild <- None

    type NavigateSelectable() =
        inherit Selectable()

        let mutable disposed = false

        abstract member Left: unit -> unit
        default this.Left() = ()

        abstract member Up: unit -> unit
        default this.Up() = ()

        abstract member Right: unit -> unit
        default this.Right() = ()

        abstract member Down: unit -> unit
        default this.Down() = ()

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if not disposed && this.Selected && this.SelectedChild.IsNone then
                if options.Hotkeys.Previous.Value.Tapped() then this.Left()
                elif options.Hotkeys.Up.Value.Tapped() then this.Up()
                elif options.Hotkeys.Next.Value.Tapped() then this.Right()
                elif options.Hotkeys.Down.Value.Tapped() then this.Down()
                elif options.Hotkeys.Select.Value.Tapped() then this.SelectedChild <- this.HoverChild
                elif options.Hotkeys.Exit.Value.Tapped() then this.Selected <- false

        override this.Dispose() = base.Dispose(); disposed <- true

    type ListSelectable(horizontal) =
        inherit NavigateSelectable()

        let items = ResizeArray<Selectable>()
        let mutable lastHover = None

        member this.Previous() =
            match this.HoverChild with
            | None -> Logging.Debug "No hoverchild for this ListSelectable, there should always be one"
            | Some w -> let i = (items.IndexOf w - 1 + items.Count) % items.Count in this.HoverChild <- Some items.[i]

        member this.Next() =
            match this.HoverChild with
            | None -> Logging.Debug "No hoverchild for this ListSelectable, there should always be one"
            | Some w -> let i = (items.IndexOf w + 1) % items.Count in this.HoverChild <- Some items.[i]

        override this.Add(c) =
            base.Add(c)
            match c with
            | :? Selectable as c -> items.Add c
            | _ -> ()

        override this.Remove(c) =
            base.Remove(c)
            match c with
            | :? Selectable as c -> items.Remove c |> ignore
            | _ -> ()

        override this.OnSelect() = base.OnSelect(); if (match lastHover with None -> true | Some l -> not (items.Contains l)) then this.HoverChild <- Some items.[0] else this.HoverChild <- lastHover
        override this.OnDeselect() = base.OnDeselect(); lastHover <- this.HoverChild; this.HoverChild <- None
        override this.OnDehover() = base.OnDehover(); for i in items do i.OnDehover()

        override this.Left() = if horizontal then this.Previous()
        override this.Right() = if horizontal then this.Next()
        override this.Up() = if not horizontal then this.Previous()
        override this.Down() = if not horizontal then this.Next()

        override this.Clear() = base.Clear(); items.Clear()

    type FlowSelectable(height, spacing) as this =
        inherit Selectable()

        let mutable h = 0.0f
        let fc = new FlowContainer()
        let ls =
            { new ListSelectable(false) with
                override _.SParent = this.SParent
                override this.Up() = base.Up(); Option.iter fc.ScrollTo this.HoverChild
                override this.Down() = base.Down(); Option.iter fc.ScrollTo this.HoverChild }

        do
            fc.Add ls
            this.Add fc

        override this.Add(c) =
            if c = (fc :> Widget) then base.Add c
            else
                c |> positionWidget(0.0f, 0.0f, h, 0.0f, 0.0f, 1.0f, h + height, 0.0f) |> ls.Add
                h <- h + height + spacing
                ls.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, h, 0.0f)

        override this.Remove(c) =
            ls.Remove c
            h <- 0.0f
            for child in ls.Children do
                child.Reposition(0.0f, 0.0f, h, 0.0f, 0.0f, 1.0f, h + height, 0.0f)
                h <- h + height + spacing
            ls.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, h, 0.0f)

        override this.OnSelect() = base.OnSelect(); ls.Selected <- true

        override this.Clear() = ls.Clear()

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)

    (*
        Specific widgets to actually build options screen
    *)

    type SelectionPage =
        {
            Content: (string * SelectionPage -> unit) -> Selectable
            Callback: unit -> unit
        }

    type BigButton(label, icon, onClick) as this =
        inherit Selectable()
        do
            this.Add(Frame((fun () -> Style.accentShade(180, 0.9f, 0.0f)), (fun () -> if this.Hover then Color.White else Color.Transparent)))
            this.Add(TextBox(K label, K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 0.8f))
            this.Add(TextBox(K ([|"❖";"✎";"♛";"⌨";"⚒"|].[icon]), K (Color.White, Color.Black), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.05f, 0.0f, 1.0f, 0.0f, 0.7f))
            this.Add(Clickable((fun () -> this.Selected <- true), fun b -> if b then this.Hover <- true))

        override this.OnSelect() =
            this.Selected <- false
            onClick()

    type LittleButton(label, onClick) as this =
        inherit Selectable()
        do
            this.Add(Frame(Color.FromArgb(80, 255, 255, 255), ()))
            this.Add(TextBox(label, (fun () -> ((if this.Hover then Style.accentShade(255, 1.0f, 0.7f) else Color.White), Color.Black)), 0.5f))
            this.Add(Clickable((fun () -> this.Selected <- true), fun b -> if b then this.Hover <- true))
        override this.OnSelect() =
            this.Selected <- false
            onClick()
        static member FromEnum<'T when 'T: enum<int>>(label: string, setting: Setting<'T>, onClick) =
            let names = Enum.GetNames(typeof<'T>)
            let values = Enum.GetValues(typeof<'T>) :?> 'T array
            let mutable i = array.IndexOf(values, setting.Value)
            LittleButton((fun () -> sprintf "%s: %s" label names.[i]),
                (fun () -> i <- (i + 1) % values.Length; setting.Value <- values.[i]; onClick()))

    type CardButton(title, subtitle, highlight, onClick, colorFunc) as this =
        inherit Selectable()

        do
            if subtitle <> "" then
                TextBox(K title, K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.6f)
                |> this.Add

                TextBox(K subtitle, K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add
            else TextBox(K title, K (Color.White, Color.Black), 0.0f) |> this.Add

            Clickable(
                (fun () -> if this.SParent.Value.Selected then this.Selected <- true),
                (fun b -> if b && this.SParent.Value.Selected then this.Hover <- true))
            |> this.Add

        new(title, subtitle, highlight, onClick) = CardButton(title, subtitle, highlight, onClick, fun () -> Style.accentShade(255, 1.0f, 0.0f))

        override this.Draw() =
            let hi = colorFunc()
            let lo = Color.FromArgb(100, hi)
            let e = highlight()
            Draw.quad (Quad.ofRect this.Bounds)
                (struct((if this.Hover then hi else lo), (if e then hi else lo), (if e then hi else lo), if this.Hover then hi else lo))
                Sprite.DefaultQuad
            base.Draw()

        override this.OnSelect() =
            base.OnSelect()
            onClick()
            this.Selected <- false

    type Selector(items: string array, setting: Setting<int>) as this =
        inherit NavigateSelectable()
        let fd() = setting.Value <- (setting.Value + 1) % items.Length
        let bk() = setting.Value <- (setting.Value + items.Length - 1) % items.Length
        do
            this.Add(new TextBox((fun () -> items.[setting.Value]), K (Color.White, Color.Black), 0.0f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
            this.Add(new Clickable((fun () -> (if not this.Selected then this.Selected <- true); fd()), fun b -> if b then this.Hover <- true))

        override this.Left() = bk()
        override this.Down() = bk()
        override this.Up() = fd()
        override this.Right() = fd()

        static member FromArray<'T>(names: string array, values: 'T array, setting: Setting<'T>) =
            new Selector(names,
                Setting.map
                    (fun e -> Array.IndexOf(values, e))
                    (fun i -> values.[i])
                    setting)

        static member FromEnum<'T>(setting: Setting<'T>) =
            let names = Enum.GetNames(typeof<'T>)
            let values = Enum.GetValues(typeof<'T>) :?> 'T array
            Selector.FromArray(names, values, setting)

        static member FromBool(setting: Setting<bool>) =
            new Selector([|"◇" ; "◆"|],
                Setting.map
                    (fun b -> if b then 1 else 0)
                    (fun i -> i > 0)
                    setting)

    type Slider<'T>(setting: Setting.Bounded<'T>, incr: float32) as this =
        inherit NavigateSelectable()
        let TEXTWIDTH = 130.0f
        let color = AnimationFade 0.5f
        let mutable dragging = false
        
        let getPercent (setting: Setting.Bounded<'T>) =
            let (Setting.Bounds (lo, hi)) = setting.Config
            let (lo, hi) = (Convert.ToSingle lo, Convert.ToSingle hi)
            let value = Convert.ToSingle setting.Value
            (value - lo) / (hi - lo)

        let setPercent (v: float32) (setting: Setting.Bounded<'T>) =
            let (Setting.Bounds (lo, hi)) = setting.Config
            let (lo, hi) = (Convert.ToSingle lo, Convert.ToSingle hi)
            setting.Value <- Convert.ChangeType((hi - lo) * v + lo, typeof<'T>) :?> 'T

        let chPercent v = setPercent (getPercent setting + v) setting
        do
            this.Animation.Add color
            this.Add(new TextBox((fun () -> setting.Value.ToString()), K (Color.White, Color.Black), 0.0f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, TEXTWIDTH, 0.0f, 0.0f, 1.0f))
            this.Reposition(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f)
            this.Add(new Clickable((fun () -> this.Selected <- true; dragging <- true), fun b -> color.Target <- if b then this.Hover <- true; 0.8f else 0.5f))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            let struct (l, t, r, b) = Rect.trimLeft TEXTWIDTH this.Bounds
            if this.Selected then
                if (Mouse.Held(MouseButton.Left) && dragging) then
                    let amt = (Mouse.X() - l) / (r - l)
                    setPercent amt setting
                else dragging <- false

        override this.Left() = chPercent(-incr)
        override this.Up() = chPercent(incr * 10.0f)
        override this.Right() = chPercent(incr)
        override this.Down() = chPercent(-incr * 10.0f)

        override this.Draw() =
            let v = getPercent setting
            let struct (l, t, r, b) = Rect.trimLeft TEXTWIDTH this.Bounds
            let cursor = Rect.create (l + (r - l) * v) t (l + (r - l) * v) b |> Rect.expand(10.0f, -10.0f)
            let m = (b + t) * 0.5f
            Draw.rect (Rect.create l (m - 10.0f) r (m + 10.0f)) (Style.accentShade(255, 1.0f, 0.0f)) Sprite.Default
            Draw.rect cursor (Style.accentShade(255, 1.0f, color.Value)) Sprite.Default
            base.Draw()

    type TextField(setting: Setting<string>) as this =
        inherit NavigateSelectable()
        let color = AnimationFade 0.5f
        do
            this.Animation.Add color
            this.Add(new TextBox(setting.Get, (fun () -> Style.accentShade(int (color.Value * 255.0f), 1.0f, color.Value), Color.Black), 0.0f))
            this.Add(new Clickable((fun () -> if not this.Selected then this.Selected <- true), fun b -> if b then this.Hover <- true))

        override this.OnSelect() =
            base.OnSelect()
            color.Target <- 1.0f
            Input.setInputMethod (setting, fun () -> this.Selected <- false)

        override this.OnDeselect() =
            base.OnDeselect()
            color.Target <- 0.5f

    type ColorPicker(color: Setting<byte>) as this =
        inherit NavigateSelectable()
        let sprite = Themes.getTexture "note"
        let n = byte sprite.Rows
        let fd() = Setting.app (fun x -> (x + n - 1uy) % n) color
        let bk() = Setting.app (fun x -> (x + 1uy) % n) color
        do this.Add(new Clickable((fun () -> (if not this.Selected then this.Selected <- true); fd ()), fun b -> if b then this.Hover <- true))

        override this.Draw() =
            base.Draw()
            if this.Selected then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.5f)) Sprite.Default
            elif this.Hover then Draw.rect this.Bounds (Style.accentShade(120, 1.0f, 0.8f)) Sprite.Default
            Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf Color.White) (Sprite.gridUV (3, int color.Value) sprite)

        override this.Left() = bk()
        override this.Up() = fd()
        override this.Right() = fd()
        override this.Down() = bk()

    type KeyBinder(setting: Setting<Bind>, allowModifiers) as this =
        inherit Selectable()
        do
            this.Add(new TextBox((fun () -> setting.Value.ToString()), (fun () -> (if this.Selected then Style.accentShade(255, 1.0f, 0.0f) else Color.White), Color.Black), 0.5f) |> positionWidgetA(0.0f, 40.0f, 0.0f, -40.0f))
            this.Add(new Clickable((fun () -> if not this.Selected then this.Selected <- true), fun b -> if b then this.Hover <- true))

        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (Style.accentShade(180, 1.0f, 0.5f)) Sprite.Default
            elif this.Hover then Draw.rect this.Bounds (Style.accentShade(120, 1.0f, 0.8f)) Sprite.Default
            Draw.rect (Rect.expand(0.0f, -40.0f) this.Bounds) (Style.accentShade(127, 0.8f, 0.0f)) Sprite.Default
            base.Draw()

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if this.Selected then
                match Input.consumeAny InputEvType.Press with
                | ValueNone -> ()
                | ValueSome b ->
                    match b with
                    | Key (k, (ctrl, _, shift)) ->
                        if k = Keys.Escape then if allowModifiers then setting.Value <- Dummy
                        elif allowModifiers then setting.Value <- Key (k, (ctrl, false, shift))
                        else setting.Value <- Key (k, (false, false, false))
                        this.Selected <- false
                    | _ -> ()

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

    (*
        Utils for constructing menus easily
    *)

    let localiseOption s = Localisation.localise("options.name." + s)
    let localiseTooltip s = Localisation.localise("options.tooltip." + s)

    let row xs =
        let r = ListSelectable(true)
        List.iter r.Add xs; r

    let column xs =
        let c = ListSelectable(false)
        List.iter c.Add xs; c

    let refreshRow number cons =
        let r = ListSelectable(true)
        let refresh() =
            r.Clear()
            let n = number()
            for i in 0 .. (n - 1) do
                r.Add(cons i n)
        refresh()
        r, refresh

    let refreshChoice (options: string array) (widgets: Widget array array) (setting: Setting<int>) =
        let rec newSetting =
            {
                Set =
                    fun x ->
                        for w in widgets.[setting.Value] do selector.SParent.Value.SParent.Value.Remove w
                        for w in widgets.[x] do selector.SParent.Value.SParent.Value.Add w
                        setting.Value <- x
                Get = setting.Get
                Config = setting.Config
            }
        and selector : Selector = new Selector(options, newSetting)
        selector.Synchronized(fun () -> newSetting.Value <- newSetting.Value)
        selector

    let PRETTYTEXTWIDTH = 500.0f
    let PRETTYHEIGHT = 80.0f
    let PRETTYWIDTH = 1200.0f

    type Divider() =
        inherit Widget()

        member this.Position(y) =
            this |> positionWidget(100.0f, 0.0f, y - 5.0f, 0.0f, 100.0f + PRETTYWIDTH, 0.0f, y + 5.0f, 0.0f)

        override this.Draw() =
            base.Draw()
            Draw.quad (Quad.ofRect this.Bounds) (struct(Color.White, Color.FromArgb(0, 255, 255, 255), Color.FromArgb(0, 255, 255, 255), Color.White)) Sprite.DefaultQuad

    type PrettySetting(name, widget: Selectable) as this =
        inherit Selectable()

        let mutable widget = widget

        do
            widget
            |> positionWidgetA(PRETTYTEXTWIDTH, 0.0f, 0.0f, 0.0f)
            |> this.Add

            TextBox(K (localiseOption name + ":"), (fun () -> ((if this.Selected then Style.accentShade(255, 1.0f, 0.2f) else Color.White), Color.Black)), 0.0f)
            |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, PRETTYTEXTWIDTH, 0.0f, PRETTYHEIGHT, 0.0f)
            |> this.Add

            TooltipRegion(localiseTooltip name) |> this.Add
    
        member this.Position(y, width, height) =
            this |> positionWidget(100.0f, 0.0f, y, 0.0f, 100.0f + width, 0.0f, y + height, 0.0f)
    
        member this.Position(y, width) = this.Position(y, width, PRETTYHEIGHT)
        member this.Position(y) = this.Position(y, PRETTYWIDTH)

        override this.Draw() =
            if this.Selected then Draw.rect this.Bounds (Color.FromArgb(180, 0, 0, 0)) Sprite.Default
            elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 0, 0, 0)) Sprite.Default
            base.Draw()
    
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if widget.Hover && not widget.Selected && this.Selected then this.HoverChild <- None; this.Hover <- true
        
        override this.OnSelect() = if not widget.Hover then widget.Selected <- true
        override this.OnDehover() = base.OnDehover(); widget.OnDehover()

        member this.Refresh(w: Selectable) =
            widget.Destroy()
            widget <- w
            this.Add(widget |> positionWidgetA(PRETTYTEXTWIDTH, 0.0f, 0.0f, 0.0f))

    type PrettyButton(name, action) as this =
        inherit Selectable()
        do
            TextBox(K (localiseOption name + "  >"), (fun () -> ((if this.Hover then Style.accentShade(255, 1.0f, 0.5f) else Color.White), Color.Black)), 0.0f) |> this.Add
            Clickable((fun () -> this.Selected <- true), (fun b -> if b then this.Hover <- true)) |> this.Add
            TooltipRegion(localiseTooltip name) |> this.Add
        override this.OnSelect() = action(); this.Selected <- false
        override this.Draw() =
            if this.Hover then Draw.rect this.Bounds (Color.FromArgb(120, 0, 0, 0)) Sprite.Default
            base.Draw()
        member this.Position(y) = this |> positionWidget(100.0f, 0.0f, y, 0.0f, 100.0f + PRETTYWIDTH, 0.0f, y + PRETTYHEIGHT, 0.0f)

    type SelectionMenu(topLevel: SelectionPage) as this =
        inherit Dialog()
    
        let stack: (Selectable * (unit -> unit)) option array = Array.create 12 None
        let mutable namestack = []
        let mutable name = ""
        let body = Widget()

        let wrapper main =
            let mutable disposed = false
            let w = 
                { new Selectable() with

                    override this.Update(elapsedTime, bounds) =
                        if disposed then this.HoverChild <- None
                        base.Update(elapsedTime, bounds)
                        if not disposed then
                            Input.absorbAll()

                    override this.VisibleBounds = this.Bounds
                    override this.Dispose() = base.Dispose(); disposed <- true
                }
            w.Add main
            w.SelectedChild <- Some main
            w
    
        let rec add (label, page) =
            let n = List.length namestack
            namestack <- localiseOption label :: namestack
            name <- String.Join(" > ", List.rev namestack)
            let w = wrapper (page.Content add)
            match stack.[n] with
            | None -> ()
            | Some (x, _) -> x.Destroy()
            stack.[n] <- Some (w, page.Callback)
            body.Add w
            let n = float32 n + 1.0f
            w.Reposition(0.0f, Render.vheight * n, 0.0f, Render.vheight * n)
            body.Move(0.0f, -Render.vheight * n, 0.0f, -Render.vheight * n)
    
        let back() =
            namestack <- List.tail namestack
            name <- String.Join(" > ", List.rev namestack)
            let n = List.length namestack
            let (w, callback) = stack.[n].Value in w.Dispose(); callback()
            let n = float32 n
            body.Move(0.0f, -Render.vheight * n, 0.0f, -Render.vheight * n)
    
        do
            this.Add body
            this.Add(TextBox((fun () -> name), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(20.0f, 0.0f, 20.0f, 0.0f, 0.0f, 1.0f, 100.0f, 0.0f))
            add ("Options", topLevel)
    
        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            match List.length namestack with
            | 0 -> this.BeginClose()
            | n -> if (fst stack.[n - 1].Value).SelectedChild.IsNone then back()
    
        override this.OnClose() = ()