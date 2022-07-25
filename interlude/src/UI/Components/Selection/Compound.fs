namespace Interlude.UI.Components.Selection.Compound

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers
open Interlude.UI.Components.Selection.Buttons
open Interlude.UI.Components.Selection.Menu

module CardSelect =

    let private h = 75.0f
    
    type Config<'T> =
        {
            NameFunc: 'T -> string
            CreateFunc: (unit -> unit) option
            DuplicateFunc: ('T -> unit) option
            EditFunc: ('T -> unit) option
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
    
    type Card<'T>(item: 'T, marked: bool, config: Config<'T>, parent: NavigateSelectable) as this =
        inherit NavigateSelectable()
        let mutable buttons = []

        let onSelect () =
            if marked then config.MarkFunc (item, false) else config.MarkFunc (item, true)
            config.Refresh()

        do

            let addButton (b: Widget1) =
                let b = (b :?> Selectable)
                buttons <- b :: buttons
                this.Add b

            new Clickable((fun () -> onSelect()), fun b -> if b then this.Hover <- true)
            |> this.Add
    
            TextBox((fun () -> config.NameFunc item), K (Color.White, Color.Black), 0.0f)
                .Position { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ 0.0f }
            |> this.Add
    
            let mutable x = -h
    
            TextBox((fun () -> if marked then Icons.selected else Icons.unselected), K (Color.White, Color.Black), 0.5f)
                .Position { Left = 1.0f %+ x; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ 0.0f }
            |> this.Add
    
            if Option.isSome config.DeleteFunc then
                x <- x - h
                IconButton(Icons.delete, fun () -> config.DeleteFunc.Value item; config.Refresh())
                    .Position { Position.Default with Left = 1.0f %+ x; Right = 1.0f %+ (x + h) }
                |> addButton
    
            if Option.isSome config.EditFunc then
                x <- x - h
                IconButton(
                    Icons.edit,
                    fun () -> config.EditFunc.Value item)
                    .Position { Position.Default with Left = 1.0f %+ x; Right = 1.0f %+ (x + h) }
                |> addButton
    
            if Option.isSome config.DuplicateFunc then
                x <- x - h
                IconButton(Icons.add, fun () -> config.DuplicateFunc.Value item; config.Refresh())
                    .Position { Position.Default with Left = 1.0f %+ x; Right = 1.0f %+ (x + h) }
                |> addButton
    
            this.Position { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ h } |> ignore

        override this.SParent = Some (parent :> Selectable)
    
        override this.Up() = parent.Up()
        override this.Down() = parent.Down()

        override this.Left() =
            match this.HoverChild with
            | Some child ->
                let index = List.findIndex ((=) child) buttons
                if index = 0 then this.HoverChild <- None
                else this.HoverChild <- Some (buttons.[index - 1])
            | None ->
                if buttons.Length > 0 then
                    this.HoverChild <- Some (buttons.[buttons.Length - 1])

        override this.Right() =
            match this.HoverChild with
            | Some child ->
                let index = List.findIndex ((=) child) buttons
                if index = buttons.Length - 1 then this.HoverChild <- None
                else this.HoverChild <- Some (buttons.[index + 1])
            | None ->
                if buttons.Length > 0 then
                    this.HoverChild <- Some (buttons.[0])
                
        override this.Update(elapsedTime, bounds) =
            if this.Selected && this.HoverChild = None && (!|"select").Tapped() then onSelect()
            base.Update(elapsedTime, bounds)
    
        override this.Draw() =
            if marked then Draw.rect this.Bounds (Style.color(80, 1.0f, 0.0f))
            if this.Selected then Draw.rect this.Bounds (Color.FromArgb(120, 255, 255, 255))
            elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 255, 255, 255))
            base.Draw()
    
    type Selector<'T>(source: Setting<('T * bool) seq>, config: Config<'T>) as this =
        inherit NavigateSelectable()
    
        let fc = FlowContainer()

        let refresh () =
            let index = match this.HoverChild with None -> 0 | Some x -> fc.Children.IndexOf x
            fc.Clear()

            let items = source.Value |> Seq.map (fun (item, marked) -> Card<'T>(item, marked, config, this))
            items |> Seq.iter fc.Add

            if config.CreateFunc.IsSome then
                { new IconButton(Icons.add, fun () -> config.CreateFunc.Value (); config.Refresh())
                    with override _.SParent = Some this }
                    .Position( Position.Box(0.0f, 0.0f, h, h) )
                |> fc.Add

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

module CardView =
    
    let HEIGHT = 80.0f

    type Selection<'T> = { IsSelected: 'T -> bool; Select: ('T * bool) -> unit }
    type Action<'T> = { Icon: string; Act: 'T -> unit; Enabled: 'T -> bool }
    type Column<'T> = { Text: 'T -> string } // todo: add headers to columns ?
    type Config<'T> =
        {
            Columns: Column<'T> list
            Actions: Action<'T> list
            Selection: Selection<'T> option
            New: 'T -> unit option
        }

    type SelectionButton<'T>(item: 'T, selected: bool, config: Config<'T>) =
        inherit StaticWidget(NodeType.Button(fun () -> config.Selection.Value.Select(item, not selected)))

        override this.Draw() = Text.drawFillB(Style.baseFont, (if selected then Icons.selected else Icons.unselected), this.Bounds, Style.text(), Alignment.CENTER)

    type ActionButton<'T>(item: 'T, action: Action<'T>) as this =
        inherit StaticWidget(NodeType.Button(this.Action))

        let enabled = action.Enabled item

        member private this.Action() = if enabled then action.Act item

        override this.Draw() = 
            if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            Text.drawFillB(Style.baseFont, action.Icon, this.Bounds, (if enabled then Style.text() else (Color.Gray, Color.Black)), Alignment.CENTER)

    type Card<'T>(item: 'T, config: Config<'T>) as this =
        inherit StaticContainer(NodeType.Switch(this._container))

        let container = FlowContainer.RightToLeft<StaticWidget>(HEIGHT)
        let select_button =
            match config.Selection with
            | Some sel -> SelectionButton(item, sel.IsSelected item, config) |> Some
            | None -> None

        do
            if select_button.IsSome then container.Add select_button.Value
            for action in config.Actions do
                container.Add(ActionButton(item, action))
            this |* container

        override this.Draw() =
            if select_button.IsSome && select_button.Value.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

        member private this._container() = container