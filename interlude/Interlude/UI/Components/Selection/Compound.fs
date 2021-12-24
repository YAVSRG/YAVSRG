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
    
        type Card<'T>(item: 'T, marked: bool, config: Config<'T>, add: string * SelectionPage -> unit, parent: NavigateSelectable) as this =
            inherit NavigateSelectable()
            let mutable buttons = []

            let onSelect () =
                if marked then config.MarkFunc (item, false) else config.MarkFunc (item, true)
                config.Refresh()

            do

                let addButton (b: Widget) =
                    let b = (b :?> Selectable)
                    buttons <- b :: buttons
                    this.Add b

                new Clickable((fun () -> onSelect()), fun b -> if b then this.Hover <- true)
                |> this.Add
    
                new TextBox((fun () -> config.NameFunc item), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add
    
                let mutable x = -h
    
                new TextBox((fun () -> if marked then markedIcon else unmarkedIcon), K (Color.White, Color.Black), 0.5f)
                |> positionWidget(x, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add
    
                if Option.isSome config.DeleteFunc then
                    x <- x - h
                    new IconButton(deleteIcon, fun () -> config.DeleteFunc.Value item; config.Refresh())
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton
    
                if Option.isSome config.EditFunc then
                    x <- x - h
                    new IconButton(
                        editIcon,
                        fun () -> 
                            let page = config.EditFunc.Value item
                            add("EditItem", { page with Callback = fun () -> page.Callback(); config.Refresh() })
                        )
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton
    
                if Option.isSome config.DuplicateFunc then
                    x <- x - h
                    new IconButton(addIcon, fun () -> config.DuplicateFunc.Value item; config.Refresh())
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton
    
                this |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, h, 0.0f) |> ignore

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
                    this.HoverChild <- Some (buttons.[buttons.Length - 1])

            override this.Right() =
                match this.HoverChild with
                | Some child ->
                    let index = List.findIndex ((=) child) buttons
                    if index = buttons.Length - 1 then this.HoverChild <- None
                    else this.HoverChild <- Some (buttons.[index + 1])
                | None ->
                    this.HoverChild <- Some (buttons.[0])
                
            override this.Update(elapsedTime, bounds) =
                if this.Selected && this.HoverChild = None && options.Hotkeys.Select.Value.Tapped() then onSelect()
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

                let items = source.Value |> Seq.map (fun (item, marked) -> Card<'T>(item, marked, config, add, this))
                items |> Seq.iter fc.Add

                if config.CreateFunc.IsSome then
                    { new IconButton(addIcon, fun () -> config.CreateFunc.Value (); config.Refresh())
                       with override _.SParent = Some this }
                    |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, h, 0.0f, h, 0.0f)
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