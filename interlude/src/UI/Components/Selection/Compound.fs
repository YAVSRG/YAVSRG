namespace Interlude.UI.Components.Selection.Compound

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Containers
open Interlude.UI.Components.Selection.Buttons
open Interlude.UI.Components.Selection.Menu

//module CardSelect =

//    let private h = 75.0f
    
//    type Config<'T> =
//        {
//            NameFunc: 'T -> string
//            CreateFunc: (unit -> unit) option
//            DuplicateFunc: ('T -> unit) option
//            EditFunc: ('T -> IMenu -> Page) option
//            DeleteFunc: ('T -> unit) option
//            ReorderFunc: ('T * bool -> unit) option
//            MarkFunc: 'T * bool -> unit

//            mutable Refresh: unit -> unit
//        }
//        static member Default : Config<'T> = 
//            {
//                NameFunc = fun o -> o.ToString()
//                CreateFunc = None
//                DuplicateFunc = None
//                EditFunc = None
//                DeleteFunc = None
//                ReorderFunc = None
//                MarkFunc = ignore

//                Refresh = ignore
//            }
    
//    type Card<'T>(item: 'T, marked: bool, config: Config<'T>, menu: IMenu, parent: NavigateSelectable) as this =
//        inherit NavigateSelectable()
//        let mutable buttons = []

//        let onSelect () =
//            if marked then config.MarkFunc (item, false) else config.MarkFunc (item, true)
//            config.Refresh()

//        do

//            let addButton (b: Widget1) =
//                let b = (b :?> Selectable)
//                buttons <- b :: buttons
//                this.Add b

//            new Clickable((fun () -> onSelect()), fun b -> if b then this.Hover <- true)
//            |> this.Add
    
//            TextBox((fun () -> config.NameFunc item), K (Color.White, Color.Black), 0.0f)
//                .Position { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ 0.0f }
//            |> this.Add
    
//            let mutable x = -h
    
//            TextBox((fun () -> if marked then Icons.selected else Icons.unselected), K (Color.White, Color.Black), 0.5f)
//                .Position { Left = 1.0f %+ x; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 1.0f %+ 0.0f }
//            |> this.Add
    
//            if Option.isSome config.DeleteFunc then
//                x <- x - h
//                IconButton(Icons.delete, fun () -> config.DeleteFunc.Value item; config.Refresh())
//                    .Position { Position.Default with Left = 1.0f %+ x; Right = 1.0f %+ (x + h) }
//                |> addButton
    
//            if Option.isSome config.EditFunc then
//                x <- x - h
//                IconButton(
//                    Icons.edit,
//                    fun () -> 
//                        let page = config.EditFunc.Value item
//                        add( E (config.NameFunc item), { page with Callback = fun () -> page.Callback(); config.Refresh() })
//                    )
//                    .Position { Position.Default with Left = 1.0f %+ x; Right = 1.0f %+ (x + h) }
//                |> addButton
    
//            if Option.isSome config.DuplicateFunc then
//                x <- x - h
//                IconButton(Icons.add, fun () -> config.DuplicateFunc.Value item; config.Refresh())
//                    .Position { Position.Default with Left = 1.0f %+ x; Right = 1.0f %+ (x + h) }
//                |> addButton
    
//            this.Position { Left = 0.0f %+ 0.0f; Top = 0.0f %+ 0.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ h } |> ignore

//        override this.SParent = Some (parent :> Selectable)
    
//        override this.Up() = parent.Up()
//        override this.Down() = parent.Down()

//        override this.Left() =
//            match this.HoverChild with
//            | Some child ->
//                let index = List.findIndex ((=) child) buttons
//                if index = 0 then this.HoverChild <- None
//                else this.HoverChild <- Some (buttons.[index - 1])
//            | None ->
//                if buttons.Length > 0 then
//                    this.HoverChild <- Some (buttons.[buttons.Length - 1])

//        override this.Right() =
//            match this.HoverChild with
//            | Some child ->
//                let index = List.findIndex ((=) child) buttons
//                if index = buttons.Length - 1 then this.HoverChild <- None
//                else this.HoverChild <- Some (buttons.[index + 1])
//            | None ->
//                if buttons.Length > 0 then
//                    this.HoverChild <- Some (buttons.[0])
                
//        override this.Update(elapsedTime, bounds) =
//            if this.Selected && this.HoverChild = None && (!|"select").Tapped() then onSelect()
//            base.Update(elapsedTime, bounds)
    
//        override this.Draw() =
//            if marked then Draw.rect this.Bounds (Style.accentShade(80, 1.0f, 0.0f))
//            if this.Selected then Draw.rect this.Bounds (Color.FromArgb(120, 255, 255, 255))
//            elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 255, 255, 255))
//            base.Draw()
    
//    type Selector<'T>(source: Setting<('T * bool) seq>, config: Config<'T>, menu: IMenu) as this =
//        inherit NavigateSelectable()
    
//        let fc = FlowContainer()

//        let refresh () =
//            let index = match this.HoverChild with None -> 0 | Some x -> fc.Children.IndexOf x
//            fc.Clear()

//            let items = source.Value |> Seq.map (fun (item, marked) -> Card<'T>(item, marked, config, menu, this))
//            items |> Seq.iter fc.Add

//            if config.CreateFunc.IsSome then
//                { new IconButton(Icons.add, fun () -> config.CreateFunc.Value (); config.Refresh())
//                    with override _.SParent = Some this }
//                    .Position( Position.Box(0.0f, 0.0f, h, h) )
//                |> fc.Add

//            let index = if index >= Seq.length items || index < 0 then 0 else index
//            if this.Selected then this.HoverChild <- Some (fc.Children.[index] :?> Selectable)

//        do
//            this.Add fc
//            config.Refresh <- fun () -> this.Synchronized refresh
//            refresh()
    
//        override this.Up() =
//            match this.HoverChild with
//            | Some s ->
//                let i = fc.Children.IndexOf s
//                this.HoverChild <- fc.Children.[(i - 1 + fc.Children.Count) % fc.Children.Count] :?> Selectable |> Some
//            | None -> ()
//        override this.Down() =
//            match this.HoverChild with
//            | Some s ->
//                let i = fc.Children.IndexOf s
//                this.HoverChild <- fc.Children.[(i + 1) % fc.Children.Count] :?> Selectable |> Some
//            | None -> ()
    
//        override this.OnSelect() =
//            base.OnSelect()
//            this.HoverChild <- Some (fc.Children.[0] :?> Selectable)