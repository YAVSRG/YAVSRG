namespace Interlude.UI.Components.Selection.Compound

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Utils
open Interlude.UI

module Grid =
    
    type IGrid =
        abstract member Refresh: unit -> unit

    let HEIGHT = 80.0f

    type Selection<'T> = { IsSelected: 'T -> bool; Select: ('T * bool) -> unit }
    type Action<'T> = { Icon: string; Act: 'T -> unit; Enabled: 'T -> bool }
    type Column<'T> = { Text: 'T -> string } // todo: add headers to columns ?
    type Config<'T> =
        {
            Columns: Column<'T> list
            Actions: Action<'T> list
            Selection: Selection<'T> option
            New: (unit -> unit) option
        }
        static member Default : Config<'T> = { Columns = []; Actions = []; Selection = None; New = None }
        member this.WithColumn(f: 'T -> string) = { this with Columns = { Text = f } :: this.Columns }
        member this.WithAction(icon: string, action: 'T -> unit, filter: 'T -> bool) = { this with Actions = { Icon = icon; Act = action; Enabled = filter } :: this.Actions }
        member this.WithAction(icon: string, action: 'T -> unit) = { this with Actions = { Icon = icon; Act = action; Enabled = K true } :: this.Actions }
        member this.WithNew(action: unit -> unit) = { this with New = Some action }
        member this.WithSelection(get: 'T -> bool, set: ('T * bool) -> unit) = { this with Selection = Some { IsSelected = get; Select = set } }

    type SelectionButton<'T>(item: 'T, selected: bool, config: Config<'T>, main: IGrid) =
        inherit StaticWidget(NodeType.Button(fun () -> config.Selection.Value.Select(item, not selected); main.Refresh()))

        override this.Draw() = Text.drawFillB(Style.baseFont, (if selected then Icons.selected else Icons.unselected), this.Bounds, Style.text(), Alignment.CENTER)

    type ActionButton<'T>(item: 'T, action: Action<'T>, main: IGrid) as this =
        inherit StaticContainer(NodeType.Button(this.Action))

        let enabled = action.Enabled item

        do
            this 
            |+ Text(Icons.add, Color = if enabled then Style.text else K (Color.Gray, Color.Black))
            |* Clickable.Focus this

        member private this.Action() = 
            if enabled then 
                action.Act item
                main.Refresh()

        override this.Draw() = 
            if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

    type CreateButton<'T>(func: unit -> unit, main: IGrid) as this =
        inherit StaticContainer(NodeType.Button (F func main.Refresh))

        do
            this 
            |+ Text Icons.add
            |* Clickable.Focus this

        override this.Draw() = 
            if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

    type Card<'T>(item: 'T, config: Config<'T>, main: IGrid) as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this._container()))

        let container = FlowContainer.RightToLeft<StaticWidget>(HEIGHT)
        let select_button =
            match config.Selection with
            | Some sel -> SelectionButton(item, sel.IsSelected item, config, main) |> Some
            | None -> None

        do
            let mutable w = 0.0f
            if select_button.IsSome then 
                container.Add select_button.Value
                w <- HEIGHT
            for action in config.Actions do
                container.Add(ActionButton(item, action, main))
                w <- w + HEIGHT
            let c = config.Columns.Length
            let pos i = 
                let percent = float32 i / float32 c
                percent %- (w * percent)
            for i, column in List.indexed config.Columns do
                this.Add( Text(column.Text item, Align = Alignment.LEFT, Position = { Position.Default with Left = pos i; Right = pos (i + 1) }.Margin(Style.padding)) )

            this
            |+ Clickable(
                (fun () -> if select_button.IsSome then select_button.Value.Select()),
                OnHover = fun b -> if b && select_button.IsSome && not select_button.Value.Focused then select_button.Value.Focus())
            |* container

        override this.Draw() =
            if select_button.IsSome && select_button.Value.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
            base.Draw()

        member private this._container() = container

    type View<'T>(source: unit -> 'T seq, config: Config<'T>) as this =
        inherit StaticContainer(NodeType.Switch(fun _ -> this._container()))

        let fc = FlowContainer.Vertical<Widget>(HEIGHT)
        let container = ScrollContainer.Flow fc

        let mutable refresh_on_next_update = false

        do
            this |* container
            this.RefreshInternal()

        member private this.RefreshInternal() =
            fc.Clear()
            for item in source() do
                fc.Add( Card(item, config, this :> IGrid) )
            match config.New with
            | Some action -> fc.Add (CreateButton (action, this :> IGrid))
            | None -> ()

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if this.Focused && refresh_on_next_update then
                 refresh_on_next_update <- false
                 this.RefreshInternal()
                 this.Focus()

        member private this._container() = container
            
        interface IGrid with
            member this.Refresh() =
                refresh_on_next_update <- true

    let create (source: unit -> 'T seq) (config: Config<'T>) = View<'T>(source, config)

