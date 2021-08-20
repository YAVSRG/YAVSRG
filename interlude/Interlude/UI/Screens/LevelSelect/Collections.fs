namespace Interlude.UI.Screens.LevelSelect

open System.Drawing
open Prelude.Common
open Prelude.Data.ChartManager
open Interlude
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Selection
open Interlude.Gameplay
open Interlude.Options
open Interlude.Input
open Interlude.Graphics
open Globals

module Collections =

    let mutable selected =
        //todo: load from settings
        let favourites = Localisation.localise "collections.Favourites"
        let c = 
            match cache.GetCollection favourites with
            | Some c -> c
            | None ->
                let n = Collection.Blank
                cache.UpdateCollection (favourites, n)
                n
        favourites, c

    module CardSelect =

        let markedIcon = "⬤"
        let unmarkedIcon = "◯"

        let addIcon = "➕"
        let editIcon = "✎"
        let deleteIcon = "✕"

        type Config<'T> =
            {
                NameFunc: 'T -> string
                EditFunc: (Setting<'T> -> SelectionPage) option
                CreateFunc: (unit -> 'T) option
                MarkFunc: 'T * bool -> unit

                CanReorder: bool
                CanDuplicate: bool
                CanDelete: bool
                CanMultiSelect: bool
            }

        type Card<'T>(item: 'T, config: Config<'T>, parent: Selector<'T>) as this =
            inherit NavigateSelectable()
            let buttons = ResizeArray<Selectable>()
            let mutable index = -1
            do
                let h = 90.0f

                let addButton (b: Widget) =
                    let b = (b :?> Selectable)
                    buttons.Add b
                    this.Add b

                new TextBox((fun () -> config.NameFunc (this.Setting: Setting<'T>).Value), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add

                let mutable x = -h

                new TextBox((fun () -> if parent.IsMarked this then markedIcon else unmarkedIcon), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(x, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add

                if config.CanDelete then
                    x <- x - h
                    new LittleButton(K deleteIcon, fun () -> parent.Delete this)
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton

                if Option.isSome config.EditFunc then
                    x <- x - h
                    new LittleButton(K editIcon, fun () -> parent.Edit this)
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton

                if config.CanDuplicate then
                    x <- x - h
                    new LittleButton(K addIcon, fun () -> parent.Duplicate this)
                    |> positionWidget(x, 1.0f, 0.0f, 0.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton

                this |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 80.0f, 0.0f) |> ignore
            member val Setting = new Setting<'T>(item)
            override this.SParent = Some (parent :> Selectable)

            override this.Up() = parent.Up()
            override this.Down() = parent.Down()
            override this.Left() =
                if index < 0 then index <- buttons.Count - 1
                else index <- index - 1
                if index < 0 then this.HoverChild <- None else this.HoverChild <- Some (buttons.[index])
            override this.Right() =
                if index = buttons.Count - 1 then index <- -1
                else index <- index + 1
                if index < 0 then this.HoverChild <- None else this.HoverChild <- Some (buttons.[index])

            override this.Draw() =
                if parent.IsMarked this then Draw.rect this.Bounds (ScreenGlobals.accentShade(80, 1.0f, 0.0f)) Sprite.Default
                if this.Selected then Draw.rect this.Bounds (Color.FromArgb(120, 255, 255, 255)) Sprite.Default
                elif this.Hover then Draw.rect this.Bounds (Color.FromArgb(80, 255, 255, 255)) Sprite.Default
                base.Draw()

        and Selector<'T>(source: ISettable<'T list>, config: Config<'T>, add: string * Selectable -> unit) as this =
            inherit NavigateSelectable()

            let fc = FlowContainer()
            let items = source.Value |> List.map (fun x -> Card<'T>(x, config, this))
            let mutable marked = if config.CanMultiSelect then List.empty else List.singleton items.Head
            do
                this.Add fc
                items |> List.iter fc.Add

            member this.Edit(item: Card<'T>) = add("EditItem", config.EditFunc.Value item.Setting add)

            member this.Create() =
                this.Synchronized(fun () -> this.Add(Card(config.CreateFunc.Value (), config, this)))

            member this.Duplicate(item: Card<'T>) =
                this.Synchronized(fun () -> this.Add(Card(item.Setting.Value, config, this)))

            member this.Delete(item: Card<'T>) =
                if not (this.IsMarked item) then
                    if item.Selected then this.Down()
                    item.Destroy()

            member this.Mark(item: Card<'T>) =
                if config.CanMultiSelect then
                    marked <- if List.contains item marked then marked else item :: marked
                else marked <- List.singleton item
                config.MarkFunc (item.Setting.Value, true)
            member this.Unmark(item: Card<'T>) =
                if config.CanMultiSelect then marked <- List.except [item] marked
                config.MarkFunc (item.Setting.Value, false)
            member this.IsMarked(item: Card<'T>) = List.contains item marked

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

            override this.OnDeselect() =
                base.OnDeselect()
                let xs = fc.Children |> Seq.map (fun w -> (w :?> Card<'T>).Setting.Value) |> List.ofSeq
                source.Value <- xs

    let editor setting add =
        column [
            PrettySetting("Coming soon", new Selectable()).Position(200.0f)
        ] :> Selectable

    let page add =
        let setting =
            { new ISettable<(string * Collection) list>() with
                override this.Value
                    with get() = cache.GetCollections() |> Seq.map (fun n -> (n, cache.GetCollection(n).Value)) |> List.ofSeq
                    and set v = ()
            }
        column
            [
                PrettySetting("Collections",
                    CardSelect.Selector(
                        setting,
                        { 
                            NameFunc = (fun (x, _) -> x)
                            MarkFunc = (fun (x, _) -> colorVersionGlobal <- colorVersionGlobal + 1; selected <- x)
                            EditFunc = Some editor
                            CreateFunc = None

                            CanReorder = false
                            CanDuplicate = false
                            CanDelete = true
                            CanMultiSelect = false
                        }, add)).Position(200.0f, 1200.0f, 600.0f)
            ] :> Selectable
    
type CollectionManager() =
    inherit Widget()
    
    override this.Draw() =
        base.Draw()
        //Text.drawJustB(Themes.font(), Localisation.localiseWith [options.Hotkeys.AddToCollection.Value.ToString()] "collections.AddHint", 25.0f, m, bottom - 60.0f, (Color.White, Color.Black), 0.5f)
        //Text.drawJustB(Themes.font(), Localisation.localiseWith [options.Hotkeys.RemoveFromCollection.Value.ToString()] "collections.RemoveHint", 25.0f, m, bottom - 30.0f, (Color.White, Color.Black), 0.5f)
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        if options.Hotkeys.Collections.Value.Tapped() then
            ScreenGlobals.addDialog <| SelectionMenu Collections.page
        if currentCachedChart.IsSome then
            if options.Hotkeys.AddToCollection.Value.Tapped() then
                if
                    match snd Collections.selected with
                    | Collection ccs -> if ccs.Contains selectedChart then false else ccs.Add selectedChart; true
                    | Playlist ps -> ps.Add (selectedChart, selectedMods, rate); true
                    | Goals gs -> false //not yet implemented
                then
                    colorVersionGlobal <- colorVersionGlobal + 1
                    ScreenGlobals.addNotification(Localisation.localiseWith [currentCachedChart.Value.Title; fst Collections.selected] "collections.Added", NotificationType.Info)
            elif options.Hotkeys.RemoveFromCollection.Value.Tapped() then
                if
                    match snd Collections.selected with
                    | Collection ccs -> ccs.Remove selectedChart
                    | Playlist ps -> ps.RemoveAll(fun (id, _, _) -> id = selectedChart) > 0
                    | Goals gs -> gs.RemoveAll(fun ((id, _, _), _) -> id = selectedChart) > 0
                then
                    colorVersionGlobal <- colorVersionGlobal + 1
                    ScreenGlobals.addNotification(Localisation.localiseWith [currentCachedChart.Value.Title; fst Collections.selected] "collections.Removed", NotificationType.Info)