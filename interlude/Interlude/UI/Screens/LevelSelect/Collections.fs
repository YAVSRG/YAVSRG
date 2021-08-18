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

        type Card<'T>(item: 'T, parent: CardSelector<'T>) as this =
            inherit NavigateSelectable()
            let buttons = ResizeArray<Selectable>()
            let mutable index = -1
            do
                let h = 100.0f

                let addButton (b: Widget) = let b = (b :?> Selectable) in buttons.Add b; this.Add b

                new TextBox((fun () -> parent.NameFunc this), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add

                let mutable x = -h

                new TextBox((fun () -> if parent.IsMarked this then markedIcon else unmarkedIcon), K (Color.White, Color.Black), 0.0f)
                |> positionWidget(x, 1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 1.0f)
                |> this.Add

                if true then
                    x <- x - h
                    new LittleButton(K deleteIcon, fun () -> parent.Delete this)
                    |> positionWidget(x, 1.0f, 0.0f, 1.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton

                if Option.isSome parent.EditHandler then
                    x <- x - h
                    new LittleButton(K editIcon, fun () -> parent.Edit this)
                    |> positionWidget(x, 1.0f, 0.0f, 1.0f, x + h, 1.0f, 0.0f, 1.0f)
                    |> addButton

                if parent.CanDuplicate then
                    x <- x - h
                    new LittleButton(K addIcon, fun () -> parent.Duplicate this)
                    |> positionWidget(x, 1.0f, 0.0f, 1.0f, x + h, 1.0f, 0.0f, 1.0f)
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

            interface System.IComparable with
                 member this.CompareTo(obj: obj) = 
                     match obj with
                     | :? Card<'T> as x -> compare this x
                     | _ -> failwith ""

        and CardSelector<'T>(source: ISettable<'T list>, markHandler: 'T * bool -> unit, add: string * Selectable -> unit) as this =
            inherit NavigateSelectable()

            let mutable marked = Set.empty
            let mutable multiSelect = false
            let fc = FlowContainer()
            let items = source.Value |> List.map (fun x -> Card<'T>(x, this))
            do
                this.Add fc
                items |> List.iter fc.Add

            member val NameFunc = fun (c: Card<'T>) -> c.Setting.Value.ToString() with get, set

            member val EditHandler = None with get, set
            member val CreateHandler: 'T option = None with get, set
            member val RefreshHandler = None with get, set //nyi

            member val CanReorder = false with get, set
            member val CanDuplicate = false with get, set
            member val AllowDeleteMarked = false with get, set
            member this.MultiSelect with get() = multiSelect and set(v) = multiSelect <- v; if not v && Set.isEmpty marked then marked <- Set.singleton items.Head

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

            member this.Edit(item: Card<'T>) = add("EditItem", this.EditHandler.Value item.Setting)

            member this.Create() =
                this.Synchronized(fun () -> this.Add(Card(this.CreateHandler.Value, this)))

            member this.Duplicate(item: Card<'T>) =
                this.Synchronized(fun () -> this.Add(Card(item.Setting.Value, this)))

            member this.Delete(item: Card<'T>) =
                if not (this.IsMarked item) || this.AllowDeleteMarked then
                    this.Unmark item
                    item.Destroy()

            member this.Mark(item: Card<'T>) =
                if multiSelect then
                    marked <- Set.add item marked
                else marked <- Set.singleton item
                markHandler(item.Setting.Value, true)
            member this.Unmark(item: Card<'T>) =
                if multiSelect then marked <- Set.remove item marked
                markHandler(item.Setting.Value, false)
            member this.IsMarked(item: Card<'T>) = Set.contains item marked

            override this.OnSelect() =
                this.HoverChild <- Some (fc.Children.[0] :?> Selectable)

            override this.OnDeselect() =
                base.OnDeselect()
                let xs = fc.Children |> Seq.map (fun w -> (w :?> Card<'T>).Setting.Value) |> List.ofSeq
                source.Value <- xs

    let gui(add)=
        let setting =
            { new ISettable<(string * Collection) list>() with
                override this.Value
                    with get() = cache.GetCollections() |> Seq.map (fun n -> (n, cache.GetCollection(n).Value)) |> List.ofSeq
                    and set v = ()
            }
        column
            [
                PrettySetting("Collections",
                    CardSelect.CardSelector(setting, (fun (x, _) -> selected <- x), add))
            ]
    
type CollectionManager() as this =
    inherit FlowSelectable(60.0f, 5.0f,
        fun () ->
            let (left, _, right, _) = this.Anchors
            left.Target <- -800.0f
            right.Target <- -800.0f)
    
    let collectionCard name =
        { new CardButton(name, "",
            (fun () -> fst Collections.selected = name),
            fun () -> colorVersionGlobal <- colorVersionGlobal + 1; Collections.selected <- (name, (cache.GetCollection name).Value)) with
            override self.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                if Mouse.Hover self.Bounds && options.Hotkeys.Delete.Value.Tapped() then
                    ScreenGlobals.addTooltip(options.Hotkeys.Delete.Value, Localisation.localiseWith [name] "misc.Delete", 2000.0,
                        fun () -> this.Synchronized(fun () -> this.Remove self; self.Dispose()); cache.DeleteCollection name; ScreenGlobals.addNotification(Localisation.localiseWith [name] "notification.Deleted", NotificationType.Info))}

    do
        CardButton(Localisation.localise "collections.Create", "", K false,
            (fun () ->
                TextInputDialog(this.Bounds, "Name collection",
                    fun s ->
                        if s <> "" && (cache.GetCollection s).IsNone then
                            cache.UpdateCollection(s, Collection.Blank)
                            collectionCard s |> this.Add)
                |> ScreenGlobals.addDialog
            ), K Color.Silver) |> this.Add
        for name in cache.GetCollections() do collectionCard name |> this.Add
        //todo: save last selected collection in options
        if fst Collections.selected = "" then
            let favourites = Localisation.localise "collections.Favourites"
            let c = 
                match cache.GetCollection favourites with
                | Some c -> c
                | None ->
                    let n = Collection.Blank
                    cache.UpdateCollection (favourites, n)
                    n
            Collections.selected <- (favourites, c)
                        
    override this.OnSelect() =
        base.OnSelect()
        let (left, _, right, _) = this.Anchors
        left.Target <- 0.0f
        right.Target <- 0.0f
    
    override this.Draw() =
        base.Draw()
        let struct (left, top, right, bottom) = this.Bounds
        let m = (right + left) * 0.5f
        Text.drawJustB(Themes.font(), Localisation.localiseWith [options.Hotkeys.AddToCollection.Value.ToString()] "collections.AddHint", 25.0f, m, bottom - 60.0f, (Color.White, Color.Black), 0.5f)
        Text.drawJustB(Themes.font(), Localisation.localiseWith [options.Hotkeys.RemoveFromCollection.Value.ToString()] "collections.RemoveHint", 25.0f, m, bottom - 30.0f, (Color.White, Color.Black), 0.5f)
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
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