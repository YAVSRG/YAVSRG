namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Tables
open Interlude.Options
open Interlude.Utils
open Interlude.Content
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Prelude.Data.Charts.Library

type private CreateTablePage() as this =
    inherit Page()

    let new_name = Setting.simple "Table" |> Setting.alphaNum
    let keymode = Setting.simple (match Chart.cacheInfo with Some c -> enum c.Keys | None -> Keymode.``4K``)

    do
        this.Content(
            column()
            |+ PrettySetting("table.name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("generic.keymode", Selector<_>.FromEnum keymode).Pos(300.0f)
            |+ PrettyButton("confirm.yes", 
                (fun () -> if Table.create(new_name.Value, int keymode.Value, Rulesets.current) then options.Table.Set (Some new_name.Value); Menu.Back() )).Pos(400.0f)
        )

    override this.Title = L"options.tables.create.name"
    override this.OnClose() = ()
    
type private CreateLevelPage() as this =
    inherit Page()
    
    let new_name = Setting.simple "" |> Setting.alphaNum
    
    do
        this.Content(
            column()
            |+ PrettySetting("table.level_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettyButton("confirm.yes", 
                (fun () -> if Table.current().Value.AddLevel new_name.Value then Menu.Back() )).Pos(300.0f)
        )
    
    override this.Title = L"options.tables.create.name"
    override this.OnClose() = ()

type private EditLevelPage(level: Level) as this =
    inherit Page()

    let new_name = Setting.simple level.Name |> Setting.alphaNum

    do
        let content =
            column()
            |+ PrettySetting("table.level_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettyButton("collections.edit.delete", 
                (fun () -> 
                    ConfirmPage(Localisation.localiseWith [level.Name] "misc.confirmdelete", 
                        fun () -> 
                            if Table.current().Value.RemoveLevel level.Name then 
                                if options.LibraryMode.Value = LibraryMode.Table then LevelSelect.refresh <- true
                                if ActiveCollection.Level level.Name = options.Collection.Value then Collections.unselect()
                                Menu.Back()
                    ).Show()
                ),
                Icon = Icons.delete).Pos(400.0f)
            |+ PrettyButton("collections.edit.select", 
                (fun () -> Collections.select_level level.Name; Menu.Back()) ).Pos(500.0f)

        this.Content content

    override this.Title = level.Name
    override this.OnClose() =
        if new_name.Value <> level.Name then
            if Table.current().Value.RenameLevel(level.Name, new_name.Value) then
                if options.Collection.Value = ActiveCollection.Level level.Name then Collections.select_level new_name.Value
                Logging.Debug (sprintf "Renamed level '%s' to '%s'" level.Name new_name.Value)
            else Logging.Debug "Rename failed, maybe that level already exists?"

type private LevelButton(name, action) =
    inherit StaticContainer(NodeType.Button (fun _ -> action()))
    
    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (sprintf "%s %s  >" Icons.folder name),
            Color = ( 
                fun () -> ( 
                    (if this.Focused then Style.color(255, 1.0f, 0.5f) else Color.White),
                    (if options.Collection.Value = ActiveCollection.Level name then Style.color(255, 0.5f, 0.0f) else Color.Black)
                )
            ),
            Align = Alignment.LEFT,
            Position = Position.Margin Style.padding)
        |* Clickable.Focus this
        base.Init parent
    
    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        base.Draw()

type private TableButton(name, action) =
    inherit StaticContainer(NodeType.Button (fun _ -> action()))
            
    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (sprintf "%s  >" name),
            Color = ( 
                fun () -> ( 
                    (if this.Focused then Style.color(255, 1.0f, 0.5f) else Color.White),
                    (if Some name = options.Table.Value then Style.color(255, 0.5f, 0.0f) else Color.Black)
                )
            ),
            Align = Alignment.LEFT,
            Position = Position.Margin Style.padding)
        |* Clickable.Focus this
        base.Init parent
            
    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        base.Draw()

type ManageTablesPage() as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
    let rec refresh() =
        container.Clear()

        container
        |+ PrettyButton("tables.install", ignore, Icon = Icons.download)
        |* Dummy()

        for name in Table.list() do
            container |* TableButton(name, fun () -> 
                options.Table.Set (Some name)
                Table.load name
                match options.Collection.Value with ActiveCollection.Level _ -> Collections.unselect() | _ -> ()
                if options.LibraryMode.Value = LibraryMode.Table then LevelSelect.refresh <- true else LevelSelect.minorRefresh <- true
                sync refresh)

        if options.EnableTableEdit.Value then
            
            container
            |+ Dummy()
            |+ PrettyButton("tables.create", (fun () -> Menu.ShowPage CreateTablePage), Icon = Icons.add)
            |* PrettyButton("tables.create_level", (fun () -> Menu.ShowPage CreateLevelPage), Icon = Icons.add_to_collection)

            match Table.current() with
            | Some t ->
                container |* Dummy()
                for level in t.Levels do
                    container |* LevelButton(level.Name, (fun () -> EditLevelPage(level).Show()) )
            | None -> ()


        if container.Focused then container.Focus()

    do
        refresh()

        this.Content( ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f)) )
        this |* WIP()

    override this.Title = L"options.table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh()

type SelectTableLevelPage(action: Level -> unit) as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
    let refresh() =
        container.Clear()
        container
        |* PrettyButton("tables.create_level", (fun () -> Menu.ShowPage CreateLevelPage), Icon = Icons.add_to_collection)

        match Table.current() with
        | Some t ->
            container |* Dummy()
            for level in t.Levels do
                container |* LevelButton(level.Name, (fun () -> action level) )
        | None -> ()

        if container.Focused then container.Focus()

    do
        refresh()

        this.Content( ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f)) )

    override this.Title = L"options.table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh()