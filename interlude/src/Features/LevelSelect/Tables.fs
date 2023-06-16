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

// todo: remove or repurpose this
type private EditLevelPage(level: Level) as this =
    inherit Page()

    let new_name = Setting.simple level.Name |> Setting.alphaNum

    let level_progress =
        let mutable total = 0.0
        for c in level.Charts do
            match Prelude.Data.Scores.Scores.getData c.Hash with
            | Some d -> 
                let ruleset_id = Table.current().Value.RulesetId
                if d.Bests.ContainsKey(ruleset_id) then
                    let (accuracy, rate) = d.Bests.[ruleset_id].Accuracy.Best
                    if System.MathF.Round(rate, 2) > 0.99f then total <- total + accuracy
            | None -> ()
        total / float level.Charts.Count

    do
        let content =
            column()
            |+ PageSetting("table.level_name", TextEntry(new_name, "none"))
                .Pos(200.0f)
            |+ Text(sprintf "Progress: %.2f%%" (level_progress * 100.0), Align = Alignment.LEFT, Position = Position.Box(0.0f, 0.0f, 100.0f, 300.0f, PRETTYWIDTH, PRETTYHEIGHT))

        this.Content content

    override this.Title = level.Name
    override this.OnClose() = ()

type private LevelButton(name, action) =
    inherit StaticContainer(NodeType.Button (fun _ -> action()))
    
    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (sprintf "%s %s  >" Icons.folder name),
            Color = ( 
                fun () -> ( 
                    (if this.Focused then Style.color(255, 1.0f, 0.5f) else Color.White), Color.Black
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
        |+ PageButton("tables.install", ignore, Icon = Icons.download)
        |* Dummy()

        for name in Table.list() do
            container |* TableButton(name, fun () -> 
                options.Table.Set (Some name)
                Table.load name
                if options.LibraryMode.Value = LibraryMode.Table then LevelSelect.refresh_all() else LevelSelect.refresh_details()
                sync refresh)

        if container.Focused then container.Focus()

    do
        refresh()

        this.Content( ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f)) )
        this |* WIP()

    override this.Title = L"table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh()

type SelectTableLevelPage(action: Level -> unit) as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
    let refresh() =
        container.Clear()

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

    override this.Title = L"table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh()