namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Gameplay
open Prelude.Data.Charts.Tables
open Prelude.Data.Charts.Sorting
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu

// todo: remove or repurpose this
type private EditLevelPage(level: Level) as this =
    inherit Page()

    let new_name = Setting.simple level.Name |> Setting.alphanumeric

    let level_progress =
        let mutable total = 0.0

        for c in level.Charts do
            match Prelude.Data.Scores.Scores.get c.Hash with
            | Some d ->
                let ruleset_id = Table.current().Value.RulesetId

                if d.PersonalBests.ContainsKey(ruleset_id) then
                    match PersonalBests.get_best_above 1.0f d.PersonalBests.[ruleset_id].Accuracy with
                    | Some accuracy -> total <- total + accuracy
                    | None -> ()
            | None -> ()

        total / float level.Charts.Count

    do
        let content =
            column ()
            |+ PageTextEntry("table.level_name", new_name).Pos(200.0f)
            |+ Text(
                sprintf "Progress: %.2f%%" (level_progress * 100.0),
                Align = Alignment.LEFT,
                Position = Position.Box(0.0f, 0.0f, 100.0f, 300.0f, PRETTYWIDTH, PRETTYHEIGHT)
            )

        this.Content content

    override this.Title = level.Name
    override this.OnClose() = ()

type private LevelButton(name, action) =
    inherit
        StaticContainer(
            NodeType.Button(fun _ ->
                Style.click.Play()
                action ()
            )
        )

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(sprintf "%s %s  >" Icons.folder name),
            Color =
                (fun () ->
                    ((if this.Focused then
                          Palette.color (255, 1.0f, 0.5f)
                      else
                          Colors.white),
                     Colors.black)
                ),
            Align = Alignment.LEFT,
            Position = Position.Margin Style.PADDING
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds (!*Palette.HOVER)

        base.Draw()

type private TableButton(name, action) =
    inherit
        StaticContainer(
            NodeType.Button(fun _ ->
                Style.click.Play()
                action ()
            )
        )

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(sprintf "%s  >" name),
            Color =
                (fun () ->
                    ((if this.Focused then
                          Palette.color (255, 1.0f, 0.5f)
                      else
                          Colors.white),
                     (if Some name = options.Table.Value then
                          Palette.color (255, 0.5f, 0.0f)
                      else
                          Colors.black))
                ),
            Align = Alignment.LEFT,
            Position = Position.Margin Style.PADDING
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds (!*Palette.HOVER)

        base.Draw()

type ManageTablesPage() as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let rec refresh () =
        container.Clear()

        container
        |+ PageButton(
            "tables.install",
            (fun () ->
                Menu.Exit()
                Interlude.Features.Import.ImportScreen.switch_to_tables ()
                Screen.change Screen.Type.Import Transitions.Flags.Default |> ignore
            ),
            Icon = Icons.download
        )
        |* Dummy()

        for e in Table.list () do
            container
            |* TableButton(
                e.Table.Name,
                fun () ->
                    options.Table.Set(Some e.Table.Name)
                    Table.load e.Table.Name

                    if options.LibraryMode.Value = LibraryMode.Table then
                        LevelSelect.refresh_all ()
                    else
                        LevelSelect.refresh_details ()

                    sync refresh
            )

        if container.Focused then
            container.Focus()

    do
        refresh ()

        this.Content(ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f)))

    override this.Title = %"table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh ()

type SelectTableLevelPage(action: Level -> unit) as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let refresh () =
        container.Clear()

        match Table.current () with
        | Some t ->
            container |* Dummy()

            for level in t.Levels do
                container |* LevelButton(level.Name, (fun () -> action level))
        | None -> ()

        if container.Focused then
            container.Focus()

    do
        refresh ()

        this.Content(ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f)))

    override this.Title = %"table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh ()
