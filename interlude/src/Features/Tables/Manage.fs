namespace Interlude.Features.Tables

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Backbeat
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.UI.Menu

type private TableButton(name, action) =
    inherit
        Container(
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
                    ((if this.Focused then Colors.yellow_accent else Colors.white),
                     (if
                          (match Content.Table with
                           | Some t -> t.Info.Name = name
                           | None -> false)
                      then
                          Palette.color (255, 0.5f, 0.0f)
                      else
                          Colors.shadow_2))
                ),
            Align = Alignment.LEFT,
            Position = Position.Margin Style.PADDING
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type ManageTablesPage(table_changed) =
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
            Icon = Icons.DOWNLOAD
        )
        |* Dummy()

        for e in Tables.list () do
            container
            |* TableButton(
                e.Info.Name,
                fun () ->
                    options.Table.Set(Some e.Id)

                    table_changed ()

                    defer refresh
            )

        match Content.Table with
        | Some table ->
            container |+ Dummy()
            |* PageButton("table.suggestions", (fun () -> SuggestionsPage(table).Show()))
        | None -> ()

        if container.Focused then
            container.Focus false

    override this.Content() =
        refresh ()
        ScrollContainer(container, Position = Position.Margin(100.0f, 200.0f))

    override this.Title = %"table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh ()
