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
open Interlude.Features.Tables.Browser

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

type SelectTablePage(refresh_table_view) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let rec refresh () =
        container.Clear()

        container
        |+ PageButton(
            %"tables.browser",
            (fun () -> TableBrowserPage().Show()),
            Icon = Icons.DOWNLOAD
        )
        |* Dummy()

        for e in Tables.list () do
            container
            |* TableButton(
                e.Info.Name,
                fun () ->
                    options.Table.Set(Some e.Id)

                    if options.LibraryMode.Value = Data.Library.Sorting.LibraryMode.Table && Screen.current_type = Screen.Type.LevelSelect then
                        refresh_table_view()

                    defer refresh
            )

        match Content.Table with
        | Some table ->
            container |+ Dummy()
            |* PageButton(%"table.suggestions", (fun () -> SuggestionsPage(table).Show()))
        | None -> ()

        if container.Focused then
            container.Focus false

    override this.Content() =
        refresh ()
        ScrollContainer(container, Position = Position.Margin(100.0f, 200.0f))

    override this.Title = sprintf "%s %s" Icons.SIDEBAR (%"table")
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh ()
