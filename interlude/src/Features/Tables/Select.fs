namespace Interlude.Features.Tables

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Backbeat
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Tables.Browser

type private TableButton(name: string, action: unit -> unit) =
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
                    if this.Focused then Colors.text_yellow_2
                    elif (match Content.Table with Some t -> t.Info.Name = name | _ -> false) then Colors.text_pink_2
                    else Colors.text
                ),
            Align = Alignment.LEFT,
            Position = Position.Shrink Style.PADDING
        )
        |* Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Render.rect this.Bounds Colors.yellow_accent.O1

        base.Draw()

type SelectTablePage(refresh_table_view: unit -> unit) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

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

                    if options.ChartGroupMode.Value = "level" && Screen.current_type = ScreenType.LevelSelect then
                        refresh_table_view()

                    GameThread.defer refresh
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
        ScrollContainer(container, Position = Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))

    override this.Title = sprintf "%s %s" Icons.SIDEBAR (%"table")
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh ()