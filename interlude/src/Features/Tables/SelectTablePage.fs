namespace Interlude.Features.Tables

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Prelude.Backbeat
open Interlude.Content
open Interlude.Options
open Interlude.UI
open Interlude.Features.Tables.Browser

type SelectTablePage(refresh_table_view: unit -> unit) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

    let rec refresh () =
        container.Clear()

        container
        |+ PageButton(
            %"tables.browser",
            (fun () -> TableBrowserPage().Show())
        )
            .Icon(Icons.DOWNLOAD)
        |* Dummy()

        for e in Tables.list () do
            container
            |* PageButton(
                e.Info.Name,
                fun () ->
                    options.Table.Set(Some e.Id)

                    if options.ChartGroupMode.Value = "level" && Screen.current_type = ScreenType.LevelSelect then
                        refresh_table_view()

                    GameThread.defer refresh
            )
                .TextColor(fun () ->
                    match Content.Table with
                    | Some t when t.Info.Name = e.Info.Name -> Colors.text_pink_2
                    | _ -> Colors.text
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
        ScrollContainer(container)
            .Position(Position.Shrink(PAGE_MARGIN_X, PAGE_MARGIN_Y))

    override this.Title = sprintf "%s %s" Icons.SIDEBAR (%"table")
        override this.OnReturnFromNestedPage() = refresh ()