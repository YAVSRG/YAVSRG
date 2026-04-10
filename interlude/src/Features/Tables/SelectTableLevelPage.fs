namespace Interlude.Features.Tables

open Percyqaz.Flux.UI
open Prelude
open Prelude.Backbeat
open Interlude.UI

type SelectTableLevelPage(table: Table, action: int -> unit) =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PAGE_ITEM_HEIGHT)

    let refresh () =
        container.Clear()

        for level in table.Info.LevelDisplayNames.Keys do
            container
                .Add(
                    PageButton(table.Info.LevelName level, fun () -> action level)
                        .Icon(Icons.FOLDER)
                )

        if container.Focused then
            container.Focus false

    override this.Content() =
        refresh ()
        ScrollContainer(container)
            .Position(Position.Shrink(100.0f, 200.0f))

    override this.Title = %"table"
    override this.OnReturnFromNestedPage() = refresh ()