namespace Interlude.Features.Tables

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Backbeat
open Interlude.UI
open Interlude.UI.Menu

type private LevelButton(name, action) =
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
            K(sprintf "%s %s  >" Icons.FOLDER name),
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

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        if this.Focused then
            Draw.rect this.Bounds (!*Palette.HOVER)

        base.Draw()

type SelectTableLevelPage(table: Table, action: int -> unit) as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let refresh () =
        container.Clear()

        for level in table.Info.LevelDisplayNames.Keys do
            container |* LevelButton(table.Info.LevelName level, (fun () -> action level))

        if container.Focused then
            container.Focus false

    do
        refresh ()

        this.Content(ScrollContainer(container, Position = Position.Margin(100.0f, 200.0f)))

    override this.Title = %"table.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh ()
