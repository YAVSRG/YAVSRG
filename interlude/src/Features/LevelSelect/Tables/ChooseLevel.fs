namespace Interlude.Features.LevelSelect.Tables

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data.Charts.Tables
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

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

type SelectTableLevelPage(action: Level -> unit) as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)

    let refresh () =
        container.Clear()

        match Table.current () with
        | Some t ->
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
