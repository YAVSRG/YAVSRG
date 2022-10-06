namespace Interlude.Features.Import

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Charts.Sorting
open Interlude.UI
open Interlude.UI.Components

type private SearchContainerLoader(t) as this =
    inherit StaticWidget(NodeType.None)
    let t = t this
    let mutable task = None
    let timer = Animation.Counter(1000.0)

    let POP_AMOUNT = 10.0f

    // loader is only drawn if it is visible on screen
    override this.Draw() =
        let value = timer.Time / 500.0 * Math.PI

        let amt v = float32 (Math.Sin v) * POP_AMOUNT
        let bar = Rect.Box(this.Bounds.CenterX, this.Bounds.CenterY, 0.0f, 0.0f).Expand(10.0f, this.Bounds.Height * 0.5f - POP_AMOUNT)

        Draw.rect (bar.Translate(-30.0f, 0.0f).Expand(0.0f, amt value)) (!*Palette.LIGHT)
        Draw.rect (bar.Expand(0.0f, amt (value - Math.PI / 3.0))) (!*Palette.LIGHT)
        Draw.rect (bar.Translate(30.0f, 0.0f).Expand(0.0f, amt (value - Math.PI / 1.5))) (!*Palette.LIGHT)

        if task.IsNone then task <- Some <| BackgroundTask.Create TaskFlags.HIDDEN "Search container loading" (t |> BackgroundTask.Callback(fun _ -> sync(fun () -> (this.Parent :?> FlowContainer.Base<Widget>).Remove this)))

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        timer.Update elapsedTime

type private SearchContainer(populate, handleFilter, itemheight) as this =
    inherit StaticContainer(NodeType.None)

    let flow = FlowContainer.Vertical<Widget>(itemheight, Spacing = 15.0f)
    let scroll = ScrollContainer.Flow(flow, Margin = Style.padding, Position = Position.TrimTop 70.0f)
    let populate = populate flow
    let handleFilter = handleFilter flow

    do
        flow |* SearchContainerLoader populate
        this
        |+ (SearchBox(Setting.simple "", (fun (f: Filter) -> handleFilter f), Position = Position.SliceTop 60.0f ))
        |* scroll

    new(populate, handleFilter) = SearchContainer(populate, handleFilter, 80.0f)
