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

    // loader is only drawn if it is visible on screen
    override this.Draw() =
        // todo: improved loading indicator here
        Text.drawFill(Style.baseFont, "Loading...", this.Bounds, Color.White, 0.5f)
        if task.IsNone then task <- Some <| BackgroundTask.Create TaskFlags.HIDDEN "Search container loading" (t |> BackgroundTask.Callback(fun _ -> sync(fun () -> (this.Parent :?> FlowContainer.Base<Widget>).Remove this)))

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
