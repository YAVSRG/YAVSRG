namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Charts.Sorting
open Interlude.UI
open Interlude.UI.Components

type SearchContainerLoader(taskWithCallback: (unit -> unit) -> unit) =
    inherit StaticWidget(NodeType.None)
    let mutable loading = false

    // loader is only drawn if it is visible on screen
    override this.Draw() =
        if not loading then taskWithCallback(fun () -> sync(fun () -> (this.Parent :?> FlowContainer.Vertical<Widget>).Remove this)); loading <- true

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)

type PopulateFunc = SearchContainer -> (unit -> unit) -> unit
and FilterFunc = SearchContainer -> Filter -> unit
and SearchContainer(populate: PopulateFunc, handleFilter: FilterFunc, itemheight) as this =
    inherit StaticContainer(NodeType.Switch(fun _ -> this.Items))

    let flow = FlowContainer.Vertical<Widget>(itemheight, Spacing = 15.0f)
    let scroll = ScrollContainer.Flow(flow, Margin = Style.padding, Position = Position.TrimTop 70.0f)

    do
        flow |* SearchContainerLoader (populate this)
        this
        |+ (SearchBox(Setting.simple "", (fun (f: Filter) -> handleFilter this f), Position = Position.SliceTop 60.0f ))
        |* scroll

    new(populate, handleFilter) = SearchContainer(populate, handleFilter, 80.0f)

    member this.Items = flow

type private DLStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed