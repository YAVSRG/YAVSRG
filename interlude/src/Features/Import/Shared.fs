namespace Interlude.Features.Import

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Charts
open Prelude.Data.Charts.Sorting
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components

[<AutoOpen>]
module Import =

    let charts_updated_ev = Event<unit>()
    let charts_updated = charts_updated_ev.Publish

    let dropFile(path: string) : bool =
        match Mounts.dropFunc with
        | Some f -> f path; true
        | None -> 
            Library.Imports.auto_convert.Request((path, false), 
                fun success -> 
                    if success then
                        Notifications.action_feedback(Icons.check, L"notification.import_success", "")
                        charts_updated_ev.Trigger()
                    else Notifications.error(L"notification.import_failure", "")
            )
            true

// todo: only etterna packs use this old one so just move it to there
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
    inherit StaticContainer(NodeType.Switch(fun _ -> if (this.Items: FlowContainer.Vertical<Widget>).Count > 0 then this.Items else this.SearchBox))

    let flow = FlowContainer.Vertical<Widget>(itemheight, Spacing = 15.0f)
    let scroll = ScrollContainer.Flow(flow, Margin = Style.PADDING, Position = Position.TrimTop 70.0f)
    let searchBox = SearchBox(Setting.simple "", (fun (f: Filter) -> handleFilter this f), Position = Position.SliceTop 60.0f )

    do
        flow |* SearchContainerLoader (populate this)
        this
        |+ searchBox
        |* scroll

    new(populate, handleFilter) = SearchContainer(populate, handleFilter, 80.0f)

    member this.Items = flow
    member private this.SearchBox = searchBox

type DownloadStatus =
    | NotDownloaded
    | Downloading
    | Installed
    | DownloadFailed