namespace Interlude.UI

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Utils
open Prelude
open Interlude.UI

type private WebRequestState =
    | Offline = 0
    | Loading = 1
    | ServerError = 2
    | Loaded = 3

type WebRequestContainer<'T>(load: WebRequestContainer<'T> -> unit, render_ui: WebRequestContainer<'T> -> 'T -> Widget) as this =
    inherit Container(NodeType.Container (fun () -> Some this.Content))

    let mutable status = WebRequestState.Loading

    let mutable content: Widget = Dummy()

    let rerender (v) =
        content <- render_ui this v

        if this.Initialised && not content.Initialised then
            content.Init this

    let data = Setting.simple Unchecked.defaultof<'T> |> Setting.trigger rerender

    member private this.Content = content

    member this.Offline() =
        require_ui_thread ()
        status <- WebRequestState.Offline

    member this.ServerError() =
        require_ui_thread ()
        status <- WebRequestState.ServerError

    member this.SetData result =
        require_ui_thread ()
        status <- WebRequestState.Loaded
        data.Value <- result

    member this.Reload() =
        require_ui_thread ()
        status <- WebRequestState.Loading
        load this

    override this.Init(parent) =

        load this

        this
        |+ LoadingIndicator.Strip(
            (fun () -> status = WebRequestState.Loading),
            Position = Position.SliceTop(Style.PADDING)
        )
        |+ Conditional((fun () -> status = WebRequestState.Offline), EmptyState(Icons.GLOBE, %"misc.offline"))
        |* Conditional((fun () -> status = WebRequestState.ServerError), EmptyState(Icons.GLOBE, %"misc.server_error"))

        base.Init parent
        content.Init this

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if status = WebRequestState.Loaded || status = WebRequestState.Loading then
            content.Update(elapsed_ms, moved)

    override this.Draw() =
        base.Draw()

        if status = WebRequestState.Loaded || status = WebRequestState.Loading then
            content.Draw()
