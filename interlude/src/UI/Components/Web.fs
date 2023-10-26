namespace Interlude.UI.Components

open Percyqaz.Common
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.Utils

type private WebRequestState =
    | Offline = 0
    | Loading = 1
    | ServerError = 2
    | Loaded = 3

type WebRequestContainer<'T>(load: WebRequestContainer<'T> -> unit, render: 'T -> Widget) as this =
    inherit StaticContainer(NodeType.None)

    let mutable status = WebRequestState.Loading

    let mutable content : Widget = Dummy()

    let rerender(v) =
        content <- render v
        if this.Initialised && not content.Initialised then content.Init this

    let data = Setting.simple Unchecked.defaultof<'T> |> Setting.trigger rerender

    member this.Offline() =
        status <- WebRequestState.Offline

    member this.ServerError() =
        status <- WebRequestState.ServerError

    member this.SetData result =
        status <- WebRequestState.Loaded
        data.Value <- result

    member this.Reload() =
        status <- WebRequestState.Loading
        load this

    override this.Init(parent) =

        load this

        this
        |+ Conditional((fun () -> status = WebRequestState.Loading), LoadingState())
        |+ Conditional((fun () -> status = WebRequestState.Offline), EmptyState(Icons.connected, L"misc.offline"))
        |* Conditional((fun () -> status = WebRequestState.ServerError), EmptyState(Icons.connected, L"misc.server_error"))

        base.Init parent
        content.Init this

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        if status = WebRequestState.Loaded then content.Update(elapsedTime, moved)

    override this.Draw() =
        base.Draw()
        if status = WebRequestState.Loaded then content.Draw()