namespace Interlude.Features.Online.Players

open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type private OnlineList() =
    inherit
        WebRequestContainer<Players.Online.Response>(
            fun this ->
                if Network.status = Network.Status.LoggedIn then
                    Players.Online.get (fun response ->
                        GameThread.defer
                        <| fun () ->
                            match response with
                            | Some result -> this.SetData result
                            | None -> this.ServerError()
                    )
                else
                    this.Offline()
            , fun _ data ->
                let contents = FlowContainer.Vertical<Widget>(60.0f, Spacing = Style.PADDING)

                for player in data.Players do
                    contents.Add(PlayerButton(player.Username, player.Color))

                ScrollContainer(contents) :> Widget
        )