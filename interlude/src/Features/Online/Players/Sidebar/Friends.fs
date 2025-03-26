namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Web.Shared.Requests
open Interlude.Features.Online

type private FriendList() =
    inherit
        WebRequestContainer<Friends.List.Response>(
            fun this ->
                if Network.status = Network.Status.LoggedIn then
                    Friends.List.get (fun response ->
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

                for player in data.Friends do
                    contents.Add(
                        PlayerButton(player.Username, player.Color)
                        |+ Text(
                            if player.Online then %"online.players.status.online"
                            else %"online.players.status.offline"
                        )
                            .Color(
                                if player.Online then
                                    Colors.text_green_2
                                else
                                    Colors.text_greyout
                            )
                            .Align(Alignment.RIGHT)
                            .Position(Position.Shrink(20.0f, 15.0f))
                    )

                ScrollContainer(contents) :> Widget
        )

    override this.Init(parent) =
        base.Init parent
        Players.friends_changed <- this.Reload