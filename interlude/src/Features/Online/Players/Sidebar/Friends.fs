namespace Interlude.Features.Online.Players

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
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
                        defer
                        <| fun () ->
                            match response with
                            | Some result -> this.SetData result
                            | None -> this.ServerError()
                    )
                else
                    this.Offline()
            , fun _ data ->
                let contents = FlowContainer.Vertical<Widget>(60.0f)

                for player in data.Friends do
                    contents.Add(
                        Container(NodeType.None)
                        |+ PlayerButton(player.Username, player.Color)
                        |+ Text(
                            (if player.Online then %"online.players.status.online" else %"online.players.status.offline"),
                            Color =
                                K(
                                    if player.Online then
                                        Colors.text_green_2
                                    else
                                        Colors.text_greyout
                                ),
                            Align = Alignment.RIGHT,
                            Position = Position.Shrink(20.0f, 15.0f)
                        )
                    )

                ScrollContainer(contents) :> Widget
        )

    override this.Init(parent) =
        base.Init parent
        Players.friends_changed <- this.Reload

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2
        base.Draw()