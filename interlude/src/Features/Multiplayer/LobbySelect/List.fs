namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI
open Interlude.Web.Shared
open Interlude.Features.Online

type LobbyInfoCard(info: LobbyInfo) =
    inherit FrameContainer(NodeType.None)

    override this.Init(parent) =
        this
            .Add(
                Text(info.Name)
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceT(50.0f).Shrink(5.0f)),

                Text(
                    match info.CurrentlyPlaying with
                    | None -> %"lobby.no_song_selected"
                    | Some s -> s
                )
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Position(Position.SliceB(40.0f).Shrink(5.0f)),

                Text(info.Players.ToString() + " " + Icons.USERS)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.RIGHT)
                    .Position(Position.SliceT(50.0f).Shrink(5.0f)),

                MouseListener()
                    .OnLeftClick(fun () -> Network.join_lobby info.Id)
            )

        base.Init parent

    member this.Name = info.Name

type LobbyList() =
    inherit Container(NodeType.None)

    let lobby_list_container =
        FlowContainer.Vertical<LobbyInfoCard>(80.0f)
            .Spacing(Style.PADDING * 3.0f)

    let mutable no_lobbies = false
    // todo: loading state

    let refresh_list () = Network.client.Send(Upstream.GET_LOBBIES)
    let create_lobby () = CreateLobbyPage().Show()

    member this.UpdateList (lobbies: LobbyInfo array) =
        no_lobbies <- lobbies.Length = 0
        lobby_list_container.Clear()

        for lobby in lobbies do
            lobby_list_container.Add(LobbyInfoCard lobby)

    override this.Init(parent: Widget) =
        this
            .Add(
                ScrollContainer(lobby_list_container)
                    .Margin(Style.PADDING)
                    .Position(Position.Shrink(0.0f, 80.0f)),

                EmptyState(Icons.USERS, %"lobby_list.none", Subtitle = %"lobby_list.none.subtitle")
                    .Conditional(fun () -> no_lobbies),

                Button(Icons.PLUS_CIRCLE + "  " + %"lobby_list.create", create_lobby)
                    .Position(Position.SliceB(60.0f).ShrinkR(250.0f)),
                Button(Icons.REFRESH_CCW + "  " + %"lobby_list.refresh", refresh_list)
                    .Position(Position.SliceB(60.0f).SliceR(250.0f)),

                SearchBox(fun query ->
                    lobby_list_container
                        .Filter(fun l -> l.Name.Contains(query, System.StringComparison.InvariantCultureIgnoreCase))
                )
                    .Position(Position.SliceT(SearchBox.HEIGHT))
        )

        base.Init parent

        refresh_list()