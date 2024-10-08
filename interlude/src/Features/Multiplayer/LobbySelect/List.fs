﻿namespace Interlude.Features.Multiplayer

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
        |+ Text(info.Name, Position = Position.SliceT(50.0f).Shrink(5.0f), Align = Alignment.LEFT)
        |+ Text(
            (match info.CurrentlyPlaying with
             | None -> %"lobby.no_song_selected"
             | Some s -> s),
            Color = K Colors.text_subheading,
            Position = Position.SliceB(40.0f).Shrink(5.0f),
            Align = Alignment.LEFT
        )
        |+ Clickable(fun () -> Network.join_lobby info.Id)
        |* Text(
            info.Players.ToString() + " " + Icons.USERS,
            Color = K Colors.text_subheading,
            Position = Position.SliceT(50.0f).Shrink(5.0f),
            Align = Alignment.RIGHT
        )

        base.Init parent

    member this.Name = info.Name

type LobbyList() =
    inherit Container(NodeType.None)

    let search_text = Setting.simple ""

    let list_container = FlowContainer.Vertical<LobbyInfoCard>(80.0f, Spacing = 10.0f)

    let mutable no_lobbies = false
    // todo: loading state

    let refresh_list () = Network.client.Send(Upstream.GET_LOBBIES)
    let create_lobby () = CreateLobbyPage().Show()

    member this.UpdateList (lobbies: LobbyInfo array) =
        no_lobbies <- lobbies.Length = 0
        list_container.Clear()

        for lobby in lobbies do
            list_container.Add(LobbyInfoCard lobby)

    override this.Init(parent) =
        this
        |+ ScrollContainer(list_container, Position = Position.Shrink(0.0f, 80.0f), Margin = Style.PADDING)
        |+ EmptyState(Icons.USERS, %"lobby_list.none", Subtitle = %"lobby_list.none.subtitle")
            .Conditional(fun () -> no_lobbies)
        |+ IconButton(
            %"lobby_list.create",
            Icons.PLUS_CIRCLE,
            60.0f,
            create_lobby,
            Position = Position.SliceB(60.0f).ShrinkR(250.0f)
        )
        |+ IconButton(
            %"lobby_list.refresh",
            Icons.REFRESH_CCW,
            60.0f,
            refresh_list,
            Position = Position.SliceB(60.0f).SliceR(250.0f)
        )
        |* SearchBox(
            search_text,
            (fun (query: string) -> list_container.Filter <- fun l -> l.Name.Contains(query, System.StringComparison.InvariantCultureIgnoreCase)),
            Position = Position.SliceT 60.0f
        )
        base.Init parent

        refresh_list()