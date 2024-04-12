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
        |+ Text(info.Name, Position = Position.SliceTop(50.0f).Margin(5.0f), Align = Alignment.LEFT)
        |+ Text(
            (match info.CurrentlyPlaying with
             | None -> %"lobby.no_song_selected"
             | Some s -> s),
            Color = K Colors.text_subheading,
            Position = Position.SliceBottom(40.0f).Margin(5.0f),
            Align = Alignment.LEFT
        )
        |+ Clickable(fun () -> Lobby.join info.Id)
        |* Text(
            info.Players.ToString() + " " + Icons.USERS,
            Color = K Colors.text_subheading,
            Position = Position.SliceTop(50.0f).Margin(5.0f),
            Align = Alignment.RIGHT
        )

        base.Init parent

    member this.Name = info.Name

type LobbyList() =
    inherit Container(NodeType.None)

    let searchtext = Setting.simple ""

    let container =
        FlowContainer.Vertical<LobbyInfoCard>(80.0f, Spacing = 10.0f, Position = Position.Margin(0.0f, 80.0f))

    let mutable no_lobbies = false

    let create_lobby () = CreateLobbyPage().Show()

    member this.UpdateList() =
        container.Clear()
        no_lobbies <- Network.lobby_list.Length = 0

        for l in Network.lobby_list do
            container.Add(LobbyInfoCard l)

    override this.Init(parent) =
        this
        |+ container
        |+ Conditional(
            (fun () -> no_lobbies),
            EmptyState(Icons.USERS, %"lobby_list.none", Subtitle = %"lobby_list.none.subtitle")
        )
        |+ IconButton(
            %"lobby_list.create",
            Icons.PLUS_CIRCLE,
            60.0f,
            create_lobby,
            Position = Position.SliceBottom(60.0f).TrimRight(250.0f)
        )
        |+ IconButton(
            %"lobby_list.refresh",
            Icons.REFRESH_CCW,
            60.0f,
            Lobby.refresh_list,
            Position = Position.SliceBottom(60.0f).SliceRight(250.0f)
        )
        |* SearchBox(
            searchtext,
            (fun () -> container.Filter <- fun l -> l.Name.ToLower().Contains searchtext.Value),
            Position = Position.SliceTop 60.0f
        )

        this.UpdateList()
        base.Init parent