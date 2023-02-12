namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Interlude.UI
open Interlude.UI.Components
open Interlude.Web.Shared

type LobbyInfoCard(info: LobbyInfo) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text(info.Name, Position = Position.SliceTop 50.0f, Align = Alignment.LEFT)
        |+ Text((match info.CurrentlyPlaying with None -> "--" | Some s -> s), Color = Style.text_subheading, Position = Position.SliceBottom 30.0f, Align = Alignment.LEFT)
        |* Text(sprintf "%i players" info.Players, Color = Style.text_subheading, Position = Position.SliceTop(50.0f).Margin(10.0f), Align = Alignment.RIGHT)
        base.Init parent

    member this.Name = info.Name

type LobbyList() =
    inherit StaticContainer(NodeType.None)

    let searchtext = Setting.simple ""

    let container = FlowContainer.Vertical<LobbyInfoCard>(80.0f, Position = Position.Margin (0.0f, 80.0f))
    let mutable no_lobbies = false

    let refresh() =
        container.Clear()
        no_lobbies <- Network.lobby_list.Length = 0
        for l in Network.lobby_list do
            container.Add(LobbyInfoCard l)

    let mutable lobby_creating = false
    let create_lobby() =
        if lobby_creating then () else

        lobby_creating <- true
        Network.create_lobby "My lobby"

    override this.Init(parent) =
        this
        |+ container
        |+ Text((fun _ -> if no_lobbies then "No lobbies" else ""), Align = Alignment.CENTER, Position = Position.TrimTop(100.0f).SliceTop(100.0f))
        |+ Button("Create a lobby", create_lobby, Position = Position.SliceBottom 80.0f)
        |* SearchBox(searchtext, (fun () -> container.Filter <- fun l -> l.Name.ToLower().Contains searchtext.Value), Position = Position.SliceTop 80.0f)
        
        base.Init parent

        refresh()
        Network.Events.receive_lobby_list.Add refresh
        Network.Events.join_lobby.Add (fun () -> lobby_creating <- false)

type LobbyScreen() =
    inherit Screen()

    override this.OnEnter(_) = ()
    override this.OnExit(_) = ()

    override this.Init(parent) =
        this
        |* LobbyList(Position = { Position.Default.Margin (0.0f, 100.0f) with Left = 0.5f %- 300.0f; Right = 0.5f %+ 300.0f })
        
        base.Init parent