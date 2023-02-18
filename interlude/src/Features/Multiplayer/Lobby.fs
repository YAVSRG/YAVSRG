namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.UI
open Interlude.UI.Components
open Interlude.Web.Shared

type LobbyInfoCard(info: LobbyInfo) =
    inherit Frame(NodeType.None)

    override this.Init(parent) =
        this
        |+ Text(info.Name, Position = Position.SliceTop(50.0f).Margin(5.0f), Align = Alignment.LEFT)
        |+ Text((match info.CurrentlyPlaying with None -> "No song selected" | Some s -> s), Color = Style.text_subheading, Position = Position.SliceBottom(40.0f).Margin(5.0f), Align = Alignment.LEFT)
        |+ Clickable(fun () -> Network.join_lobby info.Id)
        |* Text(info.Players.ToString() + " " + Icons.multiplayer, Color = Style.text_subheading, Position = Position.SliceTop(50.0f).Margin(5.0f), Align = Alignment.RIGHT)
        base.Init parent

    member this.Name = info.Name

type LobbyList() =
    inherit StaticContainer(NodeType.None)

    let searchtext = Setting.simple ""

    let container = FlowContainer.Vertical<LobbyInfoCard>(80.0f, Spacing = 10.0f, Position = Position.Margin (0.0f, 70.0f))
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
        |+ Button("Create a lobby", create_lobby, Position = Position.SliceBottom 60.0f)
        |* SearchBox(searchtext, (fun () -> container.Filter <- fun l -> l.Name.ToLower().Contains searchtext.Value), Position = Position.SliceTop 60.0f)
        
        base.Init parent

        refresh()
        Network.Events.receive_lobby_list.Add refresh
        Network.Events.join_lobby.Add (fun () -> lobby_creating <- false)

type Player(name: string, player: Network.LobbyPlayer) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect this.Bounds (Style.dark 100 ())
        Text.drawFillB(Style.baseFont, name, this.Bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.LEFT)
        Text.drawFillB(Style.baseFont, (if player.IsReady then "Ready" else ""), this.Bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.RIGHT)

    member this.Name = name

type PlayerList() =
    inherit StaticContainer(NodeType.None)

    let other_players = FlowContainer.Vertical<Player>(30.0f, Spacing = 5.0f, Position = Position.TrimTop 40.0f)

    let refresh() =
        other_players.Clear()
        match Network.lobby with
        | None -> Logging.Error "Tried to update player list while not in a lobby"
        | Some l ->
            for username in l.Players.Keys do
                other_players.Add(Player(username, l.Players.[username]))

    override this.Init(parent) =
        this |* other_players
        
        base.Init parent

    override this.Draw() =
        let user_bounds = this.Bounds.SliceTop(35.0f)
        Draw.rect user_bounds (Style.main 100 ())
        Text.drawFillB(Style.baseFont, Network.username, user_bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.LEFT)
        Text.drawFillB(Style.baseFont, (if (match Network.lobby with Some l -> l.YouAreHost | None -> false) then Icons.star + " Host" else ""), user_bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.RIGHT)

type Lobby() =
    inherit StaticContainer(NodeType.None)

    let mutable lobby_title = "Loading..."

    override this.Init(parent) =
        this
        |+ Text((fun () -> lobby_title), Align = Alignment.LEFT, Position = Position.SliceTop(80.0f).Margin(15.0f, 0.0f))
        |+ PlayerList(Position = Position.SliceLeft(600.0f).Margin(5.0f, 100.0f))
        |+ StylishButton(Network.leave_lobby, K "Leave lobby", Style.main 100, TiltLeft = false, Position = Position.Column(0.0f, 300.0f).SliceBottom(50.0f))
        |* StylishButton(ignore, K "Invite player", Style.dark 100, Position = Position.Column(300.0f, 300.0f).SliceBottom(50.0f))
        
        base.Init parent

        Network.Events.lobby_settings_updated.Add(fun () -> lobby_title <- Network.lobby.Value.Settings.Value.Name)

type LobbyScreen() =
    inherit Screen()

    let mutable in_lobby = false

    let list = LobbyList(Position = { Position.Default.Margin (0.0f, 100.0f) with Left = 0.5f %- 300.0f; Right = 0.5f %+ 300.0f })
    let main = Lobby()

    let swap = SwapContainer(Current = list)

    override this.OnEnter(_) =
        in_lobby <- Network.lobby.IsSome
        swap.Current <- if in_lobby then main :> Widget else list
        if not in_lobby then Network.refresh_lobby_list()
    override this.OnExit(_) = ()

    override this.Init(parent) =
        this |* swap
        
        base.Init parent
        Network.Events.join_lobby.Add (fun () -> in_lobby <- true; swap.Current <- main)
        Network.Events.leave_lobby.Add (fun () -> in_lobby <- false; swap.Current <- list)