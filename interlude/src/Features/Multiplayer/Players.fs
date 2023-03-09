namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Common
open Interlude.Web.Shared
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Online

type Player(name: string, player: Network.LobbyPlayer) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect this.Bounds (Style.dark 100 ())
        let icon = 
            match player.Status with
            | LobbyPlayerStatus.Ready -> Icons.ready
            | LobbyPlayerStatus.Playing -> Icons.play
            | LobbyPlayerStatus.Spectating -> Icons.preview
            | LobbyPlayerStatus.AbandonedPlay -> Icons.not_ready
            | LobbyPlayerStatus.MissingChart -> Icons.connection_failed
            | LobbyPlayerStatus.NotReady
            | _ -> ""
        Text.drawFillB(Style.baseFont, name, this.Bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.LEFT)
        Text.drawFillB(Style.baseFont, icon, this.Bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.RIGHT)

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)

        if Mouse.hover this.Bounds && Mouse.leftClick() then
            ConfirmPage(Localisation.localiseWith [name] "lobby.confirm_transfer_host", fun () -> Lobby.transfer_host name) |> Menu.ShowPage

    member this.Name = name

type PlayerList() =
    inherit StaticContainer(NodeType.None)

    let other_players = FlowContainer.Vertical<Player>(50.0f, Spacing = 5.0f)
    let other_players_scroll = ScrollContainer.Flow(other_players, Position = Position.TrimTop 60.0f)

    let refresh() =
        other_players.Clear()
        match Network.lobby with
        | None -> Logging.Error "Tried to update player list while not in a lobby"
        | Some l ->
            for username in l.Players.Keys do
                other_players.Add(Player(username, l.Players.[username]))

    override this.Init(parent) =
        this |* other_players_scroll
        refresh()
        
        Network.Events.join_lobby.Add refresh
        Network.Events.lobby_players_updated.Add refresh
        
        base.Init parent

    override this.Draw() =
        let user_bounds = this.Bounds.SliceTop(55.0f)
        Draw.rect user_bounds (Style.main 100 ())
        Text.drawFillB(Style.baseFont, Network.username, user_bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.LEFT)
        Text.drawFillB(Style.baseFont, (if (match Network.lobby with Some l -> l.YouAreHost | None -> false) then Icons.star + " Host" else ""), user_bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.RIGHT)

        base.Draw()