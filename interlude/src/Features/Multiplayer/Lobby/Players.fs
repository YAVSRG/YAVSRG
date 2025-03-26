namespace Interlude.Features.Multiplayer

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Interlude.Web.Shared
open Interlude.UI
open Interlude.Features.Online
open Interlude.Features.Online.Players

type Player(lobby: Lobby, name: string, player: LobbyPlayer) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let icon, fill, border =
            match player.Status with
            | LobbyPlayerStatus.Ready -> Icons.CHECK, Colors.green, Colors.green_accent
            | LobbyPlayerStatus.ReadyToSpectate -> Icons.EYE, Colors.green, Colors.green_accent
            | LobbyPlayerStatus.Playing -> Icons.PLAY, Colors.green, Colors.green_accent
            | LobbyPlayerStatus.Spectating -> Icons.EYE, Colors.green, Colors.green_accent
            | LobbyPlayerStatus.AbandonedPlay -> Icons.X, Colors.grey_2.O1, Colors.white
            | LobbyPlayerStatus.MissingChart -> Icons.SLASH, Colors.grey_2.O1, Colors.white
            | LobbyPlayerStatus.NotReady
            | _ -> "", Colors.cyan, Colors.cyan_accent

        let b = this.Bounds.Expand(Style.PADDING)
        Render.rect (b.SliceT Style.PADDING) border.O3
        Render.rect (b.SliceB Style.PADDING) border.O3
        let b2 = this.Bounds.Expand(Style.PADDING, 0.0f)
        Render.rect (b2.SliceR Style.PADDING) border.O3
        Render.rect (b2.SliceL Style.PADDING) border.O3

        Render.rect this.Bounds fill.O3

        Text.fill_b (Style.font, name, this.Bounds.Shrink(10.0f, 0.0f), Colors.text, Alignment.LEFT)
        Text.fill_b (Style.font, icon, this.Bounds.Shrink(10.0f, 0.0f), Colors.text, Alignment.RIGHT)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if lobby.YouAreHost && Mouse.hover this.Bounds && Mouse.left_clicked () then
            ConfirmPage([ name ] %> "lobby.confirm_transfer_host", (fun () -> lobby.TransferHost name))
                .Show()

    member this.Name = name

type PlayerList(lobby: Lobby) =
    inherit Container(NodeType.None)

    let other_players = FlowContainer.Vertical<Widget>(50.0f, Spacing = 5.0f)

    let other_players_scroll =
        ScrollContainer(other_players)
            .Margin(Style.PADDING)
            .Position(Position.ShrinkT(60.0f))

    let refresh () =
        other_players.Clear()

        for username in lobby.Players.Keys do
            other_players.Add(Player(lobby, username, lobby.Players.[username]))

        other_players.Add(
            Button(sprintf "%s %s" Icons.MAIL (%"lobby.send_invite"), (fun () -> PlayerListPage().Show()))
        )

    override this.Init(parent) =
        this |* other_players_scroll
        refresh ()

        lobby.OnPlayersUpdated.Add refresh

        base.Init parent

    override this.Draw() =

        let fill, border = Colors.cyan, Colors.cyan_accent

        let user_bounds = this.Bounds.SliceT(55.0f)

        Render.border Style.PADDING user_bounds border.O3
        Render.rect user_bounds fill.O3

        Text.fill_b (
            Style.font,
            Network.credentials.Username,
            user_bounds.Shrink(10.0f, 0.0f),
            Colors.text,
            Alignment.LEFT
        )

        Text.fill_b (
            Style.font,
            (if lobby.YouAreHost then Icons.STAR + " Host" else ""),
            user_bounds.Shrink(10.0f, 0.0f),
            Colors.text,
            Alignment.RIGHT
        )

        base.Draw()