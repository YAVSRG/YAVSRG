namespace Interlude.Web.Server.Online

open System
open Interlude.Web.Shared
open Interlude.Web.Server.Domain.Services

module Online =

    let handle_packet (id: Guid, packet: Upstream) =
        async {
            match packet with

            | Upstream.VERSION v ->
                if v = PROTOCOL_VERSION then
                    Session.handshake id
                else
                    Server.kick (id, "Your client is out of date")
            | Upstream.LOGIN_WITH_DISCORD -> Users.DiscordAuthFlow.begin_register_or_login_with_discord id
            | Upstream.COMPLETE_REGISTRATION_WITH_DISCORD username ->
                Users.DiscordAuthFlow.finish_register_with_discord (id, username) |> ignore
            | Upstream.LOGIN token -> Session.login (id, token)
            | Upstream.LOGOUT -> Lobby.ensure_player_leaves_lobby (id, (fun () -> Session.logout id))

            | Upstream.GET_LOBBIES -> Lobby.List(id).Do
            | Upstream.JOIN_LOBBY lid -> Lobby.Join(id, lid).Do
            | Upstream.CREATE_LOBBY name -> Lobby.Create(id, name).Do

            | Upstream.INVITE_TO_LOBBY username -> Lobby.Invite(id, username).Do
            | Upstream.LEAVE_LOBBY -> Lobby.Leave(id).Do
            | Upstream.CHAT msg -> Lobby.Chat(id, msg).Do
            | Upstream.READY_STATUS f -> Lobby.ReadyUp(id, f).Do
            | Upstream.MISSING_CHART -> Lobby.MissingChart(id).Do

            | Upstream.BEGIN_PLAYING -> Lobby.BeginPlaying(id).Do
            | Upstream.PLAY_DATA (timestamp, data) -> Lobby.PlayData(id, timestamp, data).Do
            | Upstream.BEGIN_SPECTATING -> Lobby.BeginSpectating(id).Do
            | Upstream.FINISH_PLAYING abandoned -> Lobby.FinishPlaying(id, abandoned).Do

            | Upstream.TRANSFER_HOST who -> Lobby.ChangeHost(id, who).Do
            | Upstream.SELECT_CHART c -> Lobby.SelectChart(id, c).Do
            | Upstream.LOBBY_SETTINGS s -> Lobby.Settings(id, s).Do
            | Upstream.START_GAME -> Lobby.StartGame(id).Do
            | Upstream.CANCEL_GAME -> Lobby.CancelStartGame(id).Do

            | Upstream.KICK_PLAYER _ -> Server.kick (id, "not yet implemented")

        }
        |> Async.Start

    let handle_connect (id: Guid) = Session.connect id

    let handle_disconnect (id: Guid) =
        Lobby.ensure_player_leaves_lobby (id, (fun () -> Session.disconnect id))