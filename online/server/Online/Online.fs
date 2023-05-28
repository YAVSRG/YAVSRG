namespace Interlude.Web.Server.Online

open System
open Percyqaz.Common
open Interlude.Web.Shared
open Interlude.Web.Server.Domain

module Online =

    let handle_packet(id: Guid, packet: Upstream) =
        async {
            match packet with
    
            | Upstream.VERSION v -> 
                if v = PROTOCOL_VERSION then LoggedInUsers.handshake id
                else Server.kick(id, "Your client is out of date")
            | Upstream.BEGIN_LOGIN_WITH_DISCORD ->
                AuthFlow.begin_login_with_discord id
            | Upstream.BEGIN_REGISTRATION_WITH_DISCORD ->
                AuthFlow.begin_register_with_discord id
            | Upstream.COMPLETE_REGISTRATION_WITH_DISCORD username ->
                AuthFlow.finish_register_with_discord (id, username)
            | Upstream.LOGIN token ->
                LoggedInUsers.login(id, token)
            | Upstream.LOGOUT ->
                Lobby.ensure_player_leaves_lobby(id, fun () -> LoggedInUsers.logout id)
    
            | Upstream.GET_LOBBIES -> Lobby.List(id).Do
            | Upstream.JOIN_LOBBY lid -> Lobby.Join(id, lid).Do
            | Upstream.CREATE_LOBBY name -> Lobby.Create(id, name).Do
    
            | Upstream.INVITE_TO_LOBBY username -> Lobby.Invite(id, username).Do
            | Upstream.LEAVE_LOBBY -> Lobby.Leave(id).Do
            | Upstream.CHAT msg -> Lobby.Chat(id, msg).Do
            | Upstream.READY_STATUS r -> Lobby.ReadyUp(id, r).Do
            | Upstream.MISSING_CHART -> Lobby.MissingChart(id).Do
    
            | Upstream.BEGIN_PLAYING -> Lobby.BeginPlaying(id).Do
            | Upstream.PLAY_DATA data -> Lobby.PlayData(id, data).Do
            | Upstream.BEGIN_SPECTATING -> Lobby.BeginSpectating(id).Do
            | Upstream.FINISH_PLAYING abandoned -> Lobby.FinishPlaying(id, abandoned).Do
            
            | Upstream.TRANSFER_HOST who -> Lobby.ChangeHost(id, who).Do
            | Upstream.SELECT_CHART c -> Lobby.SelectChart(id, c).Do
            | Upstream.LOBBY_SETTINGS s -> Lobby.Settings(id, s).Do
            | Upstream.START_GAME -> Lobby.StartGame(id).Do
            | Upstream.CANCEL_GAME -> Lobby.CancelStartGame(id).Do
    
            | Upstream.KICK_PLAYER _ -> Server.kick(id, "not yet implemented")
    
        } |> Async.Start
    
    let handle_connect(id: Guid) = LoggedInUsers.connect id

    let handle_disconnect(id: Guid) = 
        Lobby.ensure_player_leaves_lobby (id, fun () -> LoggedInUsers.disconnect id)