namespace Interlude.Web.Tests.Client

open Interlude.Web.Shared

// Client that logs in, joins a lobby, and then echoes the replay data of a user named 'Percyqaz'
type EchoClient(token: string) =
    inherit Client(System.Net.IPAddress.Parse("127.0.0.1"), 32767)

    let mutable ready_to_play = false

    let mutable status = Disconnected

    member this.Status = status

    override this.OnConnected() = status <- Connected

    override this.OnDisconnected() = status <- Disconnected

    override this.OnPacketReceived(packet: Downstream) =
        match packet with
        | Downstream.HANDSHAKE_SUCCESS -> this.Send(Upstream.LOGIN(token))
        | Downstream.LOGIN_SUCCESS username ->
            status <- LoggedIn
            this.Send(Upstream.GET_LOBBIES)
        | Downstream.LOBBY_LIST lobbies ->
            if lobbies.Length > 0 then this.Send(Upstream.JOIN_LOBBY(lobbies.[0].Id))
        | Downstream.YOU_JOINED_LOBBY ps -> status <- InLobby
        | Downstream.SELECT_CHART _ ->
            this.Send(Upstream.READY_STATUS ReadyFlag.Play)
            ready_to_play <- true
        | Downstream.GAME_START ->
            if ready_to_play then
                this.Send(Upstream.BEGIN_PLAYING)
            else
                this.Send(Upstream.BEGIN_SPECTATING)
        | Downstream.INVITED_TO_LOBBY(_, id) -> this.Send(Upstream.JOIN_LOBBY id)
        | Downstream.YOU_ARE_HOST true -> this.Send(Upstream.TRANSFER_HOST "Percyqaz")
        | Downstream.YOU_LEFT_LOBBY -> status <- LoggedIn
        | Downstream.PLAY_DATA("Percyqaz", ts, data) ->
            if ready_to_play then this.Send(Upstream.PLAY_DATA (ts, data))
        | _ -> ()