open System
open Percyqaz.Common
open Interlude.Web.Shared

Logging.Info "Make sure you have the server running"

type Status =
    | Disconnected
    | Connected
    | LoggedIn
    | InLobby of int

let NUMBER_OF_CLIENTS = 100

type TestClient(i: int) =
    inherit Client(System.Net.IPAddress.Parse("127.0.0.1"), 32767)

    let mutable status = Disconnected

    member this.Status = status

    override this.OnConnected() = 
        status <- Connected
        Logging.Info(sprintf "Client %i connected" i)

    override this.OnDisconnected() = 
        Logging.Info(sprintf "Client %i disconnected" i)
        status <- Disconnected

    override this.OnPacketReceived(packet: Downstream) =
        match packet with
        | Downstream.HANDSHAKE_SUCCESS -> this.Send(Upstream.LOGIN (sprintf "Test user %i" i))
        | Downstream.LOGIN_SUCCESS username -> 
            Logging.Info(sprintf "%s logged in " username)
            status <- LoggedIn
            if i = 0 then
                this.Send(Upstream.GET_LOBBIES)
        | Downstream.LOBBY_LIST lobbies ->
            if lobbies.Length = 0 then
                this.Send(Upstream.CREATE_LOBBY "Test lobby")
            else this.Send(Upstream.JOIN_LOBBY(lobbies.[0].Id))
        | Downstream.YOU_JOINED_LOBBY ps -> 
            if i = 0 then
                for j = 1 to NUMBER_OF_CLIENTS - 1 do
                    this.Send(Upstream.INVITE_TO_LOBBY (sprintf "Test user %i" j))
            status <- InLobby 1
            this.Send(Upstream.READY_STATUS (i % 2 = 1))
        | Downstream.LOBBY_SETTINGS s ->
            Logging.Info(sprintf "%i now in lobby: %A" i s)
        | Downstream.INVITED_TO_LOBBY (inviter, id) ->
            Logging.Info(sprintf "Client %i accepts invite from '%s'" i inviter)
            this.Send(Upstream.JOIN_LOBBY id)
        | Downstream.PLAYER_JOINED_LOBBY user ->
            match status with 
            | InLobby n -> 
                status <- InLobby (n + 1)
                if i = 0 && n + 1 = NUMBER_OF_CLIENTS then this.Send(Upstream.LEAVE_LOBBY)
            | _ -> ()
        | Downstream.YOU_ARE_HOST ->
            if i <> 0 && i <> NUMBER_OF_CLIENTS - 1 then
                this.Send(Upstream.LEAVE_LOBBY)
        | Downstream.YOU_LEFT_LOBBY ->
            Logging.Info(sprintf "%i left lobby" i)
            status <- LoggedIn
        | Downstream.SYSTEM_MESSAGE s ->
            Logging.Info(sprintf "@~> %i: %s" i s)
        | _ -> ()

let clients = Array.init NUMBER_OF_CLIENTS TestClient

for c in clients do
    c.Connect()

Console.ReadLine() |> ignore