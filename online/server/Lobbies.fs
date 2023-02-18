namespace Interlude.Web.Server

open System
open System.Collections.Generic
open Percyqaz.Common
open Interlude.Web.Shared

type LobbyId = Guid
type PlayerId = Guid

type LobbyPlayerStatus =
    | NotReady
    | Ready
    | Spectating
    | Playing

type LobbyPlayer =
    {
        Username: string
        mutable Status: LobbyPlayerStatus
    }
    static member Create name = { Username = name; Status = NotReady }

type Lobby =
    {
        Owner: PlayerId
        mutable Settings: LobbySettings
        mutable Host: PlayerId
        mutable Chart: LobbyChart option
        Players: Dictionary<PlayerId, LobbyPlayer>
    }
    static member Create (playerId, username, name) =
        {
            Owner = playerId
            Settings = { Name = name }
            Host = playerId
            Chart = None
            Players = 
                let d = Dictionary<PlayerId, LobbyPlayer>()
                d.Add(playerId, LobbyPlayer.Create username)
                d
        }

module Lobby = 

    [<RequireQualifiedAccess>]
    type Action =
        | List of player: PlayerId
        | Create of player: PlayerId * name: string
        | Join of player: PlayerId * id: LobbyId
        | Leave of player: PlayerId
        | Invite of invitor: PlayerId * invitee: string
        | Chat of sender: PlayerId * msg: string
        | ReadyUp of player: PlayerId * isReady: bool

    let private lobbies = Dictionary<LobbyId, Lobby>()
    let private in_lobby = Dictionary<PlayerId, LobbyId>()

    let private get_player_lobby_id(player: PlayerId) : LobbyId option =
        if in_lobby.ContainsKey player then Some in_lobby.[player] else None

    let valid_lobby_name (proposed: string) : bool =
        if (proposed.Length < 2 || proposed.Length > 30) then false else

        if proposed.Trim().Length <> proposed.Length then false else

        (Seq.forall (fun (c: char) -> Seq.contains c UserState.VALID_USERNAME_CHARACTERS) proposed)

    let private state_change = 
        { new Async.Service<Action, unit>()
            with override this.Handle req = async {
                    match req with

                    | Action.List (player) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in to list lobbies")
                        | Some _ ->

                        let lobbies = 
                            seq {
                                for lobby_id in lobbies.Keys do
                                    let lobby = lobbies.[lobby_id]
                                    yield { 
                                        Id = lobby_id
                                        Name = lobby.Settings.Name
                                        Players = lobby.Players.Count |> byte
                                        CurrentlyPlaying = match lobby.Chart with Some c -> Some (c.Artist + " - " + c.Title) | None -> None
                                    }
                            }
                            |> Array.ofSeq

                        Server.send(player, Downstream.LOBBY_LIST lobbies)

                    | Action.Create (player, lobby_name) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in to create a lobby")
                        | Some username ->

                        let lobby_name = if valid_lobby_name lobby_name then lobby_name else username + "'s lobby"
                        let lobby_id = Guid.NewGuid()

                        let lobby = Lobby.Create(player, username, lobby_name)

                        lobbies.Add(lobby_id, lobby)
                        in_lobby.Add(player, lobby_id)
                        Server.send(player, Downstream.YOU_JOINED_LOBBY [||])
                        Server.send(player, Downstream.YOU_ARE_HOST)
                        Server.send(player, Downstream.LOBBY_SETTINGS lobby.Settings)
                        Logging.Info (sprintf "Opened lobby: %s (%O)" lobby.Settings.Name lobby_id)

                    | Action.Join (player, lobby_id) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in to join a lobby")
                        | Some username ->

                        match get_player_lobby_id player with
                        | Some _ -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are already in a lobby")
                        | None ->

                        if not (lobbies.ContainsKey lobby_id) then Server.send(player, Downstream.SYSTEM_MESSAGE "Lobby does not exist")
                        else

                        let lobby = lobbies.[lobby_id]
                        let player_list = lobby.Players.Values |> Seq.map (fun p -> p.Username) |> Array.ofSeq

                        for p in lobby.Players.Keys do
                            Server.send(p, Downstream.PLAYER_JOINED_LOBBY username)
                            
                        in_lobby.Add(player, lobby_id)
                        lobby.Players.Add(player, LobbyPlayer.Create username)
                        Logging.Info(sprintf "%s joined lobby %O, %i players now in lobby" username lobby_id lobby.Players.Count)

                        Server.send(player, Downstream.YOU_JOINED_LOBBY player_list)
                        // todo: send them all the ready statuses too
                        Server.send(player, Downstream.LOBBY_SETTINGS lobby.Settings)

                    | Action.Leave player ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->

                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        let lobby = lobbies.[lobby_id]
                        
                        lobby.Players.Remove player |> ignore
                        in_lobby.Remove player |> ignore

                        for p in lobby.Players.Keys do
                            Server.send(p, Downstream.PLAYER_LEFT_LOBBY username)
                        Server.send(player, Downstream.YOU_LEFT_LOBBY)

                        Logging.Info(sprintf "%s left lobby %O, %i players remain" username lobby_id lobby.Players.Count)

                        if lobby.Players.Count = 0 then
                            lobbies.Remove lobby_id |> ignore
                            Logging.Info (sprintf "Closed lobby: %s (%O)" lobby.Settings.Name lobby_id)
                        else
                            if lobby.Host = player then 
                                lobby.Host <- Seq.head lobby.Players.Keys
                                Server.send(lobby.Host, Downstream.YOU_ARE_HOST)

                    | Action.Invite (sender, recipient) ->
                        match! UserState.find_username sender with
                        | None -> Server.kick(sender, "Must be logged in")
                        | Some username ->
                        
                        match get_player_lobby_id sender with
                        | None -> Server.send(sender, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        match! UserState.find_session recipient with
                        | None -> Server.send(sender, Downstream.SYSTEM_MESSAGE "User not found")
                        | Some recipient_id ->

                        Server.send(recipient_id, Downstream.INVITED_TO_LOBBY (username, lobby_id))
                            
                    | Action.Chat (player, message) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                        
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        let lobby = lobbies.[lobby_id]

                        for p in lobby.Players.Keys do
                            Server.send(p, Downstream.CHAT(username, message))

                    | Action.ReadyUp (player, ready) ->

                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                        
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        let lobby = lobbies.[lobby_id]

                        lobby.Players.[player].Status <- if ready then Ready else NotReady

                        for p in lobby.Players.Keys do
                            Server.send(p, Downstream.READY_STATUS(username, ready))
            }
        }

    let create(player, name) = state_change.Request( Action.Create(player, name) , ignore )
    let join(player, id) = state_change.Request( Action.Join(player, id) , ignore )
    let leave(player) = state_change.Request( Action.Leave(player) , ignore )
    let invite(player, target) = state_change.Request( Action.Invite(player, target) , ignore )
    let chat(player, message) = state_change.Request( Action.Chat(player, message) , ignore )
    let ready_up(player, ready) = state_change.Request( Action.ReadyUp(player, ready) , ignore )

    let list(player) = state_change.Request( Action.List player, ignore )

    let user_disconnected(player) =
        if in_lobby.ContainsKey(player) then state_change.Request( Action.Leave(player), ignore )

