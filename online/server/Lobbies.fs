namespace Interlude.Web.Server

open System
open System.Collections.Generic
open System.Linq
open System.IO
open Percyqaz.Common
open Interlude.Web.Shared

type LobbyId = Guid
type PlayerId = Guid

type Player =
    {
        Username: string
        mutable Status: LobbyPlayerStatus
        mutable CurrentPlayBuffer: MemoryStream
        mutable PlayPacketsReceived: int
        mutable PlayComplete: bool
    }
    static member Create name =
        { 
            Username = name
            Status = LobbyPlayerStatus.NotReady
            CurrentPlayBuffer = new MemoryStream([||], false)
            PlayPacketsReceived = 0
            PlayComplete = false
        }
    member this.StartPlay() =
        this.Status <- LobbyPlayerStatus.Playing
        this.PlayComplete <- false
        this.CurrentPlayBuffer <- new MemoryStream()
        this.PlayPacketsReceived <- 0
    member this.FinishPlay() =
        this.PlayComplete <- true
    member this.ReceivePlayPacket(data: byte array) : Result<unit, string> =
        if this.Status <> LobbyPlayerStatus.Playing || this.PlayComplete then
            Error "Sent play packet while not playing"
        elif data.Length % 6 > 0 then
            // todo: check 4 bytes every 6 for a monotone increasing value? otherwise flag as garbage and kick player
            Error "Sending garbage data"
        else
            this.CurrentPlayBuffer.Write(data, 0, data.Length)
            this.PlayPacketsReceived <- this.PlayPacketsReceived + 1
            if int this.CurrentPlayBuffer.Length / (this.PlayPacketsReceived * MULTIPLAYER_REPLAY_DELAY_SECONDS) > PLAY_PACKET_THRESHOLD_PER_SECOND then
                Error "Too much data sent too often"
            else Ok()
    member this.GetReplay() = this.CurrentPlayBuffer.ToArray()
        

type Lobby =
    {
        Owner: PlayerId
        mutable Settings: LobbySettings
        mutable Host: PlayerId
        mutable Chart: LobbyChart option
        mutable GameRunning: bool
        Players: Dictionary<PlayerId, Player>
    }
    static member Create (playerId, username, name) =
        {
            Owner = playerId
            Settings = { Name = name; AutomaticRoundCountdown = false; HostRotation = false }
            Host = playerId
            Chart = None
            GameRunning = false
            Players = 
                let d = Dictionary<PlayerId, Player>()
                d.Add(playerId, Player.Create username)
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
        | SelectChart of player: PlayerId * chart: LobbyChart
        | StartGame of player: PlayerId
        | BeginPlaying of player: PlayerId
        | FinishPlaying of player: PlayerId * abandoned: bool
        | BeginSpectating of player: PlayerId
        | PlayData of player: PlayerId * data: byte array
        | GameplayTimeout of lobby: LobbyId
        | Settings of player: PlayerId * settings: LobbySettings
        | ChangeHost of player: PlayerId * newhost: string
        | MissingChart of player: PlayerId

    let private lobbies = Dictionary<LobbyId, Lobby>()
    let private in_lobby = Dictionary<PlayerId, LobbyId>()

    let private get_player_lobby_id(player: PlayerId) : LobbyId option =
        if in_lobby.ContainsKey player then Some in_lobby.[player] else None

    let private valid_lobby_name (proposed: string) : bool =
        if (proposed.Length < 2 || proposed.Length > 30) then false else

        if proposed.Trim().Length <> proposed.Length then false else

        (Seq.forall (fun (c: char) -> Seq.contains c UserState.VALID_USERNAME_CHARACTERS) proposed)

    let private multicast(lobby: Lobby, packet: Downstream) =
        for p in lobby.Players.Keys do
            Server.send(p, packet)
    
    let private multicast_except(id: PlayerId, lobby: Lobby, packet: Downstream) =
        for p in lobby.Players.Keys do
            if p <> id then Server.send(p, packet)

    let private transfer_host(lobby: Lobby, new_host: PlayerId) =
        Server.send(lobby.Host, Downstream.YOU_ARE_HOST false)
        Server.send(new_host, Downstream.YOU_ARE_HOST true)
        multicast(lobby, Downstream.LOBBY_EVENT(LobbyEvent.Host, lobby.Players.[new_host].Username))
        lobby.Host <- new_host

    let private game_end(lobby: Lobby) =
        lobby.GameRunning <- false
        multicast(lobby, Downstream.GAME_END)
        Logging.Debug(sprintf "End of round in lobby %s" lobby.Settings.Name)
        if lobby.Players.Values.Any(fun p -> p.Status = LobbyPlayerStatus.Playing && not p.PlayComplete) then
            // Somebody was a straggler
            Logging.Debug(sprintf "%s round (%s) did not end cleanly" lobby.Settings.Name lobby.Chart.Value.Title)
            if lobby.Settings.HostRotation then
                Server.send(lobby.Host, Downstream.SYSTEM_MESSAGE "Round didn't end properly so you are still host")
        else if lobby.Settings.HostRotation && lobby.Players.Values.Any(fun p -> p.Status = LobbyPlayerStatus.Playing && p.PlayComplete) then
            // Somebody played the song successfully and nobody was a straggler, hence the round went ok, time for host rotate
            let user_ids = ResizeArray(lobby.Players.Keys)
            let host_index = (user_ids.IndexOf(lobby.Host) + 1) % user_ids.Count
            transfer_host(lobby, user_ids[host_index])
        for p in lobby.Players.Values do
            p.Status <- LobbyPlayerStatus.NotReady

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
                        Server.send(player, Downstream.YOU_ARE_HOST true)
                        Server.send(player, Downstream.LOBBY_SETTINGS lobby.Settings)
                        Logging.Info (sprintf "Opened lobby: %s (%O)" lobby.Settings.Name lobby_id)

                    | Action.Join (player, lobby_id) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in to join a lobby")
                        | Some username ->

                        match get_player_lobby_id player with
                        | Some _ -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are already in a lobby")
                        | None ->

                        if not (lobbies.ContainsKey lobby_id) then Server.send(player, Downstream.SYSTEM_MESSAGE "Lobby does not exist (or no longer exists)")
                        else

                        let lobby = lobbies.[lobby_id]
                        let player_list = lobby.Players.Values |> Seq.map (fun p -> p.Username) |> Array.ofSeq

                        multicast(lobby, Downstream.PLAYER_JOINED_LOBBY username)
                        multicast(lobby, Downstream.LOBBY_EVENT(LobbyEvent.Join, username))
                            
                        in_lobby.Add(player, lobby_id)
                        lobby.Players.Add(player, Player.Create username)

                        Server.send(player, Downstream.YOU_JOINED_LOBBY player_list)
                        Server.send(player, Downstream.LOBBY_SETTINGS lobby.Settings)
                        if lobby.Chart.IsSome then Server.send(player, Downstream.SELECT_CHART lobby.Chart.Value)
                        for p in lobby.Players.Values do
                            if p.Status <> LobbyPlayerStatus.NotReady then Server.send(player, Downstream.PLAYER_STATUS(p.Username, p.Status))
                        if lobby.GameRunning then Server.send(player, Downstream.GAME_START)

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

                        multicast(lobby, Downstream.PLAYER_LEFT_LOBBY username)
                        multicast(lobby, Downstream.LOBBY_EVENT(LobbyEvent.Leave, username))
                        Server.send(player, Downstream.YOU_LEFT_LOBBY)

                        if lobby.Players.Count = 0 then
                            lobbies.Remove lobby_id |> ignore
                            Logging.Info (sprintf "Closed lobby: %s (%O)" lobby.Settings.Name lobby_id)
                        else
                            if lobby.GameRunning && lobby.Players.Values.Any(fun p -> p.Status = LobbyPlayerStatus.Playing && not p.PlayComplete) |> not then game_end lobby
                            if lobby.Host = player then
                                lobby.Host <- Seq.head lobby.Players.Keys
                                Server.send(lobby.Host, Downstream.YOU_ARE_HOST true)
                                multicast(lobby, Downstream.LOBBY_EVENT(LobbyEvent.Host, lobby.Players.[lobby.Host].Username))

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

                        if in_lobby.ContainsKey recipient_id && in_lobby.[recipient_id] = lobby_id then
                            Server.send(sender, Downstream.SYSTEM_MESSAGE "User is already in this lobby")
                        else

                        Server.send(recipient_id, Downstream.INVITED_TO_LOBBY (username, lobby_id))
                        multicast(lobbies.[lobby_id], Downstream.LOBBY_EVENT(LobbyEvent.Invite, recipient))
                            
                    | Action.Chat (player, message) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                        
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        multicast(lobbies.[lobby_id], Downstream.CHAT(username, message))

                    | Action.ReadyUp (player, ready) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                        
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        let lobby = lobbies.[lobby_id]

                        let old_status = lobby.Players.[player].Status
                        match old_status with
                        | LobbyPlayerStatus.Playing
                        | LobbyPlayerStatus.AbandonedPlay
                        | LobbyPlayerStatus.Spectating -> Server.kick(player, "Ready status changed while playing/spectating")
                        | _ ->

                        let new_status = if ready then LobbyPlayerStatus.Ready else LobbyPlayerStatus.NotReady
                        if new_status <> old_status then

                            lobby.Players.[player].Status <- new_status
                            
                            multicast_except(player, lobbies.[lobby_id], Downstream.PLAYER_STATUS(username, new_status))
                            multicast(lobby, Downstream.LOBBY_EVENT((if ready then LobbyEvent.Ready else LobbyEvent.NotReady), username))

                    | Action.SelectChart (player, chart) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                        
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        let lobby = lobbies.[lobby_id]

                        if lobby.Host <> player then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "You are not host")
                        else

                        if lobby.GameRunning then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "Game is currently running")
                        else

                        if lobby.Chart <> Some chart then

                            lobby.Chart <- Some chart
                            for p in lobby.Players.Values do
                                p.Status <- LobbyPlayerStatus.NotReady

                            multicast(lobby, Downstream.SELECT_CHART chart)

                    | Action.StartGame player ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                    
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->
                        
                        let lobby = lobbies.[lobby_id]
                        
                        if lobby.Host <> player then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "You are not host")
                        else
                            
                        if lobby.Chart.IsNone then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "No chart selected")
                        else
                        
                        if lobby.GameRunning then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "Game is already running")
                        else

                        lobby.GameRunning <- true
                        multicast(lobby, Downstream.GAME_START)

                    | Action.BeginPlaying player ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                    
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->
                        
                        let lobby = lobbies.[lobby_id]
                        
                        if not lobby.GameRunning then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "Game is not running")
                        else

                        lobby.Players.[player].StartPlay()
                        multicast_except(player, lobby, Downstream.PLAYER_STATUS (username, lobby.Players.[player].Status))
                        // todo? if anyone has sent play data you are probably starting too late and should be kicked

                    | Action.FinishPlaying (player, abandoned) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                    
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->
                        
                        let lobby = lobbies.[lobby_id]
                        
                        if not lobby.GameRunning then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "Game is not running")
                        else

                        if lobby.Players.[player].PlayComplete then
                            Server.kick(player, "Play finish packet already sent")
                        else

                        let plr = lobby.Players.[player]
                        plr.FinishPlay()

                        if abandoned then
                            plr.Status <- LobbyPlayerStatus.AbandonedPlay
                            multicast_except(player, lobby, Downstream.PLAYER_STATUS (username, plr.Status))

                        if lobby.Players.Values.Any(fun p -> p.Status = LobbyPlayerStatus.Playing && not p.PlayComplete) |> not then
                            // you are last player in the lobby to finish
                            game_end lobby
                        elif not abandoned && lobby.Players.Values.Any(fun p -> p <> plr && p.Status = LobbyPlayerStatus.Playing && p.PlayComplete) |> not then
                            // you are first player in the lobby to finish
                            Logging.Debug(sprintf "First player to finish is %s, starting timeout" username)
                            async {
                                do! Async.Sleep(1000 * MULTIPLAYER_REPLAY_DELAY_SECONDS)
                                this.Request(Action.GameplayTimeout lobby_id, ignore)
                            } |> Async.Start

                    | Action.BeginSpectating player ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                    
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->
                        
                        let lobby = lobbies.[lobby_id]
                        
                        if not lobby.GameRunning then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "Game is not running")
                        else

                        match lobby.Players.[player].Status with
                        | LobbyPlayerStatus.Playing
                        | LobbyPlayerStatus.Spectating -> Server.kick(player, "Spectated while already playing/spectating")
                        | _ ->

                        lobby.Players.[player].Status <- LobbyPlayerStatus.Spectating
                        multicast_except(player, lobby, Downstream.PLAYER_STATUS (username, LobbyPlayerStatus.Spectating))
                        for p in lobby.Players.Values do
                            if p.Status = LobbyPlayerStatus.Playing && p.PlayPacketsReceived > 0 then
                                // todo: break play data that is too large into smaller packets, for when you start spectating late into a song
                                Server.send(player, Downstream.PLAY_DATA(p.Username, p.GetReplay()))

                    | Action.PlayData (player, data) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                    
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->
                        
                        let lobby = lobbies.[lobby_id]
                        
                        if not lobby.GameRunning then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "Replay sent while game is not running")
                        else

                        match lobby.Players.[player].ReceivePlayPacket(data) with
                        | Ok() -> 
                            for p in lobby.Players.Keys do
                                if p <> player && (lobby.Players.[p].Status = LobbyPlayerStatus.Playing || lobby.Players.[p].Status = LobbyPlayerStatus.Spectating) then
                                    Server.send(p, Downstream.PLAY_DATA(username, data))
                        | Error reason -> Server.kick(player, reason)

                    | Action.GameplayTimeout lobby_id ->
                        if not (lobbies.ContainsKey lobby_id) then
                            Logging.Debug("Lobby closed before gameplay timeout")
                        else
                        
                        Logging.Debug(sprintf "Timeout called on lobby with id %O" lobby_id)

                        // todo: fix bug where if you start a new round before this timeout, it will end instantly

                        if lobbies.[lobby_id].GameRunning then game_end lobbies.[lobby_id]

                    | Action.Settings (player, settings) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                    
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        let lobby = lobbies.[lobby_id]
                        
                        if lobby.Host <> player then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "You are not host")
                        else

                        lobby.Settings <- settings
                        multicast(lobby, Downstream.LOBBY_SETTINGS settings)
                    
                    | Action.ChangeHost (player, newhost) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                                        
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->
                    
                        let lobby = lobbies.[lobby_id]
                                            
                        if lobby.Host <> player then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "You are not host")
                        else

                        match! UserState.find_session newhost with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "User is not in this lobby")
                        | Some newhost_id ->
                            
                        if player = newhost_id then Server.kick(player, "Hosting yourself")
                        else

                        if not (in_lobby.ContainsKey newhost_id) || in_lobby.[newhost_id] <> lobby_id then
                            Server.send(player, Downstream.SYSTEM_MESSAGE "User is not in this lobby")
                        else
                        
                        transfer_host(lobby, newhost_id)

                    | Action.MissingChart (player) ->
                        match! UserState.find_username player with
                        | None -> Server.kick(player, "Must be logged in")
                        | Some username ->
                        
                        match get_player_lobby_id player with
                        | None -> Server.send(player, Downstream.SYSTEM_MESSAGE "You are not in a lobby")
                        | Some lobby_id ->

                        let lobby = lobbies.[lobby_id]

                        if lobby.Players.[player].Status <> LobbyPlayerStatus.NotReady then
                            Server.kick(player, "Declared missing a chart but status indicates otherwise")
                        else

                        lobby.Players.[player].Status <- LobbyPlayerStatus.MissingChart
                            
                        multicast_except(player, lobbies.[lobby_id], Downstream.PLAYER_STATUS(username, LobbyPlayerStatus.MissingChart))
            }
        }

    let create(player, name) = state_change.Request( Action.Create(player, name) , ignore )
    let join(player, id) = state_change.Request( Action.Join(player, id) , ignore )
    let leave(player) = state_change.Request( Action.Leave(player) , ignore )
    let invite(player, target) = state_change.Request( Action.Invite(player, target) , ignore )
    let chat(player, message) = state_change.Request( Action.Chat(player, message) , ignore )
    let ready_up(player, ready) = state_change.Request( Action.ReadyUp(player, ready) , ignore )
    let select_chart(player, chart) = state_change.Request( Action.SelectChart(player, chart) , ignore )
    let start_game(player) = state_change.Request( Action.StartGame(player) , ignore )
    let begin_playing(player) = state_change.Request( Action.BeginPlaying(player) , ignore )
    let finish_playing(player) = state_change.Request( Action.FinishPlaying(player) , ignore )
    let begin_spectating(player) = state_change.Request( Action.BeginSpectating(player) , ignore )
    let play_data(player, data) = state_change.Request( Action.PlayData(player, data) , ignore )
    let settings(player, settings) = state_change.Request( Action.Settings(player, settings) , ignore )
    let change_host(player, newhost) = state_change.Request( Action.ChangeHost(player, newhost) , ignore )
    let missing_chart(player) = state_change.Request( Action.MissingChart(player), ignore )

    let list(player) = state_change.Request( Action.List player, ignore )

    let user_disconnected(player, callback) =
        if in_lobby.ContainsKey(player) then state_change.Request( Action.Leave(player), callback)
        else callback()