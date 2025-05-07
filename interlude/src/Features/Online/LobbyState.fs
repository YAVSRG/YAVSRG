namespace Interlude.Features.Online

open System
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Mods
open Prelude.Data.Library
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Web.Shared

type LobbyInvite =
    {
        InvitedBy: string
        LobbyId: Guid
    }

type LobbyPlayer =
    {
        Color: Color
        mutable Status: LobbyPlayerStatus
    }
    static member Create color =
        {
            Color = Color.FromArgb color
            Status = LobbyPlayerStatus.NotReady
        }

type LobbyPlayerReplayInfo =
    {
        Replay: IReplay
        ScoreProcessor: ScoreProcessor
        GetScoreInfo: unit -> ScoreInfo
    }

type Lobby(client: Client, your_username: string, players: (string * int32) array) =

    let players =
        let d = new Dictionary<string, LobbyPlayer>()
        for (username, color) in players do
            d.Add(username, LobbyPlayer.Create color)
        d

    let replays = new Dictionary<string, LobbyPlayerReplayInfo>()

    let chat_message_ev = new Event<string * string>()
    let system_message_ev = new Event<string>()
    let lobby_settings_updated_ev = new Event<LobbySettings>()
    let lobby_event_ev = new Event<LobbyEvent * string>()
    let lobby_players_updated_ev = new Event<unit>()
    let player_status_ev = new Event<string * LobbyPlayerStatus>()
    let change_chart_ev = new Event<LobbyChart>()
    let countdown_ev = new Event<string * int>()
    let game_start_ev = new Event<unit>()
    let game_end_ev = new Event<unit>()

    (* Events *)

    member this.OnChatMessage = chat_message_ev.Publish
    member this.OnSystemMessage = system_message_ev.Publish
    member this.OnSettingsUpdated = lobby_settings_updated_ev.Publish
    member this.OnLobbyEvent = lobby_event_ev.Publish
    member this.OnPlayersUpdated = lobby_players_updated_ev.Publish
    member this.OnPlayerStatusChanged = player_status_ev.Publish
    member this.OnChartChanged = change_chart_ev.Publish
    member this.OnCountdown = countdown_ev.Publish
    member this.OnGameStart = game_start_ev.Publish
    member this.OnGameEnd = game_end_ev.Publish

    (* To be called by network code (via UI thread) *)

    member this.PlayerJoined(username: string, color: int32) =
        this.Players.[username] <- LobbyPlayer.Create color
        lobby_players_updated_ev.Trigger()

    member this.PlayerLeft(username: string) =
        this.Players.Remove(username) |> ignore
        lobby_players_updated_ev.Trigger()

    member this.ChartSelected(lc: LobbyChart) =
        this.Chart <- Some lc

        for player in this.Players.Values do
            player.Status <- LobbyPlayerStatus.NotReady

        this.ReadyStatus <- ReadyFlag.NotReady
        change_chart_ev.Trigger lc
        lobby_players_updated_ev.Trigger()

    member this.UpdateSettings(settings: LobbySettings) =
        this.Settings <- settings
        lobby_settings_updated_ev.Trigger settings

    member this.LobbyEvent(kind: LobbyEvent, player: string) =
        if player = your_username || this.Players.ContainsKey player then
            lobby_event_ev.Trigger(kind, player)
        else
            Logging.Warn "Received event from untracked player %s" player

    member this.SystemMessage(msg: string) =
        system_message_ev.Trigger msg

    member this.ChatMessage(sender: string, msg: string) =
        if sender = your_username || this.Players.ContainsKey sender then
            chat_message_ev.Trigger(sender, msg)
        else
            Logging.Warn "Received chat message from untracked player %s" sender

    member this.PlayerStatus(username: string, status: LobbyPlayerStatus) =
        match this.Players.TryGetValue username with
        | true, player ->
            player.Status <- status
            lobby_players_updated_ev.Trigger()
            player_status_ev.Trigger(username, status)
        | false, _ -> Logging.Warn "Received status for untracked player %s" username

    member this.StartCountdown(reason: string, seconds: int) =
        countdown_ev.Trigger(reason, seconds)

    member this.GameStart() =
        replays.Clear()
        this.Countdown <- false
        this.GameInProgress <- true
        game_start_ev.Trigger()

    member this.GameEnd() =
        this.GameInProgress <- false
        for player in this.Players.Values do
            player.Status <- LobbyPlayerStatus.NotReady
        this.ReadyStatus <- ReadyFlag.NotReady
        game_end_ev.Trigger()

    (* To be called by UI thread *)

    member val Settings: LobbySettings = LobbySettings.Default with get, set
    member val Players: Dictionary<string, LobbyPlayer> = players with get
    member val YouAreHost: bool = false with get, set
    member val Spectate: bool = false with get, set
    member val ReadyStatus: ReadyFlag = ReadyFlag.NotReady with get, set
    member val Chart: LobbyChart option = None with get, set
    member val GameInProgress: bool = false with get, set
    member val Countdown: bool = false with get, set

    member this.SendMessage(msg: string) = client.Send(Upstream.CHAT msg)
    member this.InvitePlayer(username: string) = client.Send(Upstream.INVITE_TO_LOBBY username)
    member this.Leave () = client.Send(Upstream.LEAVE_LOBBY)

    member this.TransferHost(username: string) = client.Send(Upstream.TRANSFER_HOST username)
    member this.ChangeSettings (settings: LobbySettings) = client.Send(Upstream.LOBBY_SETTINGS settings)

    member this.ReportMissingChart() = client.Send(Upstream.MISSING_CHART)
    member this.SetReadyStatus(flag: ReadyFlag) =
        if not this.GameInProgress then
            this.ReadyStatus <- flag
            client.Send(Upstream.READY_STATUS flag)

    member this.StartRound() = client.Send(Upstream.START_GAME)
    member this.CancelRound() = client.Send(Upstream.CANCEL_GAME)
    member this.StartPlaying() = client.Send(Upstream.BEGIN_PLAYING)
    member this.StartSpectating() = client.Send(Upstream.BEGIN_SPECTATING)
    member this.SendReplayData (timestamp: float32, data: byte array) = client.Send(Upstream.PLAY_DATA (timestamp, data))
    member this.FinishPlaying() = client.Send(Upstream.FINISH_PLAYING false)
    member this.AbandonPlaying() = client.Send(Upstream.FINISH_PLAYING true)
    member this.SelectChart (chart_meta: ChartMeta, rate: Rate, mods: ModState) =
        if this.YouAreHost then
            client.Send(
                Upstream.SELECT_CHART
                    {
                        Hash = chart_meta.Hash
                        Artist = chart_meta.Artist
                        Title = chart_meta.Title
                        Creator = chart_meta.Creator
                        Rate = rate
                        Mods = Map.toArray mods
                    }
            )

    member this.GetReplayInfo(username: string) : LobbyPlayerReplayInfo option =
        match replays.TryGetValue username with
        | true, data -> Some data
        | false, _ -> None

    member this.AddReplayInfo(username: string, info: LobbyPlayerReplayInfo) =
        if replays.ContainsKey username then
            Logging.Warn "%s already has replay info" username
        replays.[username] <- info

    member this.Replays = replays