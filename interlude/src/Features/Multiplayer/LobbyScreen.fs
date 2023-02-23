namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Gameplay.Mods
open Interlude.Web.Shared
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features.Gameplay
open Interlude.Features.Play
open Interlude.Features.Online

type Player(name: string, player: Network.LobbyPlayer) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect this.Bounds (Style.dark 100 ())
        Text.drawFillB(Style.baseFont, name, this.Bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.LEFT)
        Text.drawFillB(Style.baseFont, (if player.Status = LobbyPlayerStatus.Ready then Icons.ready else ""), this.Bounds.Shrink(5.0f, 0.0f), Style.text(), Alignment.RIGHT)

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

module SelectedChart =

    let mutable chart = None
    let mutable found = false
    let mutable difficulty = ""
    let mutable length = ""
    let mutable bpm = ""
    let mutable notecounts = ""

    let update(c: LobbyChart option) =
        
        chart <- c
        match c with
        | None ->
            found <- true
            difficulty <- ""
            length <- ""
            bpm <- ""
            notecounts <- ""
        | Some chart ->
            found <- 
                match Library.lookupHash chart.Hash with
                | Some cc -> 
                    match Library.load cc with 
                    | Some c -> Chart.change(cc, Collections.LibraryContext.None, c); rate.Set chart.Rate; true
                    | None -> false
                | None -> Logging.Info(sprintf "Chart not found locally: %s [%s]" chart.Title chart.Hash); false
            if found then
                difficulty <- sprintf "%s %.2f" Icons.star (match Chart.rating with None -> 0.0 | Some d -> d.Physical)
                length <- Chart.format_duration()
                bpm <- Chart.format_bpm()
                notecounts <- Chart.format_notecounts()
            else
                difficulty <- ""
                length <- ""
                bpm <- ""
                notecounts <- ""

type SelectedChart() =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent: Widget) =
        this
        |+ Text((fun () -> match SelectedChart.chart with Some c -> c.Title | None -> "No chart selected"), Align = Alignment.LEFT, Position = Position.SliceTop(40.0f).Margin(10.0f, 0.0f))
        |+ Text((fun () -> if SelectedChart.chart.IsSome && SelectedChart.found then Chart.cacheInfo.Value.Artist + "  •  " + Chart.cacheInfo.Value.Creator else ""), Color = Style.text_subheading, Align = Alignment.LEFT, Position = Position.TrimTop(40.0f).SliceTop(30.0f).Margin(10.0f, 0.0f))
        |+ Text((fun () -> if SelectedChart.chart.IsSome && SelectedChart.found then Chart.cacheInfo.Value.DiffName else ""), Color = Style.text_subheading, Align = Alignment.LEFT, Position = Position.TrimTop(70.0f).SliceTop(30.0f).Margin(10.0f, 0.0f))

        |+ Text((fun () -> SelectedChart.difficulty), Align = Alignment.LEFT, Position = Position.TrimTop(100.0f).SliceTop(60.0f))
        |+ Text((fun () -> SelectedChart.length), Align = Alignment.CENTER, Position = Position.TrimTop(100.0f).SliceTop(60.0f))
        |+ Text((fun () -> SelectedChart.bpm), Align = Alignment.RIGHT, Position = Position.TrimTop(100.0f).SliceTop(60.0f))
        |+ Text((fun () -> if SelectedChart.found then getModString(rate.Value, selectedMods.Value, autoplay) else ""), Align = Alignment.LEFT, Position = Position.TrimTop(160.0f).SliceTop(40.0f))
        |+ Text((fun () -> SelectedChart.notecounts), Align = Alignment.RIGHT, Position = Position.TrimTop(160.0f).SliceTop(40.0f))
        |+ Text((fun () -> if SelectedChart.found then "" else "You don't have this chart!"), Align = Alignment.CENTER, Position = Position.TrimTop(100.0f).SliceTop(60.0f))

        |+ IconButton("Change chart", Icons.reset, 50.0f, (fun () -> Screen.change Screen.Type.LevelSelect Transitions.Flags.Default), Position = Position.TrimTop(200.0f).SliceTop(50.0f).SliceLeft(300.0f))
        |* IconButton("Start game", Icons.play, 50.0f, (fun () -> Network.client.Send Upstream.START_GAME), Position = Position.TrimTop(200.0f).SliceTop(50.0f).TrimLeft(300.0f).SliceLeft(300.0f))

        SelectedChart.update Network.lobby.Value.Chart
        Network.Events.change_chart.Add(fun () -> if Screen.currentType = Screen.Type.Lobby then SelectedChart.update Network.lobby.Value.Chart)

        base.Init parent

    override this.Draw() =
        Draw.rect (this.Bounds.SliceTop(70.0f)) (if SelectedChart.found then Style.dark 180 () else Color.FromArgb(180, 100, 100, 100))
        Draw.rect (this.Bounds.SliceTop(100.0f).SliceBottom(30.0f)) (if SelectedChart.found then Style.darkD 180 () else Color.FromArgb(180, 50, 50, 50))
        Draw.rect (this.Bounds.SliceTop(100.0f).SliceLeft(5.0f)) (if SelectedChart.found then Style.main 255 () else Color.White)

        base.Draw()

type Chat() =
    inherit StaticContainer(NodeType.None)

    let MESSAGE_HEIGHT = 40.0f
    
    let current_message = Setting.simple ""

    let chat_msg(sender: string, message: string) =
        let w = Text.measure(Style.baseFont, sender) * 0.6f * MESSAGE_HEIGHT
        StaticContainer(NodeType.None)
        |+ Text(sender, Color = Style.text_subheading, Position = Position.SliceLeft w, Align = Alignment.RIGHT)
        |+ Text(": " + message, Position = Position.TrimLeft w, Align = Alignment.LEFT)

    let messages = FlowContainer.Vertical<Widget>(MESSAGE_HEIGHT, Spacing = 2.0f)
    let message_box = ScrollContainer.Flow(messages, Position = Position.TrimBottom(60.0f).Margin(5.0f))
    let chatline = TextEntry(current_message, "none", Position = Position.SliceBottom(50.0f).Margin(5.0f))

    let mutable last_msg : Widget option = None
    let add_msg(w: Widget) =
        messages.Add w
        match last_msg with
        | Some m ->
            if m.VisibleBounds.Visible then
                message_box.Scroll infinityf
        | None -> ()
        last_msg <- Some w

    override this.Init(parent) =
        this
        |+ chatline
        |+ Text((fun () -> if current_message.Value = "" then "Press ENTER to chat" else ""), Color = Style.text_subheading, Position = Position.SliceBottom(50.0f).Margin(5.0f), Align = Alignment.LEFT)
        |* message_box

        Network.Events.chat_message.Add (chat_msg >> add_msg)
        Network.Events.system_message.Add (fun msg -> add_msg (Text(msg, Align = Alignment.CENTER)))
        Network.Events.lobby_event.Add (fun (kind, data) ->
            let text, color =
                match (kind, data) with
                | LobbyEvent.Join, who -> sprintf "%s %s joined" Icons.login who, Color.Lime
                | LobbyEvent.Leave, who -> sprintf "%s %s left" Icons.logout who, Color.PaleVioletRed
                | LobbyEvent.Host, who -> sprintf "%s %s is now host" Icons.star who, Color.Gold
                | LobbyEvent.Ready, who -> sprintf "%s %s is ready" Icons.ready who, Color.PaleGreen
                | LobbyEvent.NotReady, who -> sprintf "%s %s is not ready" Icons.not_ready who, Color.DeepPink
                | LobbyEvent.Invite, who -> sprintf "%s %s invited" Icons.invite who, Color.PaleTurquoise
                | LobbyEvent.Generic, msg -> sprintf "%s %s" Icons.info msg, Color.WhiteSmoke
                | _, msg -> msg, Color.White
            add_msg (Text(text, Color = (fun () -> color, Color.Black), Align = Alignment.CENTER))
            )
        Network.Events.join_lobby.Add (fun () -> messages.Clear())

        base.Init parent

    override this.Draw() =
        Draw.rect(this.Bounds.TrimBottom 60.0f) (Color.FromArgb(180, 0, 0, 0))
        Draw.rect(this.Bounds.SliceBottom 50.0f) (Color.FromArgb(180, 0, 0, 0))
        base.Draw()

    override this.Update(elapsedTime, moved) =
        if (!|"select").Tapped() then
            if chatline.Selected && current_message.Value <> "" then 
                Network.send_chat_message current_message.Value
                current_message.Set ""
            else chatline.Select()

        base.Update(elapsedTime, moved)

type InvitePlayerPage() as this =
    inherit Page()
    

    let value = Setting.simple ""
    let submit() = Network.invite_to_lobby value.Value
    let submit_button = PrettyButton("confirm.yes", (fun () -> submit(); Menu.Back()), Enabled = false)
    
    do
        this.Content(
            column()
            |+ PrettySetting("invite_to_lobby.username", TextEntry(value |> Setting.trigger (fun s -> submit_button.Enabled <- s.Length > 0), "none")).Pos(200.0f)
            |+ submit_button.Pos(300.0f)
        )
    
    override this.Title = N"invite_to_lobby"
    override this.OnClose() = ()

type Lobby() =
    inherit StaticContainer(NodeType.None)

    let mutable lobby_title = "Loading..."

    override this.Init(parent) =
        this
        |+ Text(
            (fun () -> lobby_title),
            Align = Alignment.CENTER,
            Position = { Position.Default with Bottom = 0.0f %+ 80.0f; Top = 0.0f %+ 10.0f; Right = 0.5f %- 0.0f })
        |+ PlayerList(Position = { Left = 0.0f %+ 150.0f; Right = 0.5f %- 150.0f; Top = 0.0f %+ 100.0f; Bottom = 1.0f %- 100.0f })
        |+ StylishButton(
            Network.leave_lobby,
            K (Icons.logout + " Leave lobby"),
            Style.main 100,
            TiltLeft = false,
            Position = { Left = 0.0f %+ 0.0f; Top = 1.0f %- 50.0f; Right = (0.5f / 3f) %- 25.0f; Bottom = 1.0f %- 0.0f }
            )
        |+ StylishButton(
            (fun () -> Menu.ShowPage InvitePlayerPage),
            K (Icons.invite + " Invite player"),
            Style.dark 100,
            Position = { Left = (0.5f / 3f) %+ 0.0f; Top = 1.0f %- 50.0f; Right = (1.0f / 3f) %- 25.0f; Bottom = 1.0f %- 0.0f }
            )
        |+ StylishButton(
            (fun () -> Network.lobby.Value.Ready <- not Network.lobby.Value.Ready; Network.ready_status Network.lobby.Value.Ready),
            (fun () -> match Network.lobby with Some l -> (if l.Ready then (Icons.not_ready + " Not ready") else (Icons.ready + " Ready")) | None -> ""),
            Style.main 100,
            TiltRight = false,
            Position = { Left = (1.0f / 3f) %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.5f %- 0.0f; Bottom = 1.0f %- 0.0f }
            )
        |+ SelectedChart(Position = { Left = 0.5f %+ 20.0f; Top = 0.0f %+ 100.0f; Right = 1.0f %- 20.0f; Bottom = 0.5f %- 10.0f } )
        |* Chat(Position = { Position.Margin(20.0f) with Left = 0.5f %+ 20.0f; Top = 0.5f %+ 10.0f } )
        
        base.Init parent

        Network.Events.game_start.Add(
            fun () -> 
                if Screen.currentType = Screen.Type.Lobby then 
                    Screen.changeNew 
                        (fun () -> PlayScreen.multiplayer_screen())
                        Screen.Type.Play
                        Transitions.Flags.Default
            )
        Network.Events.lobby_settings_updated.Add(fun () -> lobby_title <- Network.lobby.Value.Settings.Value.Name)

// Screen

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
        if in_lobby then SelectedChart.update Network.lobby.Value.Chart
    override this.OnExit(_) = ()

    override this.OnBack() = 
        if in_lobby then
            Menu.ShowPage <| ConfirmPage("Leave this lobby?", Network.leave_lobby)
            None
        else Some Screen.Type.LevelSelect

    override this.Init(parent) =
        this |* swap
        
        base.Init parent
        Network.Events.join_lobby.Add (fun () -> in_lobby <- true; swap.Current <- main)
        Network.Events.leave_lobby.Add (fun () -> in_lobby <- false; swap.Current <- list)