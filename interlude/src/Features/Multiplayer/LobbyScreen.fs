namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Web.Shared
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features.Play
open Interlude.Features.Online

type LobbySettingsPage(settings: LobbySettings) as this =
    inherit Page()

    let name = Setting.simple settings.Name
    let host_rotation = Setting.simple settings.HostRotation
    let auto_countdown = Setting.simple settings.AutomaticRoundCountdown
    
    do
        this.Content(
            column()
            |+ PrettySetting("lobby.name", TextEntry(name, "none")).Pos(200.0f)
            |+ PrettySetting("lobby.host_rotation", Selector<_>.FromBool(host_rotation)).Pos(300.0f)
            |+ PrettySetting("lobby.auto_countdown", Selector<_>.FromBool(auto_countdown)).Pos(400.0f)
        )
    
    override this.Title = N"lobby"
    override this.OnClose() = 
        Lobby.settings 
            { settings with 
                Name = name.Value
                HostRotation = host_rotation.Value
                AutomaticRoundCountdown = auto_countdown.Value
            }

type InvitePlayerPage() as this =
    inherit Page()
    
    let value = Setting.simple ""
    let submit() = Lobby.invite value.Value
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
        |+ Conditional(
            (fun () -> Network.lobby.IsSome && Network.lobby.Value.Settings.IsSome && Network.lobby.Value.YouAreHost),
            Button(Icons.options, fun () -> LobbySettingsPage(Network.lobby.Value.Settings.Value) |> Menu.ShowPage),
            Position = Position.SliceTop(90.0f).Margin(10.0f).SliceRight(70.0f)
        )
        |+ Text(
            (fun () -> lobby_title),
            Align = Alignment.CENTER,
            Position = { Position.SliceTop(90.0f).Margin(10.0f) with Right = 0.5f %- 0.0f })
        |+ PlayerList(Position = { Left = 0.0f %+ 150.0f; Right = 0.5f %- 150.0f; Top = 0.0f %+ 100.0f; Bottom = 1.0f %- 100.0f })
        |+ StylishButton(
            Lobby.leave,
            K (sprintf "%s %s" Icons.logout (L"lobby.leave")),
            Style.main 100,
            TiltLeft = false,
            Position = { Left = 0.0f %+ 0.0f; Top = 1.0f %- 50.0f; Right = (0.5f / 3f) %- 25.0f; Bottom = 1.0f %- 0.0f }
            )
        |+ StylishButton(
            (fun () -> Menu.ShowPage InvitePlayerPage),
            K (sprintf "%s %s" Icons.invite (L"lobby.send_invite")),
            Style.dark 100,
            Position = { Left = (0.5f / 3f) %+ 0.0f; Top = 1.0f %- 50.0f; Right = (1.0f / 3f) %- 25.0f; Bottom = 1.0f %- 0.0f }
            )
        |+ StylishButton(
            (fun () -> Network.lobby.Value.Ready <- SelectedChart.found && not Network.lobby.Value.Ready; Lobby.set_ready Network.lobby.Value.Ready),
            (fun () -> match Network.lobby with Some l -> (if l.Ready then (sprintf "%s %s" Icons.not_ready (L"lobby.not_ready")) else (sprintf "%s %s" Icons.ready (L"lobby.ready"))) | None -> "!"),
            Style.main 100,
            TiltRight = false,
            Position = { Left = (1.0f / 3f) %+ 0.0f; Top = 1.0f %- 50.0f; Right = 0.5f %- 0.0f; Bottom = 1.0f %- 0.0f }
            )
        |+ SelectedChart(Position = { Left = 0.5f %+ 20.0f; Top = 0.0f %+ 100.0f; Right = 1.0f %- 20.0f; Bottom = 0.5f %- 0.0f } )
        |* Chat(Position = { Position.Margin(20.0f) with Left = 0.5f %+ 20.0f; Top = 0.5f %+ 0.0f } )
        
        base.Init parent

        Network.Events.game_start.Add(
            fun () ->
                // todo: if you are ready to spectate, spectate instead
                if Screen.currentType = Screen.Type.Lobby && SelectedChart.found && Network.lobby.Value.Ready then 
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

    let list = 
        StaticContainer(NodeType.None)
        |+ LobbyList(Position = Position.Default.Margin(300.0f, 100.0f).TrimRight(600.0f))
        |+ InviteList(Position = Position.Default.Margin(300.0f, 100.0f).SliceRight(500.0f))
    let main = Lobby()

    let swap = SwapContainer(Current = list)

    override this.OnEnter(_) =
        in_lobby <- Network.lobby.IsSome
        swap.Current <- if in_lobby then main :> Widget else list
        if not in_lobby then Lobby.refresh_list()
        if in_lobby then SelectedChart.update Network.lobby.Value.Chart
    override this.OnExit(_) = ()

    override this.OnBack() = 
        if in_lobby then
            Menu.ShowPage <| ConfirmPage("Leave this lobby?", Lobby.leave)
            None
        else Some Screen.Type.MainMenu

    override this.Init(parent) =
        this |* swap
        
        base.Init parent
        Network.Events.join_lobby.Add (fun () -> in_lobby <- true; swap.Current <- main)
        Network.Events.leave_lobby.Add (fun () -> in_lobby <- false; swap.Current <- list)