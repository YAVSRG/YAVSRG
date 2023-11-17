namespace Interlude.Features.Multiplayer

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Prelude.Common
open Interlude.Web.Shared
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Options
open Interlude.Features.Play
open Interlude.Features.Online
open Interlude.Features.LevelSelect
open Interlude.Features

type LobbySettingsPage(settings: LobbySettings) as this =
    inherit Page()

    let name = Setting.simple settings.Name
    let host_rotation = Setting.simple settings.HostRotation
    let auto_countdown = Setting.simple settings.AutomaticRoundCountdown

    do
        this.Content(
            column ()
            |+ PageTextEntry("lobby.name", name).Pos(200.0f)
            |+ PageSetting("lobby.host_rotation", Selector<_>.FromBool(host_rotation))
                .Pos(300.0f)
                .Tooltip(Tooltip.Info("lobby.host_rotation"))
            |+ PageSetting("lobby.auto_countdown", Selector<_>.FromBool(auto_countdown))
                .Pos(370.0f)
                .Tooltip(Tooltip.Info("lobby.auto_countdown"))
        )

    override this.Title = %"lobby.name"

    override this.OnClose() =
        Lobby.settings
            {
                Name = name.Value
                HostRotation = host_rotation.Value
                AutomaticRoundCountdown = auto_countdown.Value
            }

type Lobby() =
    inherit StaticContainer(NodeType.None)

    let mutable lobby_title = "Loading..."

    override this.Init(parent) =
        this
        |+ Conditional(
            (fun () ->
                Network.lobby.IsSome
                && Network.lobby.Value.Settings.IsSome
                && Network.lobby.Value.YouAreHost
            ),
            Button(Icons.SETTINGS, (fun () -> LobbySettingsPage(Network.lobby.Value.Settings.Value).Show())),
            Position = Position.SliceTop(90.0f).Margin(10.0f).SliceRight(70.0f)
        )
        |+ Text(
            (fun () -> lobby_title),
            Align = Alignment.CENTER,
            Position =
                { Position.SliceTop(90.0f).Margin(10.0f) with
                    Right = 0.4f %- 0.0f
                }
        )
        |+ PlayerList(
            Position =
                {
                    Left = 0.0f %+ 50.0f
                    Right = 0.4f %- 50.0f
                    Top = 0.0f %+ 100.0f
                    Bottom = 1.0f %- 100.0f
                }
        )
        |+ StylishButton(
            (fun () ->
                if Network.lobby.IsSome && SelectedChart.loaded () then
                    Preview(Gameplay.Chart.WITH_MODS.Value, ignore).Show()
            ),
            K(sprintf "%s %s" Icons.EYE (%"levelselect.preview.name")),
            !%Palette.MAIN_100,
            TiltLeft = false,
            Hotkey = "preview",
            Position =
                { Position.SliceBottom(50.0f) with
                    Right = (0.4f / 3f) %- 25.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.preview"))
        |+ StylishButton(
            ignore,
            K(sprintf "%s %s" Icons.ZAP (%"levelselect.mods.name")),
            !%Palette.DARK_100,
            Hotkey = "mods",
            Position =
                { Position.SliceBottom(50.0f) with
                    Left = (0.4f / 3f) %- 0.0f
                    Right = (0.4f / 1.5f) %- 25.0f
                }
        )
            .Tooltip(Tooltip.Info("levelselect.mods"))
        |+ Rulesets
            .QuickSwitcher(
                options.SelectedRuleset,
                Position =
                    {
                        Left = (0.4f / 1.5f) %+ 0.0f
                        Top = 1.0f %- 50.0f
                        Right = 0.4f %- 0.0f
                        Bottom = 1.0f %- 0.0f
                    }
            )
            .Tooltip(
                Tooltip
                    .Info("levelselect.rulesets", "ruleset_switch")
                    .Hotkey(%"levelselect.rulesets.picker_hint", "ruleset_picker")
            )
        |+ SelectedChart(
            Position =
                {
                    Left = 0.5f %+ 20.0f
                    Top = 0.0f %+ 100.0f
                    Right = 1.0f %- 20.0f
                    Bottom = 0.5f %- 0.0f
                }
        )
        |* Chat(
            Position =
                { Position.Margin(20.0f) with
                    Left = 0.4f %+ 20.0f
                    Top = 0.5f %+ 0.0f
                }
        )

        base.Init parent

        Network.Events.game_start.Add(fun () ->
            if
                Screen.current_type = Screen.Type.Lobby
                && Network.lobby.Value.ReadyStatus = ReadyFlag.Play
            then
                if
                    Screen.change_new
                        (fun () -> PlayScreen.multiplayer_screen ())
                        Screen.Type.Play
                        Transitions.Flags.Default
                    |> not
                then
                    Logging.Warn("Missed the start of the lobby song because you were changing screen")

        )

        Network.Events.player_status.Add(fun (username, status) ->
            if
                status = LobbyPlayerStatus.Playing
                && Screen.current_type = Screen.Type.Lobby
                && Network.lobby.Value.ReadyStatus = ReadyFlag.Spectate
            then
                if
                    Screen.change_new
                        (fun () -> SpectateScreen.spectate_screen username)
                        Screen.Type.Replay
                        Transitions.Flags.Default
                    |> not
                then
                    Logging.Warn("Missed the start of spectating because you were changing screen")
        )

        Network.Events.lobby_settings_updated.Add(fun () -> lobby_title <- Network.lobby.Value.Settings.Value.Name)

// Screen

type LobbyScreen() =
    inherit Screen()

    let mutable in_lobby = false

    let list =
        StaticContainer(NodeType.None)
        |+ LobbyList(
            Position =
                { Position.Default with
                    Right = 0.7f %+ 0.0f
                }
                    .Margin(200.0f, 100.0f)
        )
        |+ InviteList(
            Position =
                { Position.Default with
                    Left = 0.7f %+ 0.0f
                }
                    .Margin(100.0f, 100.0f)
        )

    let main = Lobby()

    let swap = SwapContainer(Current = list)

    override this.OnEnter(_) =
        in_lobby <- Network.lobby.IsSome
        swap.Current <- if in_lobby then main :> Widget else list

        if not in_lobby then
            Lobby.refresh_list ()

        if in_lobby then
            SelectedChart.switch Network.lobby.Value.Chart

        Song.on_finish <- SongFinishAction.LoopFromPreview
        DiscordRPC.in_menus ("Multiplayer lobby")

    override this.OnExit(_) = ()

    override this.OnBack() =
        if in_lobby then
            ConfirmPage("Leave this lobby?", Lobby.leave).Show()
            None
        else
            Some Screen.Type.MainMenu

    override this.Init(parent) =
        this |* swap

        base.Init parent

        Network.Events.join_lobby.Add(fun () ->
            in_lobby <- true
            swap.Current <- main
        )

        Network.Events.leave_lobby.Add(fun () ->
            in_lobby <- false
            swap.Current <- list
        )
